;;; collect.el --- Read quickly from multiple databases -*- lexical-binding: t -*-

;; Copyright (C) 2018-2019  Free Software Foundation, Inc.

;; Author: Florian Perucki <florian@perucki.fr>
;; URL: https://github.com/florianperucki/collect
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.1") (ivy "0.10.0") (hydra 0.14.0))
;; Keywords: database mongodb sql

;; This file is part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This package lets you access data from your databases of potentially
;; multiple types (MongoDB, SQL, ElasticSearch).
;;
;; IMPORTANT: This package is thought for read-only operations, and no
;; CRUD operation (other than the Read part) is initially provided.
;;
;; The typical use-case for this package is when you have one or more
;; (production) datatables which you often need to query for quick
;; information gathering. For instance, if you need to see if you have
;; any open support ticket for a specific client, you could either go to
;; your company's Web interface (if there is even one for this purpose),
;; and click on multiple menus to get what you're looking for. Or you could
;; log-in directly to your database (with a read-only user, of course), get
;; your client's ID in the clients collection, and then run a query against
;; the tickets collection using
;; the client ID.
;;
;; With this package you can do the following:
;;
;; (collect-setup
;;  ;; configure how to access your database
;;  (collect-add-database "mydb"
;;                        :key "m"
;;                        :type 'mongodb
;;                        :host "127.0.0.1:27828/mydb"
;;                        :user "readonly"
;;                        :password "passw0rd")
;;  ;; configure the 'clients' collection for the 'mydb' database
;;  (collect-configure-collection
;;     "mydb"
;;     "clients"
;;     :key "s"
;;     ;; Query projection; columns to display with `ivy'.
;;     ;; Only these fields will be requested from the database.
;;     :columns '((:name "_id" :width 30)
;;                (:name "name" :width 50)
;;                (:name "email" :width 30)
;;                (:name "some.nested.field" :width 20))
;;     ;; Available actions on a single row
;;     :actions '((:name "Tickets"
;;                :key "t"
;;                :collection "tickets"
;;                ;; the '_id' field of the current row matches
;;                ;; the 'client' field in the 'tickets' collection
;;                :foreign "client"
;;                :query "status: \"open\""
;;                :sort "_id: -1"
;;                :limit 5))))
;;
;; After running the above setup, you may '<prefix> m s' to display
;; a list of your clients, use the `ivy' prompt to find the one you are
;; looking for, and then press 'M-o t' to see some open tickets for this
;; client (M-o being the `ivy' prefix to access commands). If you want to
;; see the full document of a specific ticket you can press <RET> or 'M-o o'
;; and it will be displayed in a new buffer.
;;
;; Check the documentation to see how to create more complex queries, like
;; cross-databases queries.
;;
;; If you don't want to go through the database->collection->etc. selection
;; process, you can define a key binding of your own like this:
;;
;; (global-set-key (kbd "M-m c s")
;;                 (lambda ()
;;                   (interactive)
;;                   (collect-display "mydb" :collection "tickets")))

;;; Code:

(defvar collect-selector 'ivy)
(defvar collect--debug nil)
;; (setq collect--debug t)

(defvar collect-collection-default-columns (list '(:name "_id" :width 30)))

(defvar collect-default-column-width 30)
(defvar collect--databases (list))

(require 'collect-ivy)
(require 'collect-hydra)
(require 'collect-mongodb)

;;;###autoload
(defun collect-setup (&rest body)
  (collect--build-hydras))

;;;###autoload
(cl-defun collect-add-database (database &key key type host user password)
  "Register a database in collect"
  ;; remove any existing entry for this database
  (when (assoc database collect--databases)
    (setq collect--databases
          (delq (assoc database collect--databases) collect--databases)))

  ;; add entry for this database
  (setq collect--databases
        (append collect--databases
                (list (cons database
                            (list
                             :key key
                             :type type
                             :host host
                             :user user
                             :password password))))))

;;;###autoload
(cl-defun collect-configure-collection (database collection &key key columns actions sort limit queries)
  "Register a collection to an existing database"
  (let* ((database-object (collect--get-database database))
         (value (cons collection (list
                                  :key key
                                  :name collection
                                  :columns columns
                                  :actions (collect--configure-actions database collection actions)
                                  :sort sort
                                  :limit limit
                                  :queries queries))))
    (collect--set-property :collections value database-object t)))

;;;###autoload
(cl-defun collect-display (database &key collection skip query limit sort foreign-key document-id single)
  "Build a query, run it and display its output, either a list of rows or a single document if documentp is non-nil."
  (if single
      ;; show a single document
      (let ((show-document (collect--get-database-function "show-document" database)))
        (funcall show-document database collection document-id foreign-key))
    ;; display rows
    (let* ((query (collect--compose-query database
                                          :query query
                                          :foreign-key foreign-key
                                          :document-id document-id))
           (data (collect--build-and-run-select-query database collection skip query limit sort))
           (entries (mapcar (apply-partially 'collect--format-entry-document database collection) data)))
      (collect--display :database database :collection collection :entries entries))))

;;
;; public - commands
;;

(defun collect-show-databases ()
  "Lists your defined databases"
  (interactive)
  (let ((display-function (intern (format "collect--%S-databases" collect-selector))))
    (funcall display-function collect--databases)))

(defun collect-show-collections (database &optional defined)
  (interactive "sDatabase: ")
  (let* ((collections (collect--get-collection-names database defined))
         (display-function (intern (format "collect--%S-collections" collect-selector))))
    (funcall display-function database collections)))

;; actions

;;;###autoload
(defun collect--configure-actions (database collection actions)
  "Ensure all actions have a database and a collection field. It could already have one if the action
is targetting a different database/collection than the current one."
  (mapcar #'(lambda (action)
              (progn (when (not (plist-get action :database))
                       (plist-put action :database database))
                     (when (not (plist-get action :collection))
                       (plist-put action :collection collection))
                     action))
          actions))

(defun collect--run-action (action entry)
  "Execute an ACTION on an document ENTRY.

The executed action depends on the ACTION type property:

read: execute a read operation"
  (let* ((action-type (or (plist-get action :type) 'read))
         ;; target database for action, defaults to current
         (database (or
                    (plist-get action :database)
                    (get-text-property 0 :database entry)))
         ;; target collection for action, defaults to current
         (collection (or
                      (plist-get action :collection)
                      (get-text-property 0 :collection entry)))
         ;; document unique id
         (document-id (get-text-property 0 :id entry))
         ;; if provided, this is the field name to use instead of the
         ;; database default unique id field
         (foreign-key (plist-get action :foreign))
         ;; if t, the query returns a single document that we'll show in a new buffer
         (single (plist-get action :single))
         ;; usual query parameters
         (sort (plist-get action :sort))
         (limit (plist-get action :limit))
         (skip (plist-get action :skip))
         (projection (plist-get action :projection))
         (query (plist-get action :query)))
    (if (equal action-type 'read)
        (collect-display database
                         :collection collection
                         :skip skip
                         :query query
                         :limit limit
                         :sort sort
                         :foreign-key foreign-key
                         :document-id document-id
                         :single single)
      (error "Unknown action type %S" action-type))))

(cl-defun collect--compose-query (database &key query document-id foreign-key)
  "Build a single query from multiple arguments"
  (let ((compose-query (collect--get-database-function "compose-query" database)))
    (funcall compose-query
             :document-id document-id
             :query query
             :foreign-key foreign-key)))

(defun collect--action-copy-id (row)
  "Action to execute on any row: add the row ID to the kill ring"
  (kill-new (get-text-property 0 :id row)))

;; getting customizable option value

(defun collect--get-collection-columns (database collection)
  "Return a list of columns for the specified collection."
  (let ((columns (collect--get-collection-property :columns database collection)))
    ;; TODO: fetch default columns with database type file
    (or columns collect-collection-default-columns)))

(defun collect--get-collection-limit (database collection)
  "Return the correct result limit for this collection"
  (or (collect--get-collection-property :limit database collection)
      (collect--get-database-property :limit database)
      (collect--get-database-var "default-limit" database)))

(defun collect--get-collection-sort (database collection)
  "Return the correct sorting for this collection"
  (or (collect--get-collection-property :sort database collection)
      (collect--get-database-property :sort database)
      (collect--get-database-var "default-sort" database)))

(defun collect--get-collection-names (database &optional defined)
  "Return a list of collection names.
If defined is non-nil, only return the user-defined collections.
If defined is t, fetch the collection names remotely."
  (if defined (mapcar (apply-partially 'collect--get-property :name)
                      (collect--get-database-property :collections database))
    (collect--get-collection-names-remote database)))

(defun collect--get-column-width (database collection column)
  "Return the given column's width according to user preference.
database: string name of the database
collection: string name of the collection
column: plist"
  (or (plist-get column :width) collect-default-column-width))

;; database query

(defun collect--get-database-var (var-name database)
  "Return variable var-name of DATABASE.
DATABASE: string name of a configured database."
  (let ((database-type (collect--get-database-property :type database)))
    (symbol-value (intern (format "collect-%S-%s" database-type var-name)))))

(defun collect--get-database-function (function-name database)
  "Return function function-name, prefixed with \"collect--\" for a database type.
DATABASE: string name of a configured database"
  (let ((database-type (collect--get-database-property :type database)))
    (intern (format "collect--%S-%s" database-type function-name))))

(defun collect--build-and-run-select-query (database collection &optional skip query limit sort)
  "Run query on collection and return extracted results"
  (let* ((query (collect--build-select-query database collection skip query limit sort))
         (data (collect--run-select-query database query)))
    (collect--extract-data-documents database collection data)))

(defun collect--build-select-query (database collection &optional skip query limit sort)
  "Return a SELECT (or equivalent) query (string)."
  (let* ((sort (or sort (collect--get-collection-sort database collection)))
         (limit (or limit (collect--get-collection-limit database collection)))
         (get-projection (collect--get-database-function "get-collection-projection" database))
         (projection (funcall get-projection database collection))
         (build-query (collect--get-database-function "build-select-query" database)))
    (funcall build-query collection projection skip sort limit query)))

(defun collect--run-select-query (database query)
  "Run a query against the given database."
  (let* ((run-query (collect--get-database-function "run-select-query" database)))
    (funcall run-query database query)))

(defun collect--get-collection-names-remote (database)
  "Fetch all the existing collection names remotely."
  (let* ((display-function (collect--get-database-function "get-collection-name" database))
    (funcall display-function database))))

(defun collect--extract-data-documents (database collection data)
  "Convert data that was fetched from a collection to an internal comprehensive format."
  (let ((extract (collect--get-database-function "extract-data-documents" database)))
    (funcall extract database collection data)))

;; accessing front methods

(defun collect--get-front-function (function-name)
  (intern (format "collect--%S-%s" collect-selector function-name)))

(defun collect--get-actions (&optional database collection)
  "Return actions for the document type currently being displayed
collections if collection is nil, documents if collection is non-nil"
  (let ((get-actions (collect--get-front-function "get-actions")))
    (funcall get-actions database collection)))

(defun collect--get-prompt (database &optional collection)
  "Get formatted prompt for database and collection if provided"
  (let ((get-prompt (collect--get-front-function "get-prompt")))
    (funcall get-prompt database collection)))

(defun collect--display-prompt (prompt entries actions)
  "Run front-end command to display results"
  (let ((display (collect--get-front-function "display")))
    (funcall display prompt entries actions)))

;; helpers

(cl-defun collect--display (&key database collection entries actions)
  "Generic method to display prompt and results using the configured selector (e.g. ivy)"
  (let* ((custom-actions (collect--get-actions database collection))
         (actions (append (or actions (list)) custom-actions))
         (prompt (collect--get-prompt database collection)))
    (collect--display-prompt prompt entries (cons 1 actions))))

(defun collect--set-property (property value item &optional tolist)
  "Set an item property to the given value.
If the property is already set, override it, except when tolist is non-nil.
If tolist is non-nil, the property value is expected to be a list, and if the
property is already set the new value is appended."
  (let ((current (plist-get item property)))
    (if (not current)
        (if tolist
            (plist-put item property (list value))
          (plist-put item property value))
      (if tolist
          (plist-put item property (append current (list value)))
        (plist-put item property value)))))

(defun collect--get-property (property item)
  "Return the value of a property in a property list whose first element should be skipped."
  (plist-get (cdr item) property))

(defun collect--get-database (database)
  "Return the database structure associated to the given database name."
  (cdr (assoc database collect--databases)))

(defun collect--get-collection (database collection)
  "Return the collection structure associated to the given collection name."
  (let ((collections (collect--get-database-property :collections database)))
    (cdr (assoc collection collections))))

(defun collect--get-database-property (property database)
  "Return the value for the given database's property."
  (plist-get (collect--get-database database) property))

(defun collect--get-collection-property (property database collection)
  "Return the value for the given collection's property."
  (plist-get (collect--get-collection database collection) property))

(defun collect--requote-output (database output)
  (let ((requote (collect--get-database-function "requote-output" database)))
    (funcall requote output)))

(defun collect--show-document (row &optional projection)
  "Action to execute on any row: add the row ID to the kill ring"
  (let* ((database (get-text-property 0 :database row))
         (collection (get-text-property 0 :collection row))
         (document-id (get-text-property 0 :id row))
         (show-document (collect--get-database-function "show-document" database)))
    (funcall show-document database collection document-id)))

(defun collect--format-entry-document (database collection row)
  "Format a data ROW so it can be displayed in the selected front tool (ivy)"
  (let ((format-function (collect--get-front-function "format-entry-document")))
    (funcall format-function database collection row)))

(provide 'collect)
;;; collect.el ends here
