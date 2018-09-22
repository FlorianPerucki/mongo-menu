;;; collect.el --- Read quickly from multiple databases -*- lexical-binding: t -*-

;; Copyright (C) 2018 Florian Perucki

;; Author: Florian Perucki <florian@perucki.fr>
;; URL: https://github.com/florianperucki/collect
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.1") (ivy "0.10.0") (hydra "0.14.0"))
;; Keywords: database mongodb sql

;; This file is not part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This package lets you access data from your databases using pre-
;; configured queries.

;; IMPORTANT: This package is thought for read-only operations, and no
;; CRUD operation (other than the Read part) is initially provided.

;; Currently supported databases are:
;;
;; - MongoDB

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
(require 'collect-table)

;;;###autoload
(cl-defmacro collect-setup (&body body)
  "Execute BODY to register databases and collections. Hydras for these databases
will then be generated.

BODY should consist of a sequence of `collect-add-database'
and `collect-add-collection' calls."
  `(progn
     (setq collect--databases (list))
     ,@body
     (collect--build-hydras)))

;;;###autoload
(cl-defun collect-add-database (&key name key type host user password hide display)
  "Register a database in collect

NAME: name of the database
KEY: corresponding key in the databases hydra
TYPE: database type, e.g. 'mongodb
HOST: remote database path
USER: database user
PASSWORD: database password
HIDE: register database but don't propose it in databases hydra"
  ;; remove any existing entry for this database
  (when (assoc name collect--databases)
    (setq collect--databases
          (delq (assoc name collect--databases) collect--databases)))

  ;; add entry for this database
  (setq collect--databases
        (append collect--databases
                (list (cons name
                            (list
                             :name name
                             :key key
                             :type type
                             :host host
                             :user user
                             :password password
                             :display display
                             :hide hide))))))

;;;###autoload
(cl-defun collect-add-collection (&key database name key columns actions sort limit heads display key-not-oid)
  "Register a collection to an existing database.

DATABASE: name of the database
NAME: name of the collection
KEY: corresponding key in the collections hydra
columns: columns to display; also used for query projection
SORT: query sort
LIMIT: query limit
HEADS: pre-defined queries that will be accessible in the collection's hydra
DISPLAY: how to display result rows. By default it will use `collect-selector'.
Can also be set to 'table, in which case the rows will be displayed in a separate
KEY-NOT-OID: For MongoDB databases only. If non nil, the raw _id field value will be used
instead of putting it inside an ObjectId().
`tabulated-list-mode' buffer"
  (let* ((database-object (collect--get-database database))
         (value (cons name (list
                            :key key
                            :name name
                            :columns columns
                            :actions (collect--configure-actions database name actions)
                            :sort sort
                            :limit limit
                            :display display
                            :key-not-oid key-not-oid
                            :heads (collect--configure-heads database name heads)))))
    (collect--set-property :collections value database-object t)))

;;;###autoload
(defun collect-collection-head (database collection key)
  "Run the query associated with KEY for the COLLECTION on DATABASE."
  (let* ((head (assoc key (collect--get-collection-property :heads database collection))))
    (collect--run-query (cdr head))))

;;;###autoload
(cl-defun collect-display (&key database collection skip query limit sort foreign-key document-id single projection)
  "Build a query, run it and display its output, either a list of rows or a single document if documentp is non-nil."
  (if single
      ;; show a single document
      (collect--show-document database collection document-id :field foreign-key :projection projection)
    ;; display rows
    (let* ((query (collect--compose-query database
                                          collection
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
  (let ((display-function (collect--get-front-function "databases")))
    (funcall display-function collect--databases)))

(defun collect-show-collections (database &optional defined)
  (interactive "sDatabase: ")
  (let* ((collections (collect--get-collection-names database defined))
         (display-function (collect--get-front-function "collections" database)))
    (funcall display-function database collections)))

(cl-defun collect--show-document (database collection document-id &key field projection)
  (let ((show-document (collect--get-database-function "show-document" database)))
    (funcall show-document database collection document-id :field field :projection projection)))

(defun collect--show-documents (database collection &optional skip query limit sort)
  "Fetch and display collection's data, filtered by QUERY, SKIP, LIMIT and/or SORT if provided"
  (let* ((documents (collect--build-and-run-select-query database collection skip query limit sort))
         (entries (mapcar (apply-partially 'collect--format-entry-document database collection) documents)))
    (collect--display :database database
                      :collection collection
                      :entries entries)))

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

;;;###autoload
(defun collect--configure-heads (database collection heads)
  "Create an alist where the key is the head key. Also ensure database and collection
are populated."
  (mapcar #'(lambda (head)
              (cons
               (plist-get head :key)
               (progn (when (not (plist-get head :database))
                        (plist-put head :database database))
                      (when (not (plist-get head :collection))
                        (plist-put head :collection collection))
                      head)))
          heads))

(defun collect--run-query (query)
  "Execute a user-defined QUERY.

The executed action depends on the QUERY type property:

read: execute a read operation
anything else: currently not implemented"
  (let* ((query-type (or (plist-get query :type) 'read))
         ;; target database for query, defaults to current
         (database (plist-get query :database))
         ;; target collection for query, defaults to current
         (collection (plist-get query :collection))
         ;; usual query parameters
         (sort (plist-get query :sort))
         (limit (plist-get query :limit))
         (skip (plist-get query :skip))
         (projection (plist-get query :projection))
         (query (plist-get query :query)))
    (if (equal query-type 'read)
        (collect-display :database database
                         :collection collection
                         :projection projection
                         :skip skip
                         :query query
                         :limit limit
                         :sort sort)
      (error "Unknown query type %S" query-type))))

(cl-defun collect--compose-query (database collection &key query document-id foreign-key)
  "Build a single query from multiple arguments"
  (let ((compose-query (collect--get-database-function "compose-query" database)))
    (funcall compose-query
             database
             collection
             :document-id document-id
             :query query
             :foreign-key foreign-key)))

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

(defun collect--get-column-name (database collection column)
  "Return the given column's display name, which could be the
:name property (the remote name) or an :alias."
  (or (plist-get column :alias) (plist-get column :name)))

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

(defun collect--get-front-function (function-name &optional database collection)
  "Decide which front selector to use and return the requested function"
  (let ((selector (cond
                    (collection
                     (or
                      (collect--get-collection-property :display database collection)
                      (collect--get-database-property :display database)
                      collect-selector))
                    (database
                     (or
                      (collect--get-database-property :display database)
                      collect-selector))
                    (t
                     collect-selector))))
    (intern (format "collect--%S-%s" selector function-name))))

;; actions

(defun collect--get-actions (&optional database collection)
  "Return actions for the row type currently being displayed
collections if collection is nil, documents if collection is non-nil"
  (if collection
      ;; actions on a document row
      (let ((default-actions (list
                              `("RET" ,(collect--get-front-function "action-show-document" database collection) "Open in buffer")
                              `("y" ,(collect--get-front-function "action-copy-id" database collection) "Copy row ID")))
            (actions (mapcar `(lambda (action) (interactive) (collect--build-action ,database ,collection action))
                             (collect--get-collection-property :actions database collection))))
        (append default-actions (or actions (list))))

    ;; actions on a collection row
    (if database
        (let ((default-actions (list
                                `("RET" ,(collect--get-front-function "action-show-documents" database) "Show documents")))
              (actions (mapcar
                        `(lambda (action) (interactive) (collect--build-action ,database ,collection action))
                        (collect--get-database-property :actions database))))
          (append default-actions (or actions (list))))

      ;; actions on a database row
      (list))))

(defun collect--build-action (database collection action)
  (let* ((key (plist-get action :key))
         (name (plist-get action :name)))
    (list key `(lambda (&optional entry) (interactive) (collect--run-action ,database ,collection ',action entry)) name)))

(defun collect--get-current-entry (database collection &optional entry)
  (let ((get-current-entry (collect--get-front-function "get-current-entry" database collection)))
    (funcall get-current-entry entry)))

(defun collect--run-action (database collection action &optional entry)
  "Execute an ACTION on a single document ENTRY.
The executed action depends on the ACTION type property:

read: execute a read operation
anything else: currently not implemented"
  (let* ((entry (collect--get-current-entry database collection entry))
         (action-type (or (plist-get action :type) 'read))
         ;; target database for action, defaults to current
         (database (or
                    (plist-get action :database)
                    (plist-get entry :database)))
         ;; target collection for action, defaults to current
         (collection (or
                      (plist-get action :collection)
                      (plist-get entry :collection)))
         ;; document unique id
         (document-id (plist-get entry :document-id))
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
        (collect-display :database database
                         :collection collection
                         :projection projection
                         :skip skip
                         :query query
                         :limit limit
                         :sort sort
                         :foreign-key foreign-key
                         :document-id document-id
                         :single single)
      (error "Unknown action type %S" action-type))))

 ; TODO limit + sort

;; helpers

(cl-defun collect--display (&key database collection entries actions)
  "Generic method to display prompt and results using the configured selector (e.g. ivy)"
  (let ((display-function (collect--get-front-function "display" database collection)))
    (funcall display-function database collection entries actions)))

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

(defun collect--format-entry-document (database collection row)
  "Format a data ROW so it can be displayed in the selected front tool (ivy)"
  (let ((format-function (collect--get-front-function "format-entry-document" database collection)))
    (funcall format-function database collection row)))

(provide 'collect)
;;; collect.el ends here
