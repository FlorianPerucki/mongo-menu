;; -*- lexical-binding: t -*-

;; TODO https://emacs.stackexchange.com/questions/17130/data-structure-for-triplet-3-tuple

(defvar collect-selector 'ivy)
(defvar collect--debug nil)
;; (setq collect--debug t)

(defvar collect-collection-default-columns (list '(:name "_id" :width 30)))

(defvar collect-default-column-width 30)
(defvar collect--databases (list))

(require 'collect-ivy)
(require 'collect-mongodb)

;;
;; public - setup
;;

;;;###autoload
(defun collect/setup (&rest body)
  (collect--build-hydras))

;;;###autoload
(cl-defun collect/add-database (database &key key type host user password)
  "Register a database in collect"
  (interactive)

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
(cl-defun collect/configure-collection (database collection &key key columns actions sort limit queries)
  "Register a collection to an existing database"
  (interactive)
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
(cl-defun collect/display (database &key collection skip query limit sort foreign-key document-id single)
  "Build a query, run it and display its output, either a list of rows or a single document if documentp is non-nil."
  (interactive)
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
;; (defun collect--configure-queries (queries)
;;   "Format a list of queries customized with collect/configure-collection, for internal use"
;;   (mapcar (lambda (query)
;;             (let ((key (plist-get query :key)))
;;               (cons key query))) queries))
;;
;; public - commands
;;

(defun collect--format-entry-document (database collection row)
  "Format a data ROW so it can be displayed in the selected front tool (ivy)"
  (let ((format-function (intern (format "collect--format-entry-document-%S" collect-selector))))
    (funcall format-function database collection row)))

(defun collect-show-databases ()
  "Lists your defined databases"
  (interactive)
  (let ((display-function (intern (format "collect--databases-%S" collect-selector))))
    (funcall display-function collect--databases)))

(defun collect-show-collections (database &optional defined)
  (interactive "sDatabase: ")
  (let* ((collections (collect--get-collection-names database defined))
         (display-function (intern (format "collect--collections-%S" collect-selector))))
    (funcall display-function database collections)))


;; actions

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
        (collect/display database
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
    (or columns collect-collection-default-columns)))

(defun collect--get-collection-sort (database collection)
  "Get and format the sorting preference for this database's collection"
  (let* ((sort (collect--get-collection-property :sort database collection))
         (database-type (collect--get-database-property :type database))
         (default-sort (symbol-value (intern (format "collect--collection-default-sort-%S" database-type)))))
    (or sort default-sort)))

(defun collect--get-collection-limit (database collection)
  "This will get and format the maximum documents to fetch for this database's collection"
  (let* ((limit (collect--get-collection-property :limit database collection))
         (database-type (collect--get-database-property :type database))
         (default-limit (symbol-value (intern (format "collect-collection-default-limit-%S" database-type)))))
    (or limit default-limit)))

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

(defun collect--build-and-run-select-query (database collection &optional skip query limit sort)
  "Run query on collection and return extracted results"
  ;; TODO make it a database-specific method
  (let* ((query (collect--build-select-query database collection skip query limit sort))
         (data (collect--run-select-query database query)))
    (collect--extract-data-documents database collection data)))

(defun collect--build-select-query (database collection &optional skip query limit sort)
  "Return a SELECT (or equivalent) query (string)."
  (let* ((database-type (collect--get-database-property :type database))
         (sort (or sort (collect--get-collection-sort database collection)))
         (limit (or limit (collect--get-collection-limit database collection)))
         (get-projection (intern (format "collect--get-collection-projection-%s" database-type)))
         (projection (funcall get-projection database collection))
         (build-query (intern (format "collect--build-select-query-%S" database-type))))
    (funcall build-query collection projection skip sort limit query)))

(defun collect--run-select-query (database query)
  "Run a query against the given database."
  (let* ((database-type (collect--get-database-property :type database))
         (run-query (intern (format "collect--run-select-query-%S" database-type))))
    (funcall run-query database query)))

(defun collect--get-collection-names-remote (database)
  "Fetch all the existing collection names remotely."
  (let* ((type (collect--get-database-property :type "pricingassistant2"))
         (display-function (intern (format "collect--get-collection-names-%S" type))))
    (funcall display-function database)))

(defun collect--extract-data-documents (database collection data)
  "Convert data that was fetched from a collection to an internal comprehensive format."
  (let ((extract (collect--get-database-function "extract-data-documents" database)))
    (funcall extract database collection data)))

(defun collect--get-database-function (function-name database)
  "Return internal function function-name, prefixed with \"collect--\" for a database type.
If database is a symbol, it is considered as the database type, e.g. 'mongodb
Otherwise it must be a string matching a configured database name."
  (let ((database-type (if (symbolp database)
                           database
                         (collect--get-database-property :type database))))
    (intern (format "collect--%s-%S" function-name database-type))))

;; accessing front methods

(defun collect--get-actions (&optional database collection)
  "Return actions for the document type currently being displayed
collections if collection is nil, documents if collection is non-nil"
  (let ((get-actions (intern (format "collect--get-actions-%S" collect-selector))))
    (funcall get-actions database collection)))

(defun collect--get-prompt (database &optional collection)
  "Get formatted prompt for database and collection if provided"
  (let ((get-prompt (intern (format "collect--get-prompt-%S" collect-selector))))
    (funcall get-prompt database collection)))

(defun collect--display-prompt (prompt entries actions)
  "Run front-end command to display results"
  (let ((display (intern (format "collect--display-%S" collect-selector))))
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

(provide 'collect)
