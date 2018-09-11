;; -*- lexical-binding: t -*-

(defvar mongo-menu-selector 'ivy)
(defvar mongo-menu--debug nil)

(defvar mongo-menu-collection-default-columns (list '(:name "_id" :width 30)))

(defvar mongo-menu-default-column-width 30)
(defvar mongo-menu--databases (list))

(require 'mongo-menu-ivy)
(require 'mongo-menu-mongodb)

;;
;; public - setup
;;

;;;###autoload
(cl-defun mongo-menu/add-database (database &key key type host user password)
  "Register a database in mongo-menu"
  (interactive)

  ;; remove any existing entry for this database
  (when (assoc database mongo-menu--databases)
    (setq mongo-menu--databases
          (delq (assoc database mongo-menu--databases) mongo-menu--databases)))

  ;; add entry for this database
  (setq mongo-menu--databases
        (append mongo-menu--databases
                (list (cons database
                            (list
                             :key key
                             :type type
                             :host host
                             :user user
                             :password password))))))


;;;###autoload
(cl-defun mongo-menu/configure-collection (database collection &key key columns actions sort limit queries)
  "Register a collection to an existing database"
  (interactive)
  (let* ((database (mongo-menu--get-database database))
         (value (cons collection (list
                                  :key key
                                  :name collection
                                  :columns columns
                                  :actions actions
                                  :sort sort
                                  :limit limit
                                  :queries (mongo-menu--configure-queries queries)))))
    (mongo-menu--set-property :collections value database t)))

(defun mongo-menu--configure-queries (queries)
  "Format a list of queries customized with mongo-menu/configure-collection, for internal use"
  (mapcar (lambda (query)
            (let ((key (plist-get query :key)))
              (cons key query))) queries))
;;
;; public - commands
;;

(defun mongo-menu-show-databases ()
  "Lists your defined databases"
  (interactive)
  (let ((display-function (intern (format "mongo-menu--databases-%S" mongo-menu-selector))))
    (funcall display-function mongo-menu--databases)))

(defun mongo-menu-show-collections (database &optional defined)
  (interactive "sDatabase: ")
  (let* ((collections (mongo-menu--get-collection-names database defined))
         (display-function (intern (format "mongo-menu--collections-%S" mongo-menu-selector))))
    (funcall display-function database collections)))


;; Predefined actions

(defun mongo-menu--action-copy-id (row)
  "Action to execute on any row: add the row ID to the kill ring"
  (kill-new (get-text-property 0 :id row)))


;; getting customizable option value

(defun mongo-menu--get-collection-columns (database collection)
  "Return a list of columns for the specified collection."
  (let ((columns (mongo-menu--get-collection-property :columns database collection)))
    (or columns mongo-menu-collection-default-columns)))

(defun mongo-menu--get-collection-sort (database collection)
  "Get and format the sorting preference for this database's collection"
  (let* ((sort (mongo-menu--get-collection-property :sort database collection))
         (database-type (mongo-menu--get-database-property :type database))
         (default-sort (symbol-value (intern (format "mongo-menu--collection-default-sort-%S" database-type)))))
    (or sort default-sort)))

(defun mongo-menu--get-collection-limit (database collection)
  "This will get and format the maximum documents to fetch for this database's collection"
  (let* ((limit (mongo-menu--get-collection-property :limit database collection))
         (database-type (mongo-menu--get-database-property :type database))
         (default-limit (symbol-value (intern (format "mongo-menu-collection-default-limit-%S" database-type)))))
    (or limit default-limit)))

(defun mongo-menu--get-collection-names (database &optional defined)
  "Return a list of collection names.
If defined is non-nil, only return the user-defined collections.
If defined is t, fetch the collection names remotely."
  (if defined (mapcar (apply-partially 'mongo-menu--get-property :name)
                      (mongo-menu--get-database-property :collections database))
    (mongo-menu--get-collection-names-remote database)))

(defun mongo-menu--get-column-width (database collection column)
  "Return the given column's width according to user preference.
database: string name of the database
collection: string name of the collection
column: plist"
  (or (plist-get column :width) mongo-menu-default-column-width))

;; database query

(defun mongo-menu--build-and-run-select-query (database collection &optional skip query limit sort)
  "Run query on collection and return extracted results"
  (let* ((query (mongo-menu--build-select-query database collection skip query limit sort))
         (data (mongo-menu--run-select-query database query)))
    (mongo-menu--extract-data-documents database collection data)))

(defun mongo-menu--build-select-query (database collection &optional skip query limit sort)
  "Return a SELECT (or equivalent) query (string)."
  (let* ((database-type (mongo-menu--get-database-property :type database))
         (sort (or sort (mongo-menu--get-collection-sort database collection)))
         (limit (or limit (mongo-menu--get-collection-limit database collection)))
         (get-projection (intern (format "mongo-menu--get-collection-projection-%s" database-type)))
         (projection (funcall get-projection database collection))
         (build-query (intern (format "mongo-menu--build-select-query-%S" database-type))))
    (funcall build-query collection projection skip sort limit query)))

(defun mongo-menu--run-select-query (database query)
  "Run a query against the given database."
  (let* ((database-type (mongo-menu--get-database-property :type database))
         (run-query (intern (format "mongo-menu--run-select-query-%S" database-type))))
    (funcall run-query database query)))

(defun mongo-menu--get-collection-names-remote (database)
  "Fetch all the existing collection names remotely."
  (let* ((type (mongo-menu--get-database-property :type "pricingassistant2"))
         (display-function (intern (format "mongo-menu--get-collection-names-%S" type))))
    (funcall display-function database)))

(defun mongo-menu--extract-data-documents (database collection data)
  "Convert data that was fetched from a collection to an internal comprehensive format."
  (let ((extract (mongo-menu--get-database-function "extract-data-documents" database)))
    (funcall extract database collection data)))

(defun mongo-menu--get-database-function (function-name database)
  "Return internal function function-name, prefixed with \"mongo-menu--\" for a database type.
If database is a symbol, it is considered as the database type, e.g. 'mongodb
Otherwise it must be a string matching a configured database name."
  (let ((database-type (if (symbolp database)
                           database
                         (mongo-menu--get-database-property :type database))))
    (intern (format "mongo-menu--%s-%S" function-name database-type))))

;; helpers

(defun mongo-menu--format-entry-document (database collection document)
  "Format a document row to make it compatible with the selector (e.g. ivy)"
  (let ((formatter (intern (format "mongo-menu--format-entry-document-%S" mongo-menu-selector))))
    (funcall formatter database collection document)))

(defun mongo-menu--display (prompt entries actions)
  "Run front-end command to display results"
  (let ((display (intern (format "mongo-menu--display-%S" mongo-menu-selector))))
    (funcall display prompt entries actions)))

(defun mongo-menu--set-property (property value item &optional tolist)
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

(defun mongo-menu--get-property (property item)
  "Return the value of a property in a property list whose first element should be skipped."
  (plist-get (cdr item) property))

(defun mongo-menu--get-database (database)
  "Return the database structure associated to the given database name."
  (cdr (assoc database mongo-menu--databases)))

(defun mongo-menu--get-collection (database collection)
  "Return the collection structure associated to the given collection name."
  (let ((collections (mongo-menu--get-database-property :collections database)))
    (cdr (assoc collection collections))))

(defun mongo-menu--get-database-property (property database)
  "Return the value for the given database's property."
  (plist-get (mongo-menu--get-database database) property))

(defun mongo-menu--get-collection-property (property database collection)
  "Return the value for the given collection's property."
  (plist-get (mongo-menu--get-collection database collection) property))

(defun mongo-menu--requote-output (database output)
  (let ((requote (mongo-menu--get-database-function "requote-output" database)))
    (funcall requote output)))

(defun mongo-menu--show-document (row &optional projection)

  "Action to execute on any row: add the row ID to the kill ring"
  (let* ((database (get-text-property 0 :database row))
         (collection (get-text-property 0 :collection row))
         (document-id (get-text-property 0 :id row))
         (show-document (mongo-menu--get-database-function "show-document" database)))
    (funcall show-document database collection document-id)))

(provide 'mongo-menu)
