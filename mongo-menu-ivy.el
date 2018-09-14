;; -*- lexical-binding: t -*-


;; required functions implementation for a front support

(defun mongo-menu--display-ivy (prompt entries actions)
  "Run front-end command to display results"
  (ivy-read prompt
            entries
            :action actions))

(defun mongo-menu--databases-ivy (entries)
  "Display database list.
entries: string list"
  (mongo-menu--display :entries (mapcar 'mongo-menu--format-entry-database-ivy entries)
                       :actions (list
                                   '("o" mongo-menu--show-collections-defined-ivy "Show collections")
                                   '("O" mongo-menu--show-collections-ivy "Show all collections"))))

(defun mongo-menu--collections-ivy (database entries)
  "Display collections list for a given database.
database: string
entries: collections plist"
  (mongo-menu--display :database database
                       :entries (mapcar (apply-partially 'mongo-menu--format-entry-collection-ivy database) entries)))

(defun mongo-menu--get-prompt-ivy (&optional database collection)
  "Return prompt for the document type currently being displayed:
collections if collection is nil, documents if collection is non-nil"
  (if collection
      (format "%s > %s > " database collection)
    (if database
        (format "%s > " database)
      "> ")))

(defun mongo-menu--get-actions-ivy (&optional database collection)
  "Return actions for the row type currently being displayed
collections if collection is nil, documents if collection is non-nil"
  (if collection
      ;; actions on a document row
      (let ((default-actions (list
                              '("o" mongo-menu--show-document "Open in buffer")
                              '("y" mongo-menu--action-copy-id "Copy row ID")))
            (actions (mapcar 'mongo-menu--build-action-ivy (mongo-menu--get-collection-property :actions database collection))))
        (append default-actions (or actions (list))))

    ;; actions on a collection row
    (if database (let ((default-actions (list
                                         '("o" mongo-menu--action-show-documents-entries-ivy "Show documents")))
                       (actions (mapcar 'mongo-menu--build-action-ivy (mongo-menu--get-database-property :actions database))))
                   (append default-actions (or actions (list))))

      ;; actions on a database row
      (list))
    ))


;; internals

(defun mongo-menu--format-entry-database-ivy (entry)
  "Return a formatted string for a database row.
entry: plist

The returned string has the following property:
database: string name of the database"
  (let* ((name (car entry))
         (host (mongo-menu--get-property :host entry)))
    (propertize
     (format "%1$#-50s %2$#-50s" (s-truncate 50 name) (s-truncate 50 host))
     :database name)))

(defun mongo-menu--format-entry-collection-ivy (database entry)
  "Return the collection row string 'entry' with the following properties:
database: string name of the database
collection: string name of the collection"
  (propertize
   entry
   :database database
   :collection entry))

(iter-defun mongo-menu--format-document-fields-ivy (database collection values)
  "Generate a formatted string for each field values of a single row.
database: string
collection: string
values: list of values (string, integer, etc.)"
  (let* ((mongo-menu--tmp-index 0)
         (columns (mongo-menu--get-collection-columns database collection))
         (len (length values)))
    (when (not (equal len (length columns)))
      (error "Column templates and values mismatch"))
    (while (< mongo-menu--tmp-index len)
      (let* ((value (elt values mongo-menu--tmp-index))
             (column (elt columns mongo-menu--tmp-index))
             (width (mongo-menu--get-column-width database collection column)))
        (iter-yield (format
                     (format "%%1$#-%ss" width) ; build the format we want before using it
                     (s-truncate width value))))
      (setq mongo-menu--tmp-index (1+ mongo-menu--tmp-index)))))

(defun mongo-menu--format-entry-document-ivy (database collection entry)
  "Return a string for a single document row.

The returned string has the following properties:
database: string name of the database
collection: string name of the collection
id: unique row identifier"
  (let ((values (list)))
    (iter-do (value (mongo-menu--format-document-fields-ivy database
                                                            collection
                                                            (cdr entry)))
      (setq values (append values (list value))))
    (let* ((id (car entry))
           (output (string-join values " ")))
      (propertize output
                  :database database
                  :collection collection
                  :id id))))

(defun mongo-menu--show-collections-defined-ivy (entry)
  "Show a list of user-defined collections"
  (mongo-menu--show-collections-ivy entry t))

(defun mongo-menu--show-collections-ivy (entry &optional defined)
  "Show a list of collections.
If defined is non-nil, not database fetch is done and only user-defined collections are displayed."
  (let ((database (get-text-property 0 :database entry)))
    (mongo-menu-show-collections database defined)))

(defun mongo-menu--action-show-documents-entries-ivy (entry &optional skip query)
  "Action to execute on a collection row: query and display a list of documents"
  (let* ((database (get-text-property 0 :database entry))
         (collection (get-text-property 0 :collection entry)))
    (mongo-menu--action-show-documents-ivy database collection skip query))) ; TODO limit + sort

(defun mongo-menu--build-action-ivy (action)
  (let* ((key (plist-get action :key))
         (name (plist-get action :name)))
    (list key (apply-partially 'mongo-menu--run-action action) name)))

(defun mongo-menu--action-show-documents-ivy (database collection &optional skip query limit sort)
  "Fetch and display collection's data, filtered by query if provided"
  (interactive)
  (let* ((documents (mongo-menu--build-and-run-select-query database collection skip query limit sort))
         (entries (mapcar (apply-partially 'mongo-menu--format-entry-document-ivy database collection) documents)))
    (mongo-menu--display :database database
                         :collection collection
                         :entries entries)))

(provide 'mongo-menu-ivy)
