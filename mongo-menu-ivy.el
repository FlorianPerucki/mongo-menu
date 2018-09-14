;; -*- lexical-binding: t -*-


;; required functions implementation for a front support

(defun collect--display-ivy (prompt entries actions)
  "Run front-end command to display results"
  (ivy-read prompt
            entries
            :action actions))

(defun collect--databases-ivy (entries)
  "Display database list.
entries: string list"
  (collect--display :entries (mapcar 'collect--format-entry-database-ivy entries)
                       :actions (list
                                   '("o" collect--show-collections-defined-ivy "Show collections")
                                   '("O" collect--show-collections-ivy "Show all collections"))))

(defun collect--collections-ivy (database entries)
  "Display collections list for a given database.
database: string
entries: collections plist"
  (collect--display :database database
                       :entries (mapcar (apply-partially 'collect--format-entry-collection-ivy database) entries)))

(defun collect--get-prompt-ivy (&optional database collection)
  "Return prompt for the document type currently being displayed:
collections if collection is nil, documents if collection is non-nil"
  (if collection
      (format "%s > %s > " database collection)
    (if database
        (format "%s > " database)
      "> ")))

(defun collect--get-actions-ivy (&optional database collection)
  "Return actions for the row type currently being displayed
collections if collection is nil, documents if collection is non-nil"
  (if collection
      ;; actions on a document row
      (let ((default-actions (list
                              '("o" collect--show-document "Open in buffer")
                              '("y" collect--action-copy-id "Copy row ID")))
            (actions (mapcar 'collect--build-action-ivy (collect--get-collection-property :actions database collection))))
        (append default-actions (or actions (list))))

    ;; actions on a collection row
    (if database (let ((default-actions (list
                                         '("o" collect--action-show-documents-entries-ivy "Show documents")))
                       (actions (mapcar 'collect--build-action-ivy (collect--get-database-property :actions database))))
                   (append default-actions (or actions (list))))

      ;; actions on a database row
      (list))
    ))


;; internals

(defun collect--format-entry-database-ivy (entry)
  "Return a formatted string for a database row.
entry: plist

The returned string has the following property:
database: string name of the database"
  (let* ((name (car entry))
         (host (collect--get-property :host entry)))
    (propertize
     (format "%1$#-50s %2$#-50s" (s-truncate 50 name) (s-truncate 50 host))
     :database name)))

(defun collect--format-entry-collection-ivy (database entry)
  "Return the collection row string 'entry' with the following properties:
database: string name of the database
collection: string name of the collection"
  (propertize
   entry
   :database database
   :collection entry))

(iter-defun collect--format-document-fields-ivy (database collection values)
  "Generate a formatted string for each field values of a single row.
database: string
collection: string
values: list of values (string, integer, etc.)"
  (let* ((collect--tmp-index 0)
         (columns (collect--get-collection-columns database collection))
         (len (length values)))
    (when (not (equal len (length columns)))
      (error "Column templates and values mismatch"))
    (while (< collect--tmp-index len)
      (let* ((value (elt values collect--tmp-index))
             (column (elt columns collect--tmp-index))
             (width (collect--get-column-width database collection column)))
        (iter-yield (format
                     (format "%%1$#-%ss" width) ; build the format we want before using it
                     (s-truncate width value))))
      (setq collect--tmp-index (1+ collect--tmp-index)))))

(defun collect--format-entry-document-ivy (database collection entry)
  "Return a string for a single document row.

The returned string has the following properties:
database: string name of the database
collection: string name of the collection
id: unique row identifier"
  (let ((values (list)))
    (iter-do (value (collect--format-document-fields-ivy database
                                                            collection
                                                            (cdr entry)))
      (setq values (append values (list value))))
    (let* ((id (car entry))
           (output (string-join values " ")))
      (propertize output
                  :database database
                  :collection collection
                  :id id))))

(defun collect--show-collections-defined-ivy (entry)
  "Show a list of user-defined collections"
  (collect--show-collections-ivy entry t))

(defun collect--show-collections-ivy (entry &optional defined)
  "Show a list of collections.
If defined is non-nil, not database fetch is done and only user-defined collections are displayed."
  (let ((database (get-text-property 0 :database entry)))
    (collect-show-collections database defined)))

(defun collect--action-show-documents-entries-ivy (entry &optional skip query)
  "Action to execute on a collection row: query and display a list of documents"
  (let* ((database (get-text-property 0 :database entry))
         (collection (get-text-property 0 :collection entry)))
    (collect--action-show-documents-ivy database collection skip query))) ; TODO limit + sort

(defun collect--build-action-ivy (action)
  (let* ((key (plist-get action :key))
         (name (plist-get action :name)))
    (list key (apply-partially 'collect--run-action action) name)))

(defun collect--action-show-documents-ivy (database collection &optional skip query limit sort)
  "Fetch and display collection's data, filtered by query if provided"
  (interactive)
  (let* ((documents (collect--build-and-run-select-query database collection skip query limit sort))
         (entries (mapcar (apply-partially 'collect--format-entry-document-ivy database collection) documents)))
    (collect--display :database database
                         :collection collection
                         :entries entries)))

(provide 'collect-ivy)
