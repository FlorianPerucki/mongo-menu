;; -*- lexical-binding: t -*-

(defcustom collect-mongodb-client "/usr/local/bin/mongo"
  "Executable MongoDB client path")

(defcustom collect-collection-default-limit-mongodb 20
  "Default number of documents to fetch in a SELECT query")

(defvar collect--collection-default-sort-mongodb "_id: -1")


;; required functions implementation for a database support

(cl-defun collect--compose-query-mongodb (&key query document-id foreign-key)
  "Build a single query from multiple arguments"
  (let ((query (or query "")))
    (if document-id
        (format "%s, %s: ObjectId(\"%s\")" query (or foreign-key "_id") document-id)
      query)))

(defun collect--get-collection-names-mongodb (database)
  "Fetches collection names remotely."
  (collect--json-query-mongodb database "db.getCollectionNames()"))

(defun collect--get-collection-projection-mongodb (database collection)
  "Return the MongoDB string to use as a query projection, according to the collection's projection settings."
  (let* ((columns (collect--get-collection-columns database collection))
         (fields (mapcar (lambda (column) (format "\"%s\": 1" (plist-get column :name)))
                         columns)))
    (string-join fields ", ")))

(defun collect--build-select-query-mongodb (collection projection &optional skip sort limit query)
  "Return the MongoDB string to use as a SELECT (i.e. a find) query."
  (format "db.%s.find({%s}, {%s}).sort({%s}).skip(%s).limit(%s)"
          collection
          (or query "")
          projection
          (or sort "")
          (or skip 0)
          (or limit collect-collection-default-limit-mongodb)))

(defun collect--find-by-id-mongodb (database collection id &optional field)
  "Find a document by its id and return it"
  (let ((query (format "db.%s.findOne({%s: ObjectId(\"%s\")})" collection (or field "_id") id)))
    (collect--raw-query-mongodb database query)))

(defun collect--run-select-query-mongodb (database query &optional raw)
  "Run find the query as a mongodb cursor and returns the output as a string JSON list"
  (let ((query (format "var cursor = %s; print(\"\[\"); while(cursor.hasNext()) { printjson(cursor.next()); if (cursor.hasNext()) {print(\",\");}} print(\"]\");" query)))
    (if raw
        (collect--raw-query-mongodb database query)
      (collect--json-query-mongodb database query))))

(defun collect--extract-data-documents-mongodb (database collection data)
  "Return a list of extracted data according to the collection's columns settings.
data: a JSON list of documents"
  (mapcar (apply-partially 'collect--extract-data-document-mongodb database collection) data))

(defun collect--show-document-mongodb (database collection document-id &optional field)
  "Fetch and display a document in a separate buffer"
  (let* ((buffer (get-buffer-create (format "collect: %s (%s)" collection document-id)))
         (document (collect--find-by-id-mongodb database collection document-id field)))

    (switch-to-buffer buffer)
    (toggle-read-only -1)

    (erase-buffer)
    (insert document)
    (beginning-of-buffer)

    (json-mode)
    (toggle-read-only 1)))


;; internal functions

(defun collect--requote-output-mongodb (output)
  "Adds quotes around ObjectId in OUTPUT.
When mongo outputs json, it has unquoted ObjectIds in it that
emacs cannot interpret as json. "
  (let ((output (replace-regexp-in-string
                 "ObjectId(\"\\(.*?\\)\")"
                 "\"ObjectId(\\\\\"\\1\\\\\")\""
                 output)))
    (replace-regexp-in-string
     "ISODate(\"\\(.*?\\)\")"
     "\"ISODate(\\\\\"\\1\\\\\")\""
     output)))

(defun collect--json-query-mongodb (database query)
  "Run the query against the datatable and parse the result as JSON."
  (let* ((output (collect--raw-query-mongodb database query))
         (json-object-type 'hash-table)
         (json-array-type 'list)
         (json-key-type 'string))
    (json-read-from-string (collect--requote-output-mongodb output))))

(defun collect--raw-query-mongodb (database query)
  "Run the query against the datatable and return the result as a string."
  (let* ((host (collect--get-database-property :host database))
         (user (collect--get-database-property :user database))
         (password (collect--get-database-property :password database))
         (cmd (format "%s -u %s -p %s %s --quiet --eval '%s'"
                      collect-mongodb-client
                      user
                      password
                      host
                      query
                      )))
    (let ((output (shell-command-to-string cmd)))
      (progn
        (when collect--debug
          (message "[MONGODB] QUERY: %s" query)
          (message "[MONGODB] RESPONSE: %s" output))
        output))))

(defun collect--extract-data-document-mongodb (database collection row)
  "Return a list of columns for a single document, according to the collection's columns settings.
The _id row is always added as the first element."
  (let* ((_id (collect--get-document-field-mongodb "_id" row))
         (columns (collect--get-collection-columns database collection))
         (fields (mapcar (apply-partially 'collect--value-template-mongodb row) columns)))
    (append (list (collect--value-pretty-mongodb _id)) fields)))

(defun collect--value-template-mongodb (row field)
  "Format a single field value and escape characters not supported by json-mode."
  (let* ((field (plist-get field :name))
        (value (collect--get-document-field-mongodb field row)))
    (collect--value-pretty-mongodb value)))

(defun collect--get-document-field-mongodb (field document)
  "Returns field value in document. Supports dotted fields for accessing nested documents"
  (let* ((parts (split-string field "\\\." t))
         (subdocument (gethash (car parts) document)))
    (if (and (> (length parts) 1)
             (string-equal (type-of subdocument) "hash-table"))
        ;; we need to go deeper
        (progn
          (collect--get-document-field-mongodb
            (string-join (cdr parts) ".") ; rebuild dotted path, skipping the first one
            subdocument)
          )
      ;; we found what we're looking for
      (gethash field document))))

(defun collect--value-pretty-mongodb (value)
  "Escape characters not supported by json-mode."
  (if (eq (type-of value) 'string)
      (replace-regexp-in-string
       "^\\(ObjectId(\"\\(.*?\\)\")\\)"
       "\\2"
       value nil nil 1)
    (format "%s" value))
  )

(provide 'collect-mongodb)
