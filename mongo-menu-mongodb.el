;; -*- lexical-binding: t -*-

(defcustom mongo-menu-mongodb-client "/usr/local/bin/mongo"
  "Executable MongoDB client path")

(defcustom mongo-menu-collection-default-limit-mongodb 20
  "Default number of documents to fetch in a SELECT query")

(defvar mongo-menu--collection-default-sort-mongodb "{_id: -1}")


;; required functions implementation for a database support

(defun mongo-menu--get-collection-names-mongodb (database)
  "Fetches collection names remotely."
  (mongo-menu--json-query-mongodb database "db.getCollectionNames()"))

(defun mongo-menu--get-collection-projection-mongodb (database collection)
  "Return the MongoDB string to use as a query projection, according to the collection's projection settings."
  (let* ((columns (mongo-menu--get-collection-columns database collection))
         (fields (mapcar (lambda (column) (format "\"%s\": 1" (plist-get column :name)))
                         columns)))
    (format "{%s}" (string-join fields ", "))))

(defun mongo-menu--build-select-query-mongodb (collection projection &optional skip sort limit query)
  "Return the MongoDB string to use as a SELECT (i.e. a find) query."
  (format "db.%s.find(%s, %s).sort(%s).skip(%s).limit(%s)"
          collection
          (or query "{}")
          projection
          (or sort "{}")
          (or skip 0)
          (or limit 10)))

(defun mongo-menu--run-select-query-mongodb (database query)
  "Run find the query as a mongodb cursor and returns the output as a string JSON list"
  (let ((query (format "var cursor = %s; print(\"\[\"); while(cursor.hasNext()) { printjson(cursor.next()); if (cursor.hasNext()) {print(\",\");}} print(\"]\");" query)))
    (mongo-menu--json-query-mongodb database query)))

(defun mongo-menu--extract-data-documents-mongodb (database collection data)
  "Return a list of extracted data according to the collection's columns settings.
data: a JSON list of documents"
  (mapcar (apply-partially 'mongo-menu--extract-data-document-mongodb database collection) data))


;; internal functions

(defun mongo-menu--requote-output-mongodb (output)
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

(defun mongo-menu--json-query-mongodb (database query)
  "Run the query against the datatable and parse the result as JSON."
  (let* ((output (mongo-menu--raw-query-mongodb database query))
         (json-object-type 'hash-table)
         (json-array-type 'list)
         (json-key-type 'string))
    (json-read-from-string (mongo-menu--requote-output-mongodb output))))

(defun mongo-menu--raw-query-mongodb (database query)
  "Run the query against the datatable and return the result as a string."
  (let* ((host (mongo-menu--get-database-property :host database))
         (user (mongo-menu--get-database-property :user database))
         (password (mongo-menu--get-database-property :password database))
         (cmd (format "%s -u %s -p %s %s --quiet --eval '%s'"
                      mongo-menu-mongodb-client
                      user
                      password
                      host
                      query
                      )))
    (shell-command-to-string cmd)))

(defun mongo-menu--extract-data-document-mongodb (database collection row)
  "Return a list of columns for a single document, according to the collection's columns settings.
The _id row is always added as the first element."
  (let* ((_id (mongo-menu--get-document-field-mongodb "_id" row))
         (columns (mongo-menu--get-collection-columns database collection))
         (fields (mapcar (apply-partially 'mongo-menu--value-template-mongodb row) columns)))
    (append (list (mongo-menu--value-pretty-mongodb _id)) fields)))

(defun mongo-menu--value-template-mongodb (row field)
  "Format a single field value and escape characters not supported by json-mode."
  (let* ((field (plist-get field :name))
        (value (mongo-menu--get-document-field-mongodb field row)))
    (mongo-menu--value-pretty-mongodb value)))

(defun mongo-menu--get-document-field-mongodb (field document)
  "Returns field value in document. Supports dotted fields for accessing nested documents"
  (let* ((parts (split-string field "\\\." t))
         (subdocument (gethash (car parts) document)))
    (if (and (> (length parts) 1)
             (string-equal (type-of subdocument) "hash-table"))
        ;; we need to go deeper
        (progn
          (mongo-menu--get-document-field-mongodb
            (string-join (cdr parts) ".") ; rebuild dotted path, skipping the first one
            subdocument)
          )
      ;; we found what we're looking for
      (gethash field document))))

(defun mongo-menu--value-pretty-mongodb (value)
  "Escape characters not supported by json-mode."
  (if (eq (type-of value) 'string)
      (replace-regexp-in-string
       "^\\(ObjectId(\"\\(.*?\\)\")\\)"
       "\\2"
       value nil nil 1)
    (format "%s" value))
  )

(provide 'mongo-menu-mongodb)
