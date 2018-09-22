;;; collect.el --- Read quickly from multiple databases -*- lexical-binding: t -*-

;; Copyright (C) 2018 Florian Perucki

;; Author: Florian Perucki <florian@perucki.fr>
;; URL: https://github.com/florianperucki/collect
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.1") (ivy "0.10.0") (hydra 0.14.0))
;; Keywords: database mongodb sql

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
;; This package adds the MongoDB support to `collect'.
;; By setting the `:type' property to 'mongodb to a database,
;; you tell `collect' to use these methods and settings when
;; selecting this database.

;;; Code:

(defcustom collect-mongodb-client "/usr/local/bin/mongo"
  "Executable MongoDB client path")

(defcustom collect-mongodb-default-sort "_id: -1"
  "Default sorting of documents")

(defcustom collect-mongodb-default-limit 20
  "Default number of documents to fetch in a find query")

;; required functions for a database support

(cl-defun collect--mongodb-compose-query (database collection &key query document-id foreign-key)
  "Build a single query from multiple arguments.
If provided, QUERY must be a string in the MongoDB query syntax, without the surrounding '{}'.
DOCUMENT-ID is the stringified ObjectId for a document to select.
If FOREIGN_KEY is the field name used to query DOCUMENT-ID insteand of '_id'."
  (if document-id
      (let* ((id (if (or (collect--get-database-property :key-not-oid database)
                         (collect--get-collection-property :key-not-oid database collection))
                     (format "\"%s\"" document-id)
                   (format "ObjectId(\"%s\")" document-id)))
             (composed (format "\"%s\": %s" (or foreign-key "_id") id)))
        (if query
            (format "%s, %s" query composed)
          composed))
    query))

(defun collect--mongodb-get-collection-names (database)
  "Fetches collection names remotely."
  (collect--mongodb-json-query database "db.getCollectionNames()"))

(defun collect--mongodb-get-collection-projection (database collection)
  "Return the MongoDB string to use as a query projection, according to the collection's projection settings."
  (let* ((columns (collect--get-collection-columns database collection))
         (fields (mapcar (lambda (column) (format "\"%s\": 1" (plist-get column :name)))
                         columns)))
    (string-join fields ", ")))

(defun collect--mongodb-build-select-query (collection projection &optional skip sort limit query)
  "Return the MongoDB string to use as a SELECT (i.e. a find) query."
  (format "db.%s.find({%s}, {%s}).sort({%s}).skip(%s).limit(%s)"
          collection
          (or query "")
          projection
          (or sort "")
          (or skip 0)
          (or limit 10)))

(defun collect--mongodb-find-by-id (database collection id &optional field)
  "Find a document by its id and return it"
  (let* ((id (if (or (collect--get-database-property :key-not-oid database)
                     (collect--get-collection-property :key-not-oid database collection))
                 (format "\"%s\"" id)
               (format "ObjectId(\"%s\")" id)))
         (query (format "db.%s.findOne({%s: %s})" collection (or field "_id") id)))
    (collect--mongodb-raw-query database query)))

(defun collect--mongodb-run-select-query (database query &optional raw)
  "Run find the query as a mongodb cursor and returns the output as a string JSON list"
  (let ((query (format "var cursor = %s; print(\"\[\"); while(cursor.hasNext()) { printjson(cursor.next()); if (cursor.hasNext()) {print(\",\");}} print(\"]\");" query)))
    (if raw
        (collect--mongodb-raw-query database query)
      (collect--mongodb-json-query database query))))

(defun collect--mongodb-extract-data-documents (database collection data)
  "Return a list of extracted data according to the collection's columns settings.
data: a JSON list of documents"
  (mapcar (apply-partially 'collect--mongodb-extract-data-document database collection) data))

(cl-defun collect--mongodb-show-document (database collection document-id &key field projection)
  "Fetch and display a document in a separate buffer"
  (let* ((buffer (get-buffer-create (format "collect: %s (%s)" collection document-id)))
         (document (collect--mongodb-find-by-id database collection document-id field)))
    (switch-to-buffer buffer)
    (toggle-read-only -1)
    (erase-buffer)
    (insert document)
    (beginning-of-buffer)
    (json-mode)
    (toggle-read-only 1)))

;; internal functions

(defun collect--mongodb-requote-output (output)
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

(defun collect--mongodb-json-query (database query)
  "Run the query against the database and parse the result as JSON."
  (let* ((output (collect--mongodb-raw-query database query))
         (json-object-type 'hash-table)
         (json-array-type 'list)
         (json-key-type 'string))
    (json-read-from-string (collect--mongodb-requote-output output))))

(defun collect--mongodb-raw-query (database query)
  "Run the query against the database and return the result as a string."
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
        (if (or
             (string-match-p "Failed to connect" output)
             (string-match-p "exception: connect failed" output))
            (error "Connection failed")
          output)))))

(defun collect--mongodb-extract-data-document (database collection row)
  "Return a list of columns for a single document, according to the collection's columns settings.
The _id row is always added as the first element."
  (let* ((_id (collect--mongodb-get-document-field "_id" row))
         (columns (collect--get-collection-columns database collection))
         (fields (mapcar (apply-partially 'collect--mongodb-value-template row) columns)))
    (append (list (collect--mongodb-value-pretty _id)) fields)))

(defun collect--mongodb-value-template (row field)
  "Format a single field value and escape characters not supported by json-mode."
  (let* ((field (plist-get field :name))
         (value (collect--mongodb-get-document-field field row)))
    (collect--mongodb-value-pretty value)))

(defun collect--mongodb-get-document-field (field document)
  "Returns field value in document. Supports dotted fields for accessing nested documents"
  (let* ((parts (split-string field "\\\." t))
         (subdocument (gethash (car parts) document)))
    (if (and (> (length parts) 1)
             (string-equal (type-of subdocument) "hash-table"))
        ;; we need to go deeper
        (progn
          (collect--mongodb-get-document-field
           (string-join (cdr parts) ".") ; rebuild dotted path, skipping the first one
           subdocument)
          )
      ;; we found what we're looking for
      (gethash field document))))

(defun collect--mongodb-value-pretty (value)
  "Escape characters not supported by json-mode."
  (if (eq (type-of value) 'string)
      (replace-regexp-in-string
       "^\\(ObjectId(\"\\(.*?\\)\")\\)"
       "\\2"
       value nil nil 1)
    value)
  )

(provide 'collect-mongodb)
;;; collect-mongodb.el ends here
