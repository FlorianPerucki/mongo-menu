(defun mongo-menu-get-database ()
  "Returns current database info"
  (cdr (assoc mongo-menu-current-database mongo-menu-databases))
  )

(defun mongo-menu-requote-output (output)
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
     output)
    )
  )

(defun mongo-menu-json-query (query)
  (let* ((output (mongo-menu-raw-query query))
         (json-object-type 'hash-table)
         (json-array-type 'list)
         (json-key-type 'string))

    (json-read-from-string (mongo-menu-requote-output output))
    )
  )

(defun mongo-menu-raw-query (query)
  (let* ((db (mongo-menu-get-database))
         (host (elt db 0))
         (user (elt db 1))
         (password (elt db 2))
         (cmd (format "%s -u %s -p %s %s --quiet --eval '%s'"
                      mongo-menu-client
                      user
                      password
                      host
                      query
                      )))

    ;; (message "Mongo-menu command: %s" cmd)
    (shell-command-to-string cmd)
    )
  )

(defun mongo-menu-query-cursor (query)
  "Runs the query as a mongodb cursor and returns the output as a string JSON list"
  (let ((query (format "var cursor = %s; print(\"\[\"); while(cursor.hasNext()) { printjson(cursor.next()); if (cursor.hasNext()) {print(\",\");}} print(\"]\");" query)))
    (mongo-menu-json-query query)
    )
  )

(provide 'mongo-menu-query)
