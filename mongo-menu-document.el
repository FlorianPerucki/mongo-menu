(defun mongo-menu-document-get-field (field document)
  "Returns field value in document. Supports dotted fields for accessing nested documents"
  (let* ((parts (split-string field "\\\." t))
         (subdocument (gethash (car parts) document)))
    (if (and (> (length parts) 1)
             (string-equal (type-of subdocument) "hash-table"))
        ;; we need to go deeper
        (progn
          (mongo-menu-document-get-field
            (string-join (cdr parts) ".") ; rebuild dotted path, skipping the first one
            subdocument)
          )
      ;; we found what we're looking for
      (gethash field document))
    )
  )

(defun mongo-menu-get-current-document (&optional jsonp)
  "Queries the current daabase and collection for the current document ID"
  (let ((query (format "db.%s.findOne({_id: %s})" mongo-menu-current-collection mongo-menu-current-document-id)))
    (if jsonp
        (mongo-menu-json-query query)
      (mongo-menu-raw-query query)
      )
    )
  )

(defun mongo-menu-document-buffer (database collection id)
  "Creates and switches to a buffer displaying the JSON body of a document"
  (let* ((buffer (get-buffer-create "mongo-menu: document"))
         (window (split-window-right)))

    (select-window window)
    (switch-to-buffer buffer)
    (mongo-menu-document-mode)
    (toggle-read-only -1)
    ;; avoid performance issues with big json dumps
    (display-line-numbers-mode -1)

    (set (make-local-variable 'mongo-menu-current-database) database)
    (set (make-local-variable 'mongo-menu-current-collection) collection)
    (set (make-local-variable 'mongo-menu-current-document-id) id)

    (let ((document (mongo-menu-get-current-document)))

      ;; requote to avoid json parsing errors
      (insert (mongo-menu-requote-output document))

      ;; pretty print
      (setq-local json-reformat:indent-width 2)
      (json-pretty-print-buffer)

      (beginning-of-buffer)

      ;; TODO not actually working currently
      (perform-replace "\\\\\"" "\"" nil t nil)
      (perform-replace "\\(\"ObjectId(\"\\(.*?\\)\")\"\\)" "ObjectId(\"\\2\")" nil t nil)

      )
    )
  )

(defvar mongo-menu-document-mode-map
  (let ((map (make-keymap)))
    (progn
      (define-key map (kbd "q") 'mongo-menu-quit))
    map)
  "Parent keymap for all keymaps of modes derived from `mongo-menu-document-mode'."
  )

(define-derived-mode mongo-menu-document-mode json-mode "mongo-menu-document"
  "Parent major mode from which ox-cli major modes inherit.")

(provide 'mongo-menu-document)
