(require 'mongo-menu-document)

(defvar mongo-menu-current-document-id nil
  "Mongo _id of document currently being visualized"
  )

(defun mongo-menu-get-current-template ()
  (interactive)
  (let ((templates (cdr (assoc mongo-menu-current-database mongo-menu-collection-templates))))
    (or (cdr (assoc mongo-menu-current-collection templates)) mongo-menu-collection-template-default))
  )

(defun mongo-menu-collection-get-projection ()
  "returns projection string according to collection template (i.e. to columns to display)"
  (interactive)
  (let* ((template (mongo-menu-get-current-template))
         (fields (mapcar (lambda (item) (format "\"%s\": 1" (car item))) template)))
    (format "{%s}" (string-join fields ", ")))
  )

(defun mongo-menu-collection-list (&optional skip-projection-p)
  "List documents in current collection"
  (interactive)
  (let* ((projection (if skip-projection-p "{}" (mongo-menu-collection-get-projection)))
         (query (format "db.%s.find(%s, %s).sort(%s).skip(%s).limit(%s)"
                        mongo-menu-current-collection
                        (or mongo-menu-collection-current-query "{}")
                        projection
                        mongo-menu-query-sort
                        mongo-menu-query-skip
                        mongo-menu-query-limit))
         (data (mongo-menu-query-cursor query)))
    (mapcar 'mongo-menu-collection-extract data))
  )

(defun mongo-menu-collection-extract (row)
  (let* ((_id (mongo-menu-document-get-field "_id" row))
         (template (mongo-menu-get-current-template))
         (fields (mapcar 'mongo-menu-value-template template)))
    (list _id (apply #'vector fields)))
  )

(defun mongo-menu-value-template (field)
  (let ((value (mongo-menu-document-get-field (car field) row)))
    (mongo-menu-value-pretty value))
  )

(defun mongo-menu-value-pretty (value)
  (if (eq (type-of value) 'string)
      (replace-regexp-in-string
       "^\\(ObjectId(\"\\(.*?\\)\")\\)"
       "\\2"
       value nil nil 1)
    (format "%s" value))
  )

(defun mongo-menu-visualize-document ()
  (interactive)

  (mongo-menu-document-buffer mongo-menu-current-database
                              mongo-menu-current-collection
                              (tabulated-list-get-id)))

(defun mongo-menu-collection-query (query)
  (interactive "sQuery: ")
  (let ((mongo-menu-collection-current-query query))
    (mongo-menu-collection-refresh)
    )
  )

(defun mongo-menu-collection-refresh ()
  (interactive)
  (setq tabulated-list-entries (mongo-menu-collection-list t))
  (tabulated-list-print)
  )

(defvar mongo-menu-collection-mode-map
  (let ((map (make-keymap)))
    (progn
      (define-key map (kbd "q") 'mongo-menu-quit)
      (define-key map (kbd "y") 'mongo-menu-yank)
      (define-key map (kbd "e") 'mongo-menu-visualize-document)
      (define-key map (kbd "v") 'mongo-menu-visualize-document)
      (define-key map (kbd "/") 'mongo-menu-collection-query)
      )
    map)
  "Parent keymap for all keymaps of modes derived from `mongo-menu-collection-mode'."
  )

(defun mongo-menu-collection-buffer (database collection &optional limit skip sort query suffix)
  (let ((buffer (get-buffer-create (format "mongo-menu: %s %s" mongo-menu-current-collection (or suffix "")))))

    (switch-to-buffer buffer)
    (mongo-menu-collection-mode)

    (set (make-local-variable 'mongo-menu-current-database) database)
    (set (make-local-variable 'mongo-menu-current-collection) collection)

    (when limit
      (set (make-local-variable 'mongo-menu-query-limit) limit))
    (when skip
      (set (make-local-variable 'mongo-menu-query-skip) skip))
    (when sort
      (set (make-local-variable 'mongo-menu-query-sort) sort))

    (when query
      (set (make-local-variable 'mongo-menu-collection-current-query) query))

    (let ((template (mongo-menu-get-current-template)))

      (setq tabulated-list-format template)
      (setq tabulated-list-use-header-line 't)
      (setq tabulated-list-entries (mongo-menu-collection-list))
      (tabulated-list-init-header)
      (tabulated-list-print)
      )
    )
  )

(define-derived-mode mongo-menu-collection-mode tabulated-list-mode "mongo-menu-collection"
  "Parent major mode from which ox-cli major modes inherit.")

(provide 'mongo-menu-collection)
