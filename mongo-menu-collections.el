(require 'mongo-menu-query)
(require 'mongo-menu-collection)

(defun mongo-menu-collections-get-names ()
  (if mongo-menu-collections-defined-only
      (mapcar 'car (cdr (assoc mongo-menu-current-database mongo-menu-collection-templates)))
      (append (mongo-menu-json-query "db.getCollectionNames()") nil)
      )
  )

(defun mongo-menu-collections-list ()
  (interactive)
  ;; append to convert vector to list
  (let* ((collections (mongo-menu-collections-get-names))
         (index -1))
    (defun get-list-entry (collection)
      (setq index (1+ index))
      (list collection (vector collection))
      )
    (mapcar 'get-list-entry collections)
    )
  )

(defun mongo-menu-collections-show ()
  (interactive)
  (let ((buffer (get-buffer-create "mongo-menu: collections")))
    (switch-to-buffer buffer)
    (mongo-menu-collections-mode)
    )
  )

(defun mongo-menu-collections-buffer (database &optional defined)
  (interactive)
  (let ((buffer (get-buffer-create "mongo-menu: collections")))
    (switch-to-buffer buffer)
    (mongo-menu-collections-mode)
    (set (make-local-variable 'mongo-menu-current-database) database)
    (make-local-variable 'mongo-menu-collections-defined-only)

    (when defined (setq mongo-menu-collections-defined-only t))

    (setq tabulated-list-format [("Collection" 50 nil)])
    (setq tabulated-list-use-header-line 't)
    (setq tabulated-list-entries (mongo-menu-collections-list))

    (tabulated-list-init-header)
    (tabulated-list-print)
    )
  )

(defun mongo-menu-collections-show-defined ()
  (interactive)
  (let* ((buffer (get-buffer-create "mongo-menu: collections"))
         (mongo-menu-collections-defined-only t))
    (switch-to-buffer buffer)
    (mongo-menu-collections-mode)
    )
  )

(defun mongo-menu-collections-enter ()
  (interactive)
  (let ((mongo-menu-current-collection (tabulated-list-get-id)))
    (mongo-menu-collection-buffer mongo-menu-current-database mongo-menu-current-collection)
    )
  )

(defun mongo-menu-collections-query-db (database collection query &optional limit skip sort suffix)
  "Runs query on provided database's collection"
  (interactive)

  ;; set buffer-local vars
  (let ((mongo-menu-current-database database)
        (mongo-menu-current-collection collection)
        (mongo-menu-collection-current-query query)
        (mongo-menu-query-limit (or limit mongo-menu-query-limit))
        (mongo-menu-query-skip (or skip mongo-menu-query-skip))
        (mongo-menu-query-sort (or sort mongo-menu-query-sort)))

    (mongo-menu-collection-buffer database
                                  collection
                                  mongo-menu-query-limit
                                  mongo-menu-query-skip
                                  mongo-menu-query-sort
                                  mongo-menu-collection-current-query
                                  suffix)))

(defun mongo-menu-collections-query (query)
  (interactive "sQuery: ")
  (setq (mongo-menu-collection-current-query query))
  (mongo-menu-collections-enter)
  )

(defvar mongo-menu-collections-mode-map
  (let ((map (make-keymap)))
    (progn
      ;; (define-key map (kbd "y") 'mongo-menu-yank)
      (define-key map (kbd "q") 'mongo-menu-quit)
      (define-key map (kbd "e") 'mongo-menu-collections-enter)
      (define-key map (kbd "/") 'mongo-menu-collections-query)
      )
    map)
  "Parent keymap for all keymaps of modes derived from `mongo-menu-collections-mode'."
  )

(define-derived-mode mongo-menu-collections-mode tabulated-list-mode "mongo-menu-collections"
  "Parent major mode from which ox-cli major modes inherit."


  )


(provide 'mongo-menu-collections)
