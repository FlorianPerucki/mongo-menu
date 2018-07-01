(require 'mongo-menu-query)
(require 'mongo-menu-collections)

(defun mongo-menu-databases-buffer ()
  "Lists your defined databases"
  (interactive)
  (let ((buffer (get-buffer-create "mongo-menu: databases"))
        (window (split-window nil 40)))
    (select-window window)
    (switch-to-buffer buffer)
    (mongo-menu-databases-mode))
  )

(defun mongo-menu-databases-list ()
  "Returns databases list in the correct format for tabulated-list-mode"
  (defun get-list-entry (db)
    (list (car db) (vconcat (seq-take (cdr db) 2)))) ; don't show row 3 (password)
  (mapcar 'get-list-entry mongo-menu-databases)
  )

(defun mongo-menu-databases-show-collections ()
  (interactive)
  (mongo-menu-collections-buffer (tabulated-list-get-id))
  )

(defun mongo-menu-databases-show-collections-defined (&optional database)
  (interactive)
  (mongo-menu-collections-buffer (or database (tabulated-list-get-id)) t)
  )

(defvar mongo-menu-databases-mode-map
  (let ((map (make-keymap)))
    (progn
      (define-key map (kbd "q") 'mongo-menu-quit)
      (define-key map (kbd "e") 'mongo-menu-databases-show-collections-defined)
      (define-key map (kbd "c") 'mongo-menu-databases-show-collections)
      ;; TODO: detect collection from 'db.COLLECTION.find()' user input
      ;; (define-key map (kbd "/") 'mongo-menu-databases-query)
      )
    map)
  "Parent keymap for all keymaps of modes derived from `mongo-menu-databases-mode'."
  )

(define-derived-mode mongo-menu-databases-mode tabulated-list-mode "mongo-menu-databases"
  "Parent major mode from which ox-cli major modes inherit."

  (setq tabulated-list-format [("Database" 50 nil) ("User" 30 nil)])
  (setq tabulated-list-use-header-line 't)
  (setq tabulated-list-entries (mongo-menu-databases-list))
  (tabulated-list-init-header)
  )


(provide 'mongo-menu-databases)
