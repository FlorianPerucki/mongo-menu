;; TODO
;; pagination
;; fetch indices => collection template

(defvar mongo-menu-databases nil)

(defvar mongo-menu-client "/usr/local/bin/mongo")
(defvar mongo-menu-collection-template-default [("_id" 50 nil)])

(defvar mongo-menu-current-database nil)
(defvar mongo-menu-current-collection nil)

(defvar mongo-menu-collection-current-query nil
  "Currently defined Mongo query to fetch documents from current collection")

(defvar mongo-menu-collections-defined-only nil
  "Whether mongo-menu should fetch collections or just show the ones that were defined")

(defvar mongo-menu-query-skip 0)
(defvar mongo-menu-query-limit 10)
(defvar mongo-menu-query-sort "{_id: -1}")

(defun mongo-menu-quit ()
  (interactive)
  (kill-current-buffer)
  (delete-window)
  )

(require 'mongo-menu-databases)

(provide 'mongo-menu)
