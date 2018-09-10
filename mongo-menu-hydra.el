;;; -*- lexical-binding: t; -*-

(defun mongo-menu--hydra-run-query (database collection action)
  (let* ((skip (plist-get (cdr action) :skip))
         (query (plist-get (cdr action) :query))
         (sort (plist-get (cdr action) :sort))
         (limit (plist-get (cdr action) :limit)))
    `(lambda ()
       (interactive)
       (let ((entries (mongo-menu--build-and-run-select-query ,database ,collection ,skip ,query ,limit ,sort)))
         (mongo-menu--display (format "%s > %s > " ,database ,collection)
                              (mapcar (apply-partially 'mongo-menu--format-entry-document ,database ,collection) entries)
                              ;; TODO: custom actions
                              '(1
                                ("y" mongo-menu--action-copy-id "Copy row ID")))))))


(defun mongo-menu--build-hydra-queries (database collection)
  (let* ((queries (mongo-menu--get-collection-property :queries database collection))
         (default-queries (list
                           `("1" (lambda () (interactive) (mongo-menu--action-show-documents-ivy ,database ,collection)) "All")))
         (queries-heads
          (mapcar
           (lambda (query)
             (let* ((key (plist-get (cdr query) :key))
                    (name (plist-get (cdr query) :name)))
               (list key (mongo-menu--hydra-run-query database collection query) name :exit t)))
           queries))
         (queries-heads (append (or default-queries (list)) queries-heads)))
    (eval `(defhydra mongo-menu--hydra-tmp (:color blue)
             "Mongo-menu"
             ,@queries-heads))
    (mongo-menu--hydra-tmp/body)))

(defun mongo-menu--build-hydra-collections (database)
  (let* ((collections (mongo-menu--get-collection-names database t))
         (collection-heads
          (mapcar
           (lambda (collection)
             (let ((key (mongo-menu--get-collection-property :key database collection)))
               (list key `(mongo-menu--build-hydra-queries ,database ,collection) collection :exit t)))
           collections)))
    (eval `(defhydra mongo-menu--hydra-tmp (:color blue)
             "Mongo-menu"
             ,@collection-heads))
    (mongo-menu--hydra-tmp/body)))

(defun mongo-menu--build-hydras ()
  (let ((database-heads
         (mapcar (lambda (x)
                   (list (plist-get (cdr x) :key) `(mongo-menu--build-hydra-collections ,(car x)) (car x) :exit t))
                 mongo-menu--databases)))
    (eval `(defhydra hydra-mongo-menu (:color blue)
             "Mongo-menu"
             ,@database-heads))))

(mongo-menu--build-hydras)
(global-set-key (my/kbd "q") 'hydra-mongo-menu/body)

(provide 'mongo-menu-hydra)
