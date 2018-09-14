;;; -*- lexical-binding: t; -*-

(defun mongo-menu--build-hydra-query (database collection action)
  (let* ((skip (plist-get action :skip))
         (query (plist-get action :query))
         (limit (plist-get action :limit))
         (sort (plist-get action :sort)))
    `(mongo-menu/display ,database
                         :collection ,collection
                         :skip ,skip
                         :query ,query
                         :limit ,limit
                         :sort ,sort)))

(defun mongo-menu--build-hydra-queries (database collection)
  "Build and display a hydra proposing actions for the previously selected collection"
  (let* ((queries (mongo-menu--get-collection-property :queries database collection))
         (default-queries (list
                           `("1" (lambda () (interactive) (mongo-menu--action-show-documents-ivy ,database ,collection)) "All")))
         (queries-heads
          (mapcar
           (lambda (query)
             (let* ((key (plist-get query :key))
                    (name (plist-get query :name)))
               (list key (mongo-menu--build-hydra-query database collection query) name :exit t)))
           queries))
         (queries-heads (append (or default-queries (list)) queries-heads)))
    (eval `(defhydra mongo-menu--hydra-tmp (:color blue)
             "Mongo-menu"
             ,@queries-heads))
    (mongo-menu--hydra-tmp/body)))

(defun mongo-menu--build-hydra-collections (database)
  "Build and display a hydra proposing access to customized collections"
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

(provide 'mongo-menu-hydra)
