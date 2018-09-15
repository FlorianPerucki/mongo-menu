;;; -*- lexical-binding: t; -*-

(defun collect--build-hydra-query (database collection action)
  (let* ((skip (plist-get action :skip))
         (query (plist-get action :query))
         (limit (plist-get action :limit))
         (sort (plist-get action :sort)))
    `(collect/display ,database
                         :collection ,collection
                         :skip ,skip
                         :query ,query
                         :limit ,limit
                         :sort ,sort)))

(defun collect--build-hydra-queries (database collection)
  "Build and display a hydra proposing actions for the previously selected collection"
  (let* ((queries (collect--get-collection-property :queries database collection))
         (default-queries (list
                           `("1" (lambda () (interactive) (collect--action-show-documents-ivy ,database ,collection)) "All")))
         (queries-heads
          (mapcar
           (lambda (query)
             (let* ((key (plist-get query :key))
                    (name (plist-get query :name)))
               (list key (collect--build-hydra-query database collection query) name :exit t)))
           queries))
         (queries-heads (append (or default-queries (list)) queries-heads)))
    (eval `(defhydra collect--hydra-tmp (:color blue)
             "Collect"
             ,@queries-heads))
    (collect--hydra-tmp/body)))

(defun collect--build-hydra-collections (database)
  "Build and display a hydra proposing access to customized collections"
  (let* ((collections (collect--get-collection-names database t))
         (collection-heads
          (mapcar
           (lambda (collection)
             (let ((key (collect--get-collection-property :key database collection)))
               (list key `(collect--build-hydra-queries ,database ,collection) collection :exit t)))
           collections)))
    (eval `(defhydra collect--hydra-tmp (:color blue)
             "Collect"
             ,@collection-heads))
    (collect--hydra-tmp/body)))

(defun collect--build-hydras ()
  (let ((database-heads
         (mapcar (lambda (x)
                   (list (plist-get (cdr x) :key) `(collect--build-hydra-collections ,(car x)) (car x) :exit t))
                 collect--databases)))
    (eval `(defhydra hydra-collect (:color blue)
             "Collect"
             ,@database-heads))))

(provide 'collect-hydra)
