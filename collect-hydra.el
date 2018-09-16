;;; collect.el --- Read quickly from multiple databases -*- lexical-binding: t -*-

;; Copyright (C) 2018-2019  Free Software Foundation, Inc.

;; Author: Florian Perucki <florian@perucki.fr>
;; URL: https://github.com/florianperucki/collect
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.1") (ivy "0.10.0") (hydra 0.14.0))
;; Keywords: database mongodb sql

;; This file is part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This package provide helper functions to generate `hydra' heads
;; according to the user-specified database configurations.

;;; Code:

(defun collect--build-hydras ()
  "Build all hydras from user-defined database configurations."
  (let ((database-heads
         (mapcar (lambda (x)
                   (list (plist-get (cdr x) :key) `(collect--hydra-build-collections ,(car x)) (car x) :exit t))
                 collect--databases)))
    (eval `(defhydra hydra-collect (:color blue)
             "Collect"
             ,@database-heads))))

(defun collect--hydra-build-collections (database)
  "Build and display a hydra proposing access to customized collections"
  (let* ((collections (collect--get-collection-names database t))
         (collection-heads
          (mapcar
           (lambda (collection)
             (let ((key (collect--get-collection-property :key database collection)))
               (list key `(collect--hydra-build-queries ,database ,collection) collection :exit t)))
           collections)))
    (eval `(defhydra collect--hydra-tmp (:color blue)
             "Collect"
             ,@collection-heads))
    (collect--hydra-tmp/body)))

(defun collect--hydra-build-queries (database collection)
  "Build and display a hydra proposing actions for the previously selected collection"
  (let* ((queries (mapcar 'cdr (collect--get-collection-property :queries database collection)))
         (default-queries (list
                           `("1" (lambda () (interactive) (collect--action-show-documents-ivy ,database ,collection)) "All")))
         (queries-heads
          (mapcar
           (lambda (query)
             (let* ((key (plist-get query :key))
                    (name (plist-get query :name)))
               (list key `(collect-collection-query ,database ,collection ,key) name :exit t)))
           queries))
         (queries-heads (append (or default-queries (list)) queries-heads)))
    (eval `(defhydra collect--hydra-tmp (:color blue)
             "Collect"
             ,@queries-heads))
    (collect--hydra-tmp/body)))

(provide 'collect-hydra)
;;; collect-hydra.el ends here
