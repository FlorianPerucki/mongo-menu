;;; collect.el --- Read quickly from multiple databases -*- lexical-binding: t -*-

;; Copyright (C) 2018 Florian Perucki

;; Author: Florian Perucki <florian@perucki.fr>
;; URL: https://github.com/florianperucki/collect
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.1") (ivy "0.10.0") (hydra 0.14.0))
;; Keywords: database mongodb sql

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This package provide helper functions to generate `hydra' heads
;; according to the user-specified database configurations.

;;; Code:

(defun collect--build-hydras ()
  "Build all hydras from user-defined database configurations."
  (let ((heads (collect--hydra-databases-heads)))
    (eval `(defhydra hydra-collect (:color blue)
             "Collect"
             ,@heads))))

(defun collect--hydra-databases-heads ()
  (mapcar (lambda (x)
            (list (plist-get (cdr x) :key) `(collect--hydra-build-collections ,(car x)) (car x) :exit t))
          (-filter (lambda (database)
                     (not (plist-get (cdr database) :hide)))
                   collect--databases)))

(defun collect--hydra-collections-heads (database)
  (let ((collections (collect--get-collection-names database t)))
    (mapcar
     (lambda (collection)
       (let ((key (collect--get-collection-property :key database collection)))
         (list key `(collect--hydra-build-queries ,database ,collection) collection :exit t)))
     collections)))

(defun collect--hydra-build-collections (database)
  "Build and display a hydra proposing access to customized collections"
  (let ((heads (collect--hydra-collections-heads database)))
    (eval `(defhydra collect--hydra-tmp (:color blue)
             "Collect"
             ,@heads))
    (collect--hydra-tmp/body)))

(defun collect--hydra-build-queries (database collection)
  "Build and display a hydra proposing actions for the previously selected collection"
  (let* ((queries (mapcar 'cdr (collect--get-collection-property :queries database collection)))
         (default-queries (list
                           `("1" (lambda () (interactive) (collect--ivy-action-show-documents ,database ,collection)) "All")))
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
