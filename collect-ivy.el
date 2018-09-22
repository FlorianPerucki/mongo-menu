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
;; This package adds the `ivy' support to `collect'.
;; To use ivy configure the following variable from collect.el:
;;
;; (setq collect-selector 'ivy)
;;
;; ivy is used by default.

;;; Code:

;; required functions implementation for a front support

(defun collect--ivy-display (database collection entries actions)
  (let* ((custom-actions (collect--get-actions database collection))
         (actions (append (or actions (list)) custom-actions))
         (prompt (collect--ivy-get-prompt database collection)))
    (collect--ivy-read prompt entries (cons 1 actions))))

(defun collect--ivy-read (prompt entries actions)
  "Run front-end command to display results"
  (ivy-read prompt
            entries
            :action actions))

(defun collect--ivy-databases (entries)
  "Display database list.
entries: string list"
  (collect--display :entries (mapcar 'collect--ivy-format-entry-database entries)
                    :actions (list
                              '("RET" collect--ivy-show-collections-defined "Show collections")
                              '("O" collect--ivy-show-collections "Show all collections"))))

(defun collect--ivy-collections (database entries)
  "Display collections list for a given database.
database: string
entries: collections plist"
  (collect--display :database database
                    :entries (mapcar (apply-partially 'collect--ivy-format-entry-collection database) entries)))

(defun collect--ivy-action-show-document (row)
  "Action to execute on any row: show a single document"
  (interactive)
  (let* ((database (get-text-property 0 :database row))
         (collection (get-text-property 0 :collection row))
         (document-id (get-text-property 0 :document-id row)))
    (collect--show-document database collection document-id)))

(defun collect--ivy-action-show-documents (row)
  "Action to execute on a collection row: query and display a list of documents"
  (let* ((database (get-text-property 0 :database row))
         (collection (get-text-property 0 :collection row)))
    (collect--show-documents database collection)))

(defun collect--action-ivy-copy-id (row)
  "Action to execute on any row: add the row ID to the kill ring"
  (kill-new (get-text-property 0 :document-id row)))

(defun collect--ivy-get-prompt (&optional database collection)
  "Return prompt for the document type currently being displayed:
collections if collection is nil, documents if collection is non-nil"
  (if collection
      (format "%s > %s > " database collection)
    (if database
        (format "%s > " database)
      "> ")))

(defun collect--ivy-get-current-entry (&optional entry)
  (let* ((database (get-text-property 0 :database entry))
         ;; target collection for action, defaults to current
         (collection (get-text-property 0 :collection entry))
         ;; document unique id
         (document-id (get-text-property 0 :document-id entry)))
    (list
     :database database
     :collection collection
     :document-id document-id)))

;; internals

(defun collect--ivy-format-entry-database (entry)
  "Return a formatted string for a database row.
ENTRY: plist

The returned string has the following property:
database: string name of the database"
  (let* ((name (car entry))
         (host (collect--get-property :host entry)))
    (propertize
     (format "%1$#-50s %2$#-50s" (s-truncate 50 name) (s-truncate 50 host))
     :database name)))

(defun collect--ivy-format-entry-collection (database entry)
  "Return the collection row string ENTRY with the following properties:
database: string name of the database
collection: string name of the collection"
  (propertize
   entry
   :database database
   :collection entry))

(iter-defun collect--ivy-format-document-fields (database collection values)
  "Generate a formatted string for each field values of a single row.
database: string
collection: string
values: list of values (string, integer, etc.)"
  (let* ((collect--tmp-index 0)
         (columns (collect--get-collection-columns database collection))
         (len (length values)))
    (when (not (equal len (length columns)))
      (error "Column templates and values mismatch"))
    (while (< collect--tmp-index len)
      (let* ((value (elt values collect--tmp-index))
             (column (elt columns collect--tmp-index))
             (value (collect--get-column-value database collection column value))
             (width (collect--get-column-width database collection column)))
        (iter-yield (format
                     (format "%%1$#-%ss" width) ; build the format we want before using it
                     (s-truncate width value))))
      (setq collect--tmp-index (1+ collect--tmp-index)))))

(defun collect--ivy-format-entry-document (database collection entry)
  "Return a string for a single document row.

The returned string has the following properties:
database: string name of the database
collection: string name of the collection
id: unique row identifier"
  (let ((values (list)))
    (iter-do (value (collect--ivy-format-document-fields database
                                                         collection
                                                         (cdr entry)))
      (setq values (append values (list value))))
    (let* ((id (car entry))
           (output (string-join values " ")))
      (propertize output
                  :database database
                  :collection collection
                  :document-id id))))

(defun collect--ivy-show-collections-defined (entry)
  "Show a list of user-defined collections"
  (collect--ivy-show-collections entry t))

(defun collect--ivy-show-collections (entry &optional defined)
  "Show a list of collections.
If defined is non-nil, no database fetch is done and only user-defined collections are displayed."
  (let ((database (get-text-property 0 :database entry)))
    (collect-show-collections database defined)))


(provide 'collect-ivy)
;;; collect-ivy.el ends here
