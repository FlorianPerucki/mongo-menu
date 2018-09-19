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
                              '("o" collect--ivy-show-collections-defined "Show collections")
                              '("O" collect--ivy-show-collections "Show all collections"))))

(defun collect--ivy-collections (database entries)
  "Display collections list for a given database.
database: string
entries: collections plist"
  (collect--display :database database
                    :entries (mapcar (apply-partially 'collect--ivy-format-entry-collection database) entries)))

(defun collect--ivy-get-prompt (&optional database collection)
  "Return prompt for the document type currently being displayed:
collections if collection is nil, documents if collection is non-nil"
  (if collection
      (format "%s > %s > " database collection)
    (if database
        (format "%s > " database)
      "> ")))

(defun collect--ivy-get-actions (&optional database collection)
  "Return actions for the row type currently being displayed
collections if collection is nil, documents if collection is non-nil"
  (if collection
      ;; actions on a document row
      (let ((default-actions (list
                              '("o" collect--show-document "Open in buffer")
                              '("y" collect--action-copy-id "Copy row ID")))
            (actions (mapcar 'collect--ivy-build-action (collect--get-collection-property :actions database collection))))
        (append default-actions (or actions (list))))

    ;; actions on a collection row
    (if database (let ((default-actions '(("o" collect--ivy-action-show-documents-entries "Show documents")))
                       (actions (mapcar 'collect--ivy-build-action (collect--get-database-property :actions database))))
                   (append default-actions (or actions (list))))

      ;; actions on a database row
      (list))
    ))


;; internals

(defun collect--ivy-format-entry-database (entry)
  "Return a formatted string for a database row.
entry: plist

The returned string has the following property:
database: string name of the database"
  (let* ((name (car entry))
         (host (collect--get-property :host entry)))
    (propertize
     (format "%1$#-50s %2$#-50s" (s-truncate 50 name) (s-truncate 50 host))
     :database name)))

(defun collect--ivy-format-entry-collection (database entry)
  "Return the collection row string 'entry' with the following properties:
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
                  :id id))))

(defun collect--ivy-show-collections-defined (entry)
  "Show a list of user-defined collections"
  (collect--ivy-show-collections entry t))

(defun collect--ivy-show-collections (entry &optional defined)
  "Show a list of collections.
If defined is non-nil, not database fetch is done and only user-defined collections are displayed."
  (let ((database (get-text-property 0 :database entry)))
    (collect-show-collections database defined)))

(defun collect--ivy-action-show-documents-entries (entry &optional skip query)
  "Action to execute on a collection row: query and display a list of documents"
  (let* ((database (get-text-property 0 :database entry))
         (collection (get-text-property 0 :collection entry)))
    (collect--ivy-action-show-documents database collection skip query))) ; TODO limit + sort

(defun collect--ivy-build-action (action)
  (let* ((key (plist-get action :key))
         (name (plist-get action :name)))
    (list key (apply-partially 'collect--ivy-run-action action) name)))

(defun collect--ivy-run-action (action entry)
  "Execute an ACTION on a single document ENTRY.
The executed action depends on the ACTION type property:

read: execute a read operation"
  (let* ((action-type (or (plist-get action :type) 'read))
         ;; target database for action, defaults to current
         (database (or
                    (plist-get action :database)
                    (get-text-property 0 :database entry)))
         ;; target collection for action, defaults to current
         (collection (or
                      (plist-get action :collection)
                      (get-text-property 0 :collection entry)))
         ;; document unique id
         (document-id (get-text-property 0 :id entry))
         ;; if provided, this is the field name to use instead of the
         ;; database default unique id field
         (foreign-key (plist-get action :foreign))
         ;; if t, the query returns a single document that we'll show in a new buffer
         (single (plist-get action :single))
         ;; usual query parameters
         (sort (plist-get action :sort))
         (limit (plist-get action :limit))
         (skip (plist-get action :skip))
         (projection (plist-get action :projection))
         (query (plist-get action :query)))
    (if (equal action-type 'read)
        (collect-display :database database
                         :collection collection
                         :projection projection
                         :skip skip
                         :query query
                         :limit limit
                         :sort sort
                         :foreign-key foreign-key
                         :document-id document-id
                         :single single)
      (error "Unknown action type %S" action-type))))

(defun collect--ivy-action-show-documents (database collection &optional skip query limit sort)
  "Fetch and display collection's data, filtered by QUERY, SKIP, LIMIT and/or SORT if provided"
  (interactive)
  (let* ((documents (collect--build-and-run-select-query database collection skip query limit sort))
         (entries (mapcar (apply-partially 'collect--ivy-format-entry-document database collection) documents)))
    (collect--display :database database
                      :collection collection
                      :entries entries)))

(provide 'collect-ivy)
;;; collect-ivy.el ends here
