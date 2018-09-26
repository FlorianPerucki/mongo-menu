;;; collect-test.el --- Tests for collect -*- lexical-binding: t -*-

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

;;; Code:

(require 'generator)
(require 'subr-x)
(require 'json)
(require 'hydra)
(require 'el-mock)
(require 'ivy)
(require 'ert)
(load-file "collect-hydra.el")
(load-file "collect-mongodb.el")
(load-file "collect-ivy.el")
(load-file "collect-table.el")
(load-file "collect.el")


;; heavily inspired from https://github.com/abo-abo/swiper/blob/master/ivy-test.el

(defvar ivy-expr nil
  "Holds a test expression to evaluate with `ivy-eval'.")

(defvar ivy-result nil
  "Holds the eval result of `ivy-expr' by `ivy-eval'.")

(defun ivy-eval ()
  "Evaluate `ivy-expr'."
  (interactive)
  (setq ivy-result (eval ivy-expr)))

(defun ivy-with (expr keys)
  "Evaluate EXPR followed by KEYS."
  (let ((ivy-expr expr))
    (execute-kbd-macro
     (vconcat (kbd "C-c i")
              (kbd keys)))
    ivy-result))

(global-set-key (kbd "C-c i") 'ivy-eval)

(defun collect-hydra-keys (keys)
  "Run hydra-collect/body and then choose KEYS heads successively"
  (execute-kbd-macro
   (vconcat (kbd "C-c h")
            (kbd keys))))

(global-set-key (kbd "C-c h") 'hydra-collect/body)


(ert-deftest test-collect ()
  (collect-setup
   (collect-add-database
    :name "db1"
    :key "1"
    :type 'mongodb
    :host "host1"
    :user "user"
    :password "password")
   (collect-add-collection
    :database "db1"
    :name "collection1"
    :key "c"
    :columns '((:name "_id" :width 30)
               (:name "name" :width 50))
    :sort "name: 1"
    :limit 15)
   (collect-add-collection
    :database "db1"
    :name "collection2"
    :key "d"
    :columns '((:name "_id" :width 30)
               (:name "somefield" :width 50))
    :sort "_id: -1"
    :limit 10))

  (should (equal
           (length (collect--get-database-property :collections "db1"))
           2))
  (should (equal
           (collect--get-database-property :name "db1")
           "db1"))
  (should (equal
           (collect--get-database-property :key "db1")
           "1"))
  (should (equal
           (collect--get-database-property :type "db1")
           'mongodb))
  (should (equal
           (collect--get-database-property :host "db1")
           "host1"))
  (should (equal
           (collect--get-database-property :user "db1")
           "user"))
  (should (equal
           (collect--get-database-property :password "db1")
           "password"))
  (should (equal
           (collect--get-collection-property :name "db1" "collection1")
           "collection1"))
  (should (equal
           (collect--get-collection-property :key "db1" "collection1")
           "c"))
  (let ((columns (collect--get-collection-property :columns "db1" "collection1")))
    (progn
      (should (equal
               (plist-get (car columns) :name)
               "_id"))
      (should (equal
               (plist-get (car columns) :width)
               30))
      (should (equal
               (plist-get (cadr columns) :name)
               "name"))
      (should (equal
               (plist-get (cadr columns) :width)
               50))))
  (should (equal
           (collect--get-collection-property :sort "db1" "collection1")
           "name: 1"))
  (should (equal
           (collect--get-collection-property :limit "db1" "collection1")
           15)))

(ert-deftest test-collect-hydra ()
  (collect-setup
   (collect-add-database
    :name "db1"
    :key "1"
    :type 'mongodb
    :host "host1"
    :user "user"
    :password "password")
   (collect-add-collection
    :database "db1"
    :name "collection1"
    :key "c"
    :columns '((:name "_id" :width 30)
               (:name "name" :width 50))
    :sort "name: 1"
    :limit 15
    :heads '((:name "Foobar"
                    :key "p"
                    :query "\"foo\": \"bar\""
                    :sort "_id: -1"
                    :limit 100)))
   (collect-add-database
    :name "db2"
    :key "2"
    :type 'mongodb
    :host "host1"
    :user "user"
    :password "password"
    :hide t))

  ;; second database should be hidden
  (should (equal
           (length (collect--hydra-databases-heads))
           1))

  ;; running "Foobar" head
  (with-mock
    (stub collect--mongodb-raw-query => "[{\"_id\": 123, \"name\": \"foo\"}]")
    (mock (collect--ivy-read
           ;; prompt
           "db1 > collection1 > "
           ;; entries
           '(#("123                            foo                                               "
               0 81
               (:document-id "123" :collection "collection1" :database "db1")))
           ;; actions
           '(1
             ("RET" collect--ivy-action-show-document "Open in buffer")
             ("y" collect--ivy-action-copy-id "Copy row ID")))
          :times 1)
    (collect-hydra-keys "1 c p")))

(ert-deftest test-collect-ivy ()
  (collect-setup
   (collect-add-database
    :name "db1"
    :key "d"
    :type 'mongodb
    :host "host1"
    :user "user"
    :password "password")
   (collect-add-collection
    :database "db1"
    :name "collection1"
    :key "c"
    :columns '((:name "_id" :width 30)
               (:name "name" :width 50))
    :sort "name: 1"
    :limit 15
    :actions '((:name "Items"
                      :key "c"
                      :collection "collection2"
                      :foreign "item"
                      :query "\"some.flag\": true"
                      :sort "_id: -1"
                      :limit 100))))

  ;; test databases view
  (with-mock
    (stub collect--mongodb-raw-query => "[{\"_id\": \"123\", \"name\": \"foo\"}]")
    (mock (collect--ivy-read
           ;; prompt
           "> "
           ;; entries
           '(#("db1                                                host1                                             "
               0 101
               (:database "db1")))
           ;; actions
           '(1 ("RET" collect--ivy-show-collections-defined "Show collections")
               ("O" collect--ivy-show-collections "Show all collections")))
          :times 1)
    (collect-show-databases))

  ;; test collections view
  (with-mock
    (stub collect--mongodb-raw-query => "[{\"_id\": \"123\", \"name\": \"foo\"}]")
    (mock (collect--ivy-read
           ;; prompt
           "db1 > "
           ;; entries
           '(#("collection1" 0 11 (:collection "collection1" :database "db1")))
           ;; actions
           '(1 ("RET" collect--ivy-action-show-documents "Show documents")))
          :times 1)

    (collect-show-collections "db1" t))

  ;; test documents view
  (let ((collection-entry #("collection1" 0 11 (:collection "collection1" :database "db1"))))
    (with-mock
      (stub collect--mongodb-raw-query => "[{\"_id\": \"123\", \"name\": \"foo\"}]")
      (mock (collect--display
             :database "db1"
             :collection "collection1"
             :entries '(#("123                            foo                                               "
                          0 81
                          (:document-id "123" :collection "collection1" :database "db1"))))
            :times 1)
      (funcall (collect--get-front-function "action-show-documents" "db1" "collection1") collection-entry)
      ))

  ;; test ivy action
  (with-mock
    (stub collect--mongodb-raw-query => "[{\"_id\": \"123\", \"name\": \"foo\"}]")
    (mock (collect--display :database "db1"
                     :collection "collection2"
                     :entries '(#("123                           " 0 30
                                  ;; we did not register collection2 so we only display the document-id column
                                  (:fields (("_id" :name "_id" :hide nil :raw-value "123" :value "123                           "))
                                           :document-id "123" :collection "collection2" :database "db1"))))
          :times 1)

    (should (equal (ivy-with `(ivy-read
                               ;; prompt
                               "db1 > collection2 > "
                               ;; entries
                               '(#("123                           " 0 30
                                   (:fields (("_id"
                                              :name "_id"
                                              :hide nil
                                              :raw-value "123"
                                              :value "123                           "))
                                            :document-id "123"
                                            :collection "collection2"
                                            :database "db1")))
                               ;; actions
                               :action
                               `(1
                                 ,(collect--build-action "db1" "collection1"
                                                         '(:name "Items"
                                                                 :key "c"
                                                                 :collection "collection2"
                                                                 :foreign "item"
                                                                 :query "\"some.flag\": true"
                                                                 :sort "_id: -1"
                                                                 :limit 100))))
                             "M-o c")
                   #("123                           " 0 30
                     '(
                      :fields (("_id" :name "_id" :hide nil :raw-value "123" :value "123                           "))
                      :document-id "123" :collection "collection2" :database "db1")))))

  (should (equal
           (mapcar 'car (collect--get-actions "db1" "collection1"))
           (list "RET" "y" "c"))))

(ert-deftest test-collect-mongodb ()
  (collect-setup
   (collect-add-database
    :name "db1"
    :key "1"
    :type 'mongodb
    :host "host1"
    :user "user"
    :password "password")
   (collect-add-collection
    :database "db1"
    :name "collection1"
    :key "c"
    :columns '((:name "_id" :width 30)
               (:name "name" :width 50))
    :sort "name: 1"
    :limit 15))

  (should (equal
           (collect--mongodb-get-collection-projection "db1" "collection1")
           "\"_id\": 1, \"name\": 1"))

  (should (equal
           (collect--build-select-query "db1" "collection1")
           "db.collection1.find({}, {\"_id\": 1, \"name\": 1}).sort({name: 1}).skip(0).limit(15)"))

  (should (equal
           (collect--mongodb-compose-query "db1" "collection1" :document-id "123")
           "\"_id\": ObjectId(\"123\")"))

  (should (equal
           (collect--mongodb-compose-query "db1" "collection1" :query "field1: 1")
           "field1: 1"))

  (should (equal
           (collect--mongodb-compose-query "db1" "collection1"
                                           :query "field1: 1"
                                           :document-id "12345")
           "field1: 1, \"_id\": ObjectId(\"12345\")"))

  (should (equal
           (collect--mongodb-compose-query "db1" "collection1"
                                           :document-id "123"
                                           :foreign-key "foo")
           "\"foo\": ObjectId(\"123\")"))

  (should (equal
           (collect--mongodb-compose-query "db1" "collection1"
                                           :query "field1: 1"
                                           :document-id "12345"
                                           :foreign-key "foo.bar")
           "field1: 1, \"foo.bar\": ObjectId(\"12345\")"))

  (with-mock
    (stub collect--mongodb-raw-query => "[{\"_id\": ObjectId(\"12345\"), \"name\": \"foo\", \"nested\": {\"field\": \"bar\"}}]")

    (let ((row (car (collect--mongodb-json-query "db1" ""))))
      (should (equal
               (collect--mongodb-get-document-field "_id" row)
               "ObjectId(\"12345\")"))
      (should (equal
               (collect--mongodb-get-document-field "name" row)
               "foo"))
      (should (equal
               (collect--mongodb-get-document-field "nested.field" row)
               "bar"))
      (should (equal
               (collect--mongodb-extract-data-document "db1" "collection1" row)
               '("12345" "12345" "foo"))))))

(ert-deftest test-collect-table ()

  ;; test case 1: default display for database, table display for collection
  (collect-setup
   (collect-add-database
    :name "db1"
    :key "d"
    :type 'mongodb
    :host "host1"
    :user "user"
    :password "password")
   (collect-add-collection
    :database "db1"
    :name "collection1"
    :key "c"
    :display 'table
    :columns '((:name "_id" :width 30)
               (:name "name" :alias "other-name" :width 50))
    :sort "name: 1"
    :limit 15
    :actions '((:name "Items"
                      :key "c"
                      :collection "collection2"
                      :foreign "item"
                      :query "\"some.flag\": true"
                      :sort "_id: -1"
                      :limit 100)
               (:name "Items2"
                      :key "d"
                      :collection "collection2"
                      :from "name"
                      :sort "_id: -1"
                      :limit 100))))

  (global-set-key (kbd "C-c c") 'hydra-collect/body)

  ;; test databases view
  (with-mock
    (stub collect--mongodb-raw-query => "[{\"_id\": \"123\", \"name\": \"foo\"}]")
    (mock (collect--ivy-read
           ;; prompt
           "> "
           ;; entries
           '(#("db1                                                host1                                             "
               0 101
               (:database "db1")))
           ;; actions
           '(1 ("RET" collect--ivy-show-collections-defined "Show collections")
               ("O" collect--ivy-show-collections "Show all collections")))
          :times 1)
    (collect-show-databases))

  ;; test collections view
  (with-mock
    (stub collect--mongodb-raw-query => "[{\"_id\": \"123\", \"name\": \"foo\"}]")
    (mock (collect--ivy-read
           ;; prompt
           "db1 > "
           ;; entries
           '(#("collection1" 0 11 (:collection "collection1" :database "db1")))
           ;; actions
           '(1 ("RET" collect--ivy-action-show-documents "Show documents")))
          :times 1)
    (collect-show-collections "db1" t))

  (should (equal
           (mapcar 'car (collect--get-actions "db1" "collection1"))
           (list "RET" "y" "c" "d")))

  ;; test running an action on a table row
  (with-mock
    (stub collect--mongodb-raw-query => "[{\"_id\": \"123\", \"name\": \"foo\"}]")

    ;; simulate getting tabulated-list-mode current selection
    (stub tabulated-list-get-id =>
          #("123" 0 3
            (:fields (("_id" :name "_id" :hide nil :raw-value "123" :value "123")
                      ("name" :name "name" :hide nil :raw-value "foo" :value "foo"))
                     :document-id "123"
                     :collection "collection1"
                     :database "db1"))
          ["123" "foo"])

    ;; display table
    (collect-display :database "db1"
                     :collection "collection1"
                     :document-id "123")

    (mock (collect--ivy-read
           ;; prompt
           "db1 > collection2 > "
           ;; entries
           '(#("123                           " 0 30
               (:fields (("_id"
                          :name "_id"
                          :hide nil
                          :raw-value "123"
                          :value "123                           "))
                        :document-id "123"
                        :collection "collection2"
                        :database "db1")))
           ;; actions
           '(1
             ("RET" collect--ivy-action-show-document "Open in buffer")
             ("y" collect--ivy-action-copy-id "Copy row ID")))
          :times 1)

    ;; press command key
    (execute-kbd-macro (vconcat (kbd "c")))

    ;; test the :from keyword
    (mock (collect--mongodb-raw-query
           "db1"
           "var cursor = db.collection2.find({\"_id\": ObjectId(\"foo\")}, {\"_id\": 1}).sort({_id: -1}).skip(0).limit(100); print(\"[\"); while(cursor.hasNext()) { printjson(cursor.next()); if (cursor.hasNext()) {print(\",\");}} print(\"]\");")
          => "[{\"_id\": \"foo\", \"name\": \"bar\"}]")

    (mock (collect--ivy-read
           ;; prompt
           "db1 > collection2 > "
           ;; entries
           '(#("foo                           " 0 30
               (:fields (("_id"
                          :name "_id"
                          :hide nil
                          :raw-value "foo"
                          :value "foo                           "))
                        :document-id "foo"
                        :collection "collection2"
                        :database "db1")))
           ;; actions
           '(1
             ("RET" collect--ivy-action-show-document "Open in buffer")
             ("y" collect--ivy-action-copy-id "Copy row ID")))
          :times 1)

    ;; press command key
    (execute-kbd-macro (vconcat (kbd "d")))))
;; collect-test.el ends here
