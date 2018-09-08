;; -*- lexical-binding: t -*-


(defun run-query-test (test)
  (let (mongo-menu--databases (list))
    (mongo-menu/add-database
     "db1"
     :type 'mongodb
     :host "host1"
     :user "user"
     :password "password")
    (mongo-menu/configure-collection
     "db1"
     "collection1"
     :columns (list
               '(:name "_id" :width 30)
               '(:name "name" :with 50))
     :sort "{name: 1}"
     :limit 15
     ;; :actions (list
     ;;           '(1
     ;;             :key "a"
     ;;             :text "adSettings"
     ;;             :type 'query
     ;;             :projection (list "adSettings")))
     )
    (funcall test)))


(ert-deftest test-common-configure ()
  "Test that configure a database and a collection populates the mongo-menu--databases correctly"
  (run-query-test
   (lambda ()
     (should (equal
              mongo-menu--databases
              (list
               '("db1" :type mongodb :host "host1" :user "user" :password "password" :collections
                 ((#1="collection1" :name #1# :columns
                      ((:name "_id" :width 30)
                       (:name "name" :with 50))
                      :actions nil)))))))))

(ert-deftest test-mongo-menu-get-database ()
  "Tests that database is correctly extracted from config"
  (run-query-test
   (lambda ()
     (should (equal
              (mongo-menu--get-database "db1")
              '(:type mongodb
                      :host "host1"
                      :user "user"
                      :password "password"
                      :collections (("collection1"
                                     :name "collection1"
                                     :columns ((:name "_id" :width 30)
                                               (:name "name" :with 50))
                                     :actions nil))))))))

(ert-deftest test-common-get-collection ()
  "Tests that some database's collection is correctly extracted from config"
  (run-query-test
   (lambda ()
     (let ((columns (mongo-menu--get-collection-columns "db1" "collection1")))
       (should
        (equal
         columns
         (list
          '(:name "_id" :width 30)
          '(:name "name" :with 50))))))))

(ert-deftest test-get-collection-sort ()
  "Test that we get the manually configured sort setting value"
  (run-query-test
   (lambda ()
     (should (equal
              (mongo-menu--get-collection-sort "db1" "collection1")
              "{name: 1}")))))

(ert-deftest test-get-collection-limit ()
  "Test that we get the manually configured sort setting value"
  (run-query-test
   (lambda ()
     (should (equal
              (mongo-menu--get-collection-limit "db1" "collection1")
              15)))))
