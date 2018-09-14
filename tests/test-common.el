;; -*- lexical-binding: t -*-

(setq collect--expected-databases
      '(("db1"
         :key "1"
         :type mongodb
         :host "host1"
         :user "user"
         :password "password"
         :collections
         (("collection1"
           :key "c"
           :name "collection1"
           :columns ((:name "_id" :width 30)
                     (:name "name" :with 50))
           :actions nil
           :sort "{name: 1}"
           :limit 15
           :queries nil)))))

(defun configure-test-common ()
  "Configure initial state"
  (collect/add-database
     "db1"
     :key "1"
     :type 'mongodb
     :host "host1"
     :user "user"
     :password "password")
    (collect/configure-collection
     "db1"
     "collection1"
     :key "c"
     :columns (list
               '(:name "_id" :width 30)
               '(:name "name" :with 50))
     :sort "{name: 1}"
     :limit 15))

(defun run-query-test-common (test)
  "Configure initial test and run test"
  (let (collect--databases (list))
    (configure-test-common)
    (funcall test)))

(ert-deftest test-common-configure ()
  "Test that configure a database and a collection populates the collect--databases correctly"
  (run-query-test-common
   (lambda ()
     (should (equal
              collect--databases
              collect--expected-databases)))))

(ert-deftest test-common-configure-database-twice ()
  "Test that configuring the same database twice will not create duplicates"
  (run-query-test-common
   (lambda ()
     (configure-test-common)
     (should (equal
              collect--databases
              collect--expected-databases)))))

(ert-deftest test-common-configure-collection-twice ()
  "Test that configuring the same collection twice will not create duplicates"
  ;; TODO: make it pass
  (run-query-test-common
   (lambda ()
     (collect/configure-collection
      "db1"
      "collection1"
      :key "c"
      :columns (list
                '(:name "_id" :width 30)
                '(:name "name" :with 50))
      :sort "{name: 1}"
      :limit 15)
     (should (equal
              collect--databases
              collect--expected-databases)))))


(ert-deftest test-common-get-database ()
  "Tests that database is correctly extracted from config"
  (run-query-test-common
   (lambda ()
     (should (equal
              (collect--get-database "db1")
              (car collect--expected-databases))))))

(ert-deftest test-common-get-collection ()
  "Tests that some database's collection is correctly extracted from config"
  (run-query-test-common
   (lambda ()
     (let ((columns (collect--get-collection-columns "db1" "collection1")))
       (should
        (equal
         columns
         (list
          '(:name "_id" :width 30)
          '(:name "name" :with 50))))))))

(ert-deftest test-get-collection-sort ()
  "Test that we get the manually configured sort setting value"
  (run-query-test-common
   (lambda ()
     (should (equal
              (collect--get-collection-sort "db1" "collection1")
              "{name: 1}")))))

(ert-deftest test-get-collection-limit ()
  "Test that we get the manually configured sort setting value"
  (run-query-test-common
   (lambda ()
     (should (equal
              (collect--get-collection-limit "db1" "collection1")
              15)))))
