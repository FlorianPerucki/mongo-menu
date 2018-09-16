;; -*- lexical-binding: t -*-


(defun run-query-test (test)
  (let (collect--databases (list))
    (collect-add-database
     :name "db1"
     :type 'mongodb
     :host "host1"
     :user "user"
     :password "password")
    (collect-configure-collection
     :database "db1"
     :name "collection1"
     :columns (list
               '(:name "_id" :width 30)
               '(:name "name" :with 50)
               '(:name "nested.field" :with 50))
     ;; :actions (list
     ;;           '(1
     ;;             :key "a"
     ;;             :text "adSettings"
     ;;             :type 'query
     ;;             :projection (list "adSettings")))
     )
    (funcall test)))

(ert-deftest test-collect--requote-output ()
  "Test that invalid JSON objects are correctly escaped"
  (should (string-equal
           (collect--mongodb-requote-output "{\"_id\": ObjectId(\"1234\")}")
           "{\"_id\": \"ObjectId(\\\"1234\\\")\"}"))
  (should (string-equal
           (collect--mongodb-requote-output "{\"date\": ISODate(\"1111\")}")
           "{\"date\": \"ISODate(\\\"1111\\\")\"}")))

(ert-deftest test-get-collection-projection ()
  "Test the projection format according to column settings"
  (run-query-test
   (lambda ()
     (should (equal
              (collect--mongodb-get-collection-projection "db1" "collection1")
              "\"_id\": 1, \"name\": 1, \"nested.field\": 1")))))

(ert-deftest test-build-select-query-mongodb ()
  (run-query-test
   (lambda ()
     (should (equal
              (collect--mongodb-build-select-query
               "collection1"
               (collect--mongodb-get-collection-projection "db1" "collection1"))
              "db.collection1.find({}, {\"_id\": 1, \"name\": 1, \"nested.field\": 1}).sort({}).skip(0).limit(10)"))
     (should (equal
              (collect--mongodb-build-select-query
               "collection1"
               (collect--mongodb-get-collection-projection "db1" "collection1")
               10
               "_id: -1"
               30
               "name: \"foo\"")
              "db.collection1.find({name: \"foo\"}, {\"_id\": 1, \"name\": 1, \"nested.field\": 1}).sort({_id: -1}).skip(10).limit(30)")))))

(ert-deftest test-extract-data-documents-mongodb ()
  "Test that we correctly extract column values from a (json) hash-table"
  (run-query-test
   (lambda ()
     (should (equal
              (collect--mongodb-extract-data-documents
               "db1"
               "collection1"
               (list #s(hash-table
                        test equal
                        data (
                              "_id" "ObjectId(\"5b8e7cf5580ad8003a68a4ed\")"
                              "name" "foo"
                              "nested" #s(hash-table
                                         test equal
                                         data ("field" "wassup"))))))
              (list '("5b8e7cf5580ad8003a68a4ed" "5b8e7cf5580ad8003a68a4ed" "foo" "wassup")))))))
