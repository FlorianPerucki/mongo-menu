(require 'el-mock)

(defun run-query-test (test)
  (let* ((mongo-menu-client "mongo")
         (mongo-menu-databases '(("db1" . ["ip1" "login1" "password1"])))
         (mongo-menu-current-database "db1"))
    (funcall test)))

(ert-deftest test-mongo-menu-requote-output ()
  "Tests that invalid JSON objects are correctly escaped"
  (should (string-equal
           (mongo-menu-requote-output "{\"_id\": ObjectId(\"1234\")}")
           "{\"_id\": \"ObjectId(\\\"1234\\\")\"}"))
  (should (string-equal
           (mongo-menu-requote-output "{\"date\": ISODate(\"1111\")}")
           "{\"date\": \"ISODate(\\\"1111\\\")\"}")))

(ert-deftest test-mongo-menu-get-database ()
  "Tests that database creds are correctly extracted from config"
  (run-query-test
   (lambda ()
     (should (equal
              (mongo-menu-get-database)
              ["ip1" "login1" "password1"])))))

(ert-deftest test-mongo-menu-raw-query ()
  "Tests that the final mongo shell command is correct"
  (run-query-test
   (lambda ()
     (with-mock
       (mock (shell-command-to-string *) => nil)
       (mock (shell-command-to-string "mongo -u login1 -p password1 ip1 --quiet --eval 'db.collection.find()'") => t)
       (should (mongo-menu-raw-query "db.collection.find()"))))))

(ert-deftest test-mongo-menu-json-query ()
  "Tests that JSON data is correctly extracted from the raw query output"
  (run-query-test
   (lambda ()
     (with-mock
       (mock (mongo-menu-raw-query *) => "{\"_id\": ObjectId(\"1234\"), \"name\": \"test\"}")
       (let ((data (mongo-menu-json-query "some query")))
         (should (string-equal (gethash "name" data) "test")))))))
