(require 'el-mock)

(defun run-collection-test (test)
  (let* ((mongo-menu-client "mongo")
         (mongo-menu-databases '(("db1" . ["ip1" "login1" "password1"])))
         (mongo-menu-current-database "db1")
         (mongo-menu-current-collection "collection1")
         (mongo-menu-collection-templates
          '(("db1" . (("collection1" . [("_id" 30 nil) ("field1" 50 nil)])
                      ("collection2" . [("_id" 30 nil)]))))))
    (funcall test)))

(ert-deftest test-mongo-menu-get-current-template ()
  "Tests that the correct column template is returned"
  (run-collection-test
   (lambda ()
     (should (equal (mongo-menu-get-current-template) [("_id" 30 nil) ("field1" 50 nil)]))))
  )

(ert-deftest test-mongo-menu-collection-get-projection ()
  "Tests that the correct column projection is returned"
  (run-collection-test
   (lambda ()
     (should (string-equal (mongo-menu-collection-get-projection) "{\"_id\": 1, \"field1\": 1}"))))
  )

(ert-deftest test-mongo-menu-collection-list ()
  "Tests the whole process from the raw mongo query to the processed data ready for tabulated-list-mode"
  (run-collection-test
   (lambda ()
     (let ((query "var cursor = db.collection1.find({}, {}).sort({_id: -1}).skip(0).limit(10); print(\"[\"); while(cursor.hasNext()) { printjson(cursor.next()); if (cursor.hasNext()) {print(\",\");}} print(\"]\");"))
       (with-mock
         (mock (mongo-menu-raw-query *) => "error")
         (mock (mongo-menu-raw-query query) => "[{\"_id\": \"A\", \"field1\": 1}, {\"_id\": \"B\", \"field1\": 2}]")
         (let ((data (mongo-menu-collection-list t)))
           ;; FIXME: field1 should be an integer
           (should (equal data (list (list "A" ["A" "1"]) (list "B" ["B" "2"]))))))))))
