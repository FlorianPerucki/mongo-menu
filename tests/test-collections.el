(defun run-collections-test (test)
  (let* ((mongo-menu-client "mongo")
         (mongo-menu-databases '(("db1" . ["ip1" "login1" "password1"])))
         (mongo-menu-current-database "db1")
         (mongo-menu-current-collection "collection1")
         (mongo-menu-collection-templates
          '(("db1" . (("collection1" . [("_id" 30 nil) ("field1" 50 nil)])
                      ("collection2" . [("_id" 30 nil)]))))))
    (funcall test)))

(ert-deftest test-mongo-menu-collections-list ()
  (run-collections-test
   (lambda ()
     (should (equal (mongo-menu-collections-list) (list "collection1" "collection2"))))))
