(ert-deftest test-mongo-menu-document-get-field ()
  "Tests document field getter function"
  (let ((document #s(hash-table
                     test equal
                     data (
                           "name" "test"
                           "settings" #s(hash-table
                                         test equal
                                         data (
                                               "color" "red"
                                               "font" #s(hash-table
                                                         test equal
                                                         data("weight" "bold"))))))))

    (should (string-equal (mongo-menu-document-get-field "name" document) "test"))
    (should (string-equal (mongo-menu-document-get-field "settings.color" document) "red"))
    (should (string-equal (mongo-menu-document-get-field "settings.font.weight" document) "bold"))
    )
  )
