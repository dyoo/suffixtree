(module test-longest-common-substring mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2))
           (planet "text-ui.ss" ("schematics" "schemeunit.plt" 2))
           "longest-common-substring.ss")
  
  (define longest-common-substring-tests
    (test-suite
     "test-longest-common-substring tests"
     (test-equal?
      "substring test"
      (longest-common-substring "01001" "1100101") "1001")
     
     (test-equal?
      "simple test 1"
      (longest-common-substring "0" "0") "0")
     
     (test-equal?
      "simple test 2"
      (longest-common-substring "01" "0") "0")
     
     (test-equal?
      "simple test 3"
      (longest-common-substring "0" "01") "0")
     
     (test-equal?
      "empty case"
      (longest-common-substring "" "") "")
     
     (test-equal?
      "another empty case"
      (longest-common-substring "1" "0") "")
     
     (test-equal?
      "comparing three strings"
      (longest-common-substring "1010101" "11101" "00000100000")
      "01")))
  
  (test/text-ui longest-common-substring-tests))