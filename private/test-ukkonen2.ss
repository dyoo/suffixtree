(module test-ukkonen2 mzscheme
  (require "test-common-definitions.ss")
  (require "ukkonen2.ss")
  
  
  (define ukkonen-test-suite
    (make-test-suite
     "ukkonen test suite"
     
     (make-test-case
      "empty construction"
      (let-values (((root tree) (root-and-tree)))
        (assert-equal? (list) (node-children root))
        (assert-label-equal? (make-label "") (node-up-label root))
        (assert-eq? #f (node-suffix-link root))))
     
     
     (make-test-case
      "adding a leaf."
      (let-values (((root tree) (root-and-tree)))
        (node-add-leaf! root (make-label "bookkeeper"))
        (assert-equal? 1 (length (node-children root)))))
     
     
     (make-test-case
      "tree-contains? #t on empty tree"
      (let-values (((root tree) (root-and-tree)))
        (assert-true (tree-contains? tree (make-label "")))))
     
     
     (make-test-case
      "tree-contains? #f on empty tree"
      (let-values (((root tree) (root-and-tree)))
        (assert-false (tree-contains? tree (make-label "munging")))))
     
     
     (make-test-case
      "tree-contains? on whole tree"
      (let-values (((root tree) (root-and-tree)))
        (node-add-leaf! root (make-label "fezzick"))
        (assert-true (tree-contains? tree (make-label "fezzick")))))
     
     
     (make-test-case
      "tree-contains? on prefix"
      (let-values (((root tree) (root-and-tree)))
        (node-add-leaf! root (make-label "wesley"))
        (assert-true (tree-contains? tree (make-label "wes")))))
     
     
     (make-test-case
      "tree-contains? failing"
      (let-values (((root tree) (root-and-tree)))
        (node-add-leaf! root (make-label "inigo"))
        (assert-false (tree-contains? tree (make-label "montoya")))))
     
     
     (make-test-case
      "tree-contains? passing through two nodes"
      (let-values (((root tree) (root-and-tree)))
        (node-up-split!
         (node-add-leaf! root (make-label "inconcievable")) 5)
        (assert-true (tree-contains? tree (make-label "inconcievable")))))
     
     
     (make-test-case
      "adding two leaves."
      (let-values (((root tree) (root-and-tree)))
        (node-add-leaf! root (make-label "bookkeeper"))
        (node-add-leaf! root (make-label "ookkeeper"))
        (assert-equal? 2 (length (node-children root)))))
     
     
     (make-test-case
      "finding children"
      (let-values (((root tree) (root-and-tree)))
        (let ((leaf-1 (node-add-leaf! root (make-label "blah")))
              (leaf-2 (node-add-leaf! root (make-label "lah"))))
          (assert-eq? leaf-1 (node-find-child root #\b))
          (assert-eq? leaf-2 (node-find-child root #\l))
          (assert-false (node-find-child root #\a)))))
     
     
     (make-test-case
      "test splitting"
      (let-values (((root tree) (root-and-tree)))
        (let* ((leaf (node-add-leaf! root (make-label "ssi")))
               (joint (node-up-split! leaf 1)))
          (assert-equal? 1 (length (node-children root)))
          (assert-eq? root (node-parent joint))
          (assert-eq? joint (node-parent leaf))
          (assert-eq? joint (node-find-child root #\s))
          (assert-eq? leaf (node-find-child joint #\s)))))
     
     
     (make-test-case
      "node-up-splice-leaf!"
      (let-values (((root tree) (root-and-tree)))
        (let ((leaf (node-add-leaf! root (make-label "aaz"))))
          (let-values (((joint split-leaf)
                        (node-up-splice-leaf! leaf 1 (make-label "z"))))
            (assert-label-equal? (make-label "z") (node-up-label split-leaf))
            (assert-label-equal? (make-label "az") (node-up-label leaf))
            (assert-label-equal? (make-label "a")
                                 (node-up-label
                                  (node-parent split-leaf)))))))
     
     
     (make-test-case
      "following"
      (let-values (((root tree) (root-and-tree)))
        (let ((leaf (node-add-leaf! root (make-label "delicious"))))
          (node-follow/k root (make-label "delicious")
                         (lambda (node) #t)
                         (lambda (node offset) (fail))
                         (lambda (node label label-offset) (fail))
                         (lambda (node offset label label-offset) (fail)))
          (node-follow/k root (make-label "deli")
                         (lambda (node) (fail))
                         (lambda (node offset)
                           (assert-eq? node leaf)
                           (assert-equal? 4 offset))
                         (lambda (node label label-offset) (fail))
                         (lambda (node offset label label-offset) (fail)))
          (node-follow/k root (make-label "yummy")
                         (lambda (node) (fail))
                         (lambda (node offset) (fail))
                         (lambda (node label label-offset)
                           (assert-eq? root node)
                           (assert-label-equal? (make-label "yummy")
                                                (sublabel label label-offset)))
                         (lambda (node offset label label-offset) (fail)))
          (node-follow/k root (make-label "done")
                         (lambda (node) (fail))
                         (lambda (node offset) (fail))
                         (lambda (node label label-offset) (fail))
                         (lambda (node offset label label-offset)
                           (assert-eq? leaf node)
                           (assert-equal? 1 offset)
                           (assert-label-equal?
                            (make-label "one")
                            (sublabel label label-offset)))))))
     
     
     (make-test-case
      "test skip-counting"
      (let-values (((root tree) (root-and-tree)))
        (let* ((some-node (node-add-leaf! root (make-label "arabidopsis")))
               (another-leaf (node-add-leaf! some-node (make-label "thaliana"))))
          (let-values (((node offset)
                        (skip-count root (make-label "arabidopsist"))))
            (assert-eq? another-leaf node)
            (assert-equal? 1 offset)))))
     
     
     (make-test-case
      "test skip-counting on end"
      (let-values (((root tree) (root-and-tree)))
        (let* ((some-node (node-add-leaf! root (make-label "crunchy")))
               (another-leaf (node-add-leaf! some-node (make-label "bacon"))))
          (let-values (((node offset)
                        (skip-count root (make-label "crunchy"))))
            (assert-equal? 7 offset)
            (assert-eq? some-node node)))))
     
     
     (make-test-case
      "test skip-counting on empty string"
      (let-values (((root tree) (root-and-tree)))
        (let* ((some-node (node-add-leaf! root (make-label "power")))
               (another-leaf (node-add-leaf! some-node (make-label "rangers"))))
          (let-values (((node offset)
                        (skip-count root (make-label ""))))
            (assert-equal? 0 offset)
            (assert-eq? root node)))))
     
     
     (make-test-case
      "skip-counting within the tree on empty string"
      (let-values (((root tree) (root-and-tree)))
        (let ((leaf (node-add-leaf! root (make-label "blah"))))
          (let-values (((node offset)
                        (skip-count leaf (make-label ""))))
            (assert-eq? leaf node)
            (assert-equal? 4 offset)))))
     
     
     (make-test-case
      "simple suffix tree adding with single character"
      (let-values (((root tree) (root-and-tree)))
        (suffix-tree-add! tree (make-label "a"))
        (assert-not-false (node-find-child root #\a))))
     
     
     (make-test-case
      "simple suffix tree adding: aa --- implicit tree"
      (let-values (((root tree) (root-and-tree)))
        (suffix-tree-add! tree (make-label "aa"))
        (let ((child (node-find-child root #\a)))
          (assert-not-false child)
          (assert-label-equal? (make-label "aa") (node-up-label child)))))
     
     
     (make-test-case
      "simple suffix tree adding: aa with sentinel --- explicit tree"
      (let*-values
          (((root tree) (root-and-tree))
           ((label) (string->label/with-sentinel "aa"))
           ((a-label) (make-label "a"))
           ((sentinel-label) (sublabel label (sub1 (label-length label)))))
        (suffix-tree-add! tree label)
        (let ((child (node-find-child root (label-ref a-label 0))))
          (assert-not-false child)
          (assert-label-equal? a-label (node-up-label child))
          (assert-not-false (node-find-child child (label-ref a-label 0)))
          (assert-label-equal?
           (sublabel label 1)
           (node-up-label (node-find-child child (label-ref a-label 0)))))))
     
     
     
     (make-test-case
      "simple suffix tree adding: a$"
      (let-values (((root tree) (root-and-tree)))
        (suffix-tree-add! tree (make-label "a$"))
        (assert-not-false (node-find-child root #\a))
        (assert-label-equal? (make-label "a$")
                             (node-up-label
                              (node-find-child root #\a)))
        (assert-not-false (node-find-child root #\$))
        (assert-label-equal? (make-label "$")
                             (node-up-label
                              (node-find-child root #\$)))))
     
     
     (make-test-case
      "missisippi example: missisippi$"
      (let-values (((root tree) (root-and-tree)))
        (suffix-tree-add! tree (make-label "mississippi$"))
        (assert tree-contains? tree (make-label "mississippi$"))
        (assert tree-contains? tree (make-label "ississippi$"))
        (assert tree-contains? tree (make-label "ssissippi$"))
        (assert tree-contains? tree (make-label "sissippi$"))
        (assert tree-contains? tree (make-label "issippi$"))
        (assert tree-contains? tree (make-label "ssippi$"))
        (assert tree-contains? tree (make-label "sippi$"))
        (assert tree-contains? tree (make-label "ippi$"))
        (assert tree-contains? tree (make-label "ppi$"))
        (assert tree-contains? tree (make-label "pi$"))
        (assert tree-contains? tree (make-label "i$"))
        (assert tree-contains? tree (make-label "$"))
        (assert tree-contains? tree (make-label ""))))
     
     
     (make-test-case
      "missisippi$ example as a vector of symbols"
      (let* ((v (list->vector '(m i s s i s s i p p i $)))
             (label (vector->label v)))
        (let-values (((root tree) (root-and-tree)))
          (suffix-tree-add! tree label)
          (for-each-sublabel (lambda (s)
                               (assert tree-contains? tree s))
                             label))))
     
     
     (make-test-case
      "test all substrings of binary strings of length 8"
      (let loop ((n 0))
        (when (< n 256)
          (let-values (((root tree) (root-and-tree)))
            (let ((binary-label (make-label
                                 (string-append
                                  (zfill (number->string n 2) 8)
                                  "$"))))
              (suffix-tree-add! tree binary-label)
              (for-each-sublabel (lambda (s)
                                   (assert tree-contains? tree s
                                           (string-append
                                            "couldn't find "
                                            (label->string s)
                                            " in "
                                            (label->string binary-label))))
                                 binary-label)
              (loop (add1 n)))))))
     
     
     
     (make-test-case
      "testing jump-to-suffix on node with suffix link"
      (let*-values (((tree nodes) (tree-structure-for-00000100$))
                    ((node offset) (jump-to-suffix (list-ref nodes 8))))
        (assert-eq? (list-ref nodes 0) node)
        (assert-equal? 0 offset)))
     
     
     (make-test-case
      "another jump-to-suffix test."
      (let-values (((root tree) (root-and-tree)))
        (let* ((leaf (node-add-leaf! root (make-label "00000100$")))
               (joint (node-up-split! leaf 4)))
          (let-values (((n o) (jump-to-suffix joint)))
            (assert-eq? root n)
            (assert-false o)))))
     
     
     (make-test-case
      "jumping-to-suffix on root should return (values root #f)"
      (let*-values (((root tree) (root-and-tree))
                    ((n o) (jump-to-suffix root)))
        (assert-eq? root n)
        (assert-false o)))
     
     
     (make-test-case
      "find-next-extension-point at root should point to the root"
      (let-values (((root tree) (root-and-tree)))
        (let-values (((node offset i*)
                      (find-next-extension-point/add-suffix-link! root
                                                                  (make-label "hercules") 0 0)))
          (assert-eq? node root)
          (assert-equal? 0 offset)
          (assert-equal? 0 i*))))
     
     
     (make-test-case
      "looking for the next extension point"
      (let-values (((tree nodes) (tree-structure-for-00000100$)))
        (let-values (((node offset i*) (find-next-extension-point/add-suffix-link!
                                        (list-ref nodes 6)
                                        (make-label "00000100$")
                                        8 7)))
          (assert-eq? node (list-ref nodes 8))
          (assert-equal? 1 offset)
          (assert-equal? 8 i*))))
     
     
     (make-test-case
      "find-next-extension on beginning of 00$ construction"
      (let-values (((root tree) (root-and-tree)))
        (let ((leaf
               (node-add-leaf! root (make-label "00$"))))
          (let-values (((extension-node extension-offset i*)
                        (find-next-extension-point/add-suffix-link! root
                                                                    (make-label "001$")
                                                                    1 1)))
            (assert-eq? leaf extension-node)
            (assert-equal? 1 extension-offset)
            (assert-equal? 2 i*)))))
     
     
     (make-test-case
      "find-next-extension intensional failure"
      (let-values (((root tree) (root-and-tree)))
        (let ((leaf
               (node-add-leaf! root (make-label "000000"))))
          (let-values (((extension-node extension-offset i*)
                        (find-next-extension-point/add-suffix-link! root
                                                                    (make-label "000000")
                                                                    1 1)))
            (assert-false extension-node)
            (assert-false extension-offset)
            (assert-false i*)))))
     
     
     (make-test-case
      "extend-at-point! at the root"
      (let*-values (((root tree) (root-and-tree))
                    ((label) (make-label "foo$"))
                    ((first-char) #\f))
        (assert-false (node-find-child root first-char))
        (let ((last-active-node
               (extend-at-point! root 0 label 0)))
          (assert-eq? last-active-node root)
          (assert-not-false (node-find-child root first-char)))))
     
     
     (make-test-case
      "extend-at-point with splice"
      (let-values (((root tree) (root-and-tree)))
        (let ((leaf (node-add-leaf! root (make-label "001"))))
          (let ((new-active-point (extend-at-point! leaf 1
                                                    (make-label "001")
                                                    2)))
            (assert-label-equal? (make-label "0")
                                 (node-up-label new-active-point))
            (assert-label-equal? (make-label "01")
                                 (node-up-label leaf))
            (assert-not-false (node-find-child new-active-point
                                               #\1))))))
     
     
     
     (make-test-case
      "suffix-tree-add! on two single elements"
      (let-values (((root tree) (root-and-tree))
                   ((label-1 label-2) (values (make-label "$")
                                              (make-label "!"))))
        (suffix-tree-add! tree label-1)
        (suffix-tree-add! tree label-2)
        (for-each-sublabel (lambda (s) (assert tree-contains? tree s))
                           label-1)
        (for-each-sublabel (lambda (s) (assert tree-contains? tree s))
                           label-2)))
     
     
     (make-test-case
      "suffix-tree-add! on two short strings"
      (let-values (((root tree) (root-and-tree))
                   ((label-1 label-2) (values (make-label "a$")
                                              (make-label "a!"))))
        (suffix-tree-add! tree label-1)
        (suffix-tree-add! tree label-2)
        (assert tree-contains? tree (make-label "a"))
        (assert tree-contains? tree (make-label "a$"))
        (assert tree-contains? tree (make-label "$"))
        (assert tree-contains? tree (make-label "a!"))
        (assert tree-contains? tree (make-label "!"))
        (assert tree-contains? tree (make-label ""))
        ))
     
     
     ;; FIXME: Refactor pairwise comparison of these strings.
     (make-test-case
      "suffix-tree-add! on two strings"
      (let-values (((root tree) (root-and-tree))
                   ((label-1 label-2) (values (make-label "aaazzz$")
                                              (make-label "aaazza!"))))
        (suffix-tree-add! tree label-1)
        (suffix-tree-add! tree label-2)
        (for-each-sublabel (lambda (s) (assert tree-contains? tree s))
                           label-1)
        (for-each-sublabel (lambda (s) (assert tree-contains? tree s))
                           label-2)))
     
     ))
  
  (printf "ukkonen tests~%")
  (test/text-ui ukkonen-test-suite))

