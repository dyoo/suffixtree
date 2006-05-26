(require (planet "test.ss" ("schematics" "schemeunit.plt" 1 1))
         (planet "text-ui.ss" ("schematics" "schemeunit.plt" 1 1))
         "suffixtree.ss"
         "util.ss")


(define-simple-assertion (assert-label-equal? a b)
  (label-equal? a b))

(define util-tests
  (make-test-suite
   "test-util.ss"

   (make-test-case
    "longest common sublabel of empty strings is empty strings 1"
    (assert-label-equal? (string->label "")
                         (longest-common-sublabel (string->label "")
                                                  (string->label ""))))

   (make-test-case
    "longest common sublabel of empty strings is empty strings 2"
    (assert-label-equal? (string->label "")
                         (longest-common-sublabel (string->label "aha!")
                                                  (string->label ""))))

   (make-test-case
    "longest common sublabel of empty strings is empty strings 2"
    (assert-label-equal? (string->label "")
                         (longest-common-sublabel (string->label "")
                                                  (string->label "bah!"))))

   (make-test-case
    "longest common sublabel with trivial answer"
    (assert-label-equal? (string->label "a")
                         (longest-common-sublabel (string->label "a?")
                                                  (string->label "a!"))))
   (make-test-case
    "longest-common-sublabel with less trivial answer"
    (assert-label-equal?
     (string->label "alive")
     (longest-common-sublabel (string->label "superiorcalifornialives?")
                              (string->label "sealiver!"))))

   (make-test-case
    "longest-common-sublabel with less trivial answer and sentinels"
    (assert-label-equal?
     (string->label "alive")
     (longest-common-sublabel (string->label/with-sentinel "superiorcalifornialives")
                              (string->label/with-sentinel "sealiver"))))
      
   (make-test-case
    "path-label on empty tree"
    (assert-label-equal? (string->label "")
                         (path-label (tree-root (make-tree)))))

   (make-test-case
    "path-label on leaf with no joints"
    (let ((tree (make-tree)))
      (tree-add! tree (string->label "abcd"))
      (let ((leaf (node-find-child (tree-root tree) #\a)))
        (assert-label-equal? (string->label "abcd")
                             (path-label leaf)))))
   (make-test-case
    "path-label on the joints of 'abac'
    +--- a ---- bac
    |    |
    |    + ---- c
    |
    +--- bac
    |
    +--- c
    "
    (let* ((tree (make-tree))
           (root (tree-root tree)))
      (tree-add! tree (string->label "abac"))
      (assert-label-equal? (string->label "a")
                           (path-label (node-find-child root #\a)))
      (assert-label-equal? (string->label "abac")
                           (path-label (node-find-child
                                              (node-find-child root #\a)
                                              #\b)))
      (assert-label-equal? (string->label "ac")
                           (path-label (node-find-child
                                              (node-find-child root #\a)
                                              #\c)))
      (assert-label-equal? (string->label "bac")
                           (path-label (node-find-child root #\b)))
      (assert-label-equal? (string->label "c")
                           (path-label (node-find-child root #\c)))))))



(error-print-width 800)
(printf "util.ss tests~%")
(test/text-ui util-tests)
