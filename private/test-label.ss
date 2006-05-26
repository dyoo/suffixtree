(require "test-common-definitions.ss")


(define label-tests
  (make-test-suite
   "test-label.ss"


   (make-test-case
    "label construction"
    (let ((label (make-label "hello")))
      (assert-equal? 5 (label-length label))))

   
   (make-test-case
    "test simple referencing"
    (let ((label (make-label "foobar")))
      (assert-equal? #\f (label-ref label 0))
      (assert-equal? #\o (label-ref label 1))
      (assert-equal? #\o (label-ref label 2))
      (assert-equal? #\b (label-ref label 3))
      (assert-equal? #\a (label-ref label 4))
      (assert-equal? #\r (label-ref label 5))))

   (make-test-case
    "test simple sublabeling and referencing"
    (let* ((label (make-label "supercalifragilisticexpialidocious"))
           (l2 (sublabel label 5 9))
           (l3 (sublabel l2 1 2)))

      (assert-equal? 5 (label-length (sublabel label 0 5)))
      (assert-equal? 4 (label-length l2))
      (assert-equal? 1 (label-length l3))
      
      (assert-equal? #\c (label-ref l2 0))
      (assert-equal? #\a (label-ref l2 1))
      (assert-equal? #\l (label-ref l2 2))
      (assert-equal? #\i (label-ref l2 3))

      (assert-equal? #\a (label-ref l3 0))))

   
   (make-test-case
    "prefix: empty-case"
    (assert-true (label-prefix? (make-label "") (make-label ""))))

   
   (make-test-case
    "prefix: equal case"
    (assert-true (label-prefix? (make-label "abra") (make-label "abra"))))

   (make-test-case
    "prefix: proper"
    (assert-true (label-prefix? (make-label "abra") (make-label "abracadabra"))))

   (make-test-case
    "prefix: empty is the prefix of everything"
    (assert-true (label-prefix? (make-label "") (make-label "blah"))))

   (make-test-case
    "prefix: simple failures"
    (assert-false (label-prefix? (make-label "hello") (make-label "")))
    (assert-false (label-prefix? (make-label "hello") (make-label "hi"))))

   (make-test-case
    "sublabel: single shift"
    (assert label-equal? (make-label "ello") (sublabel (make-label "hello") 1)))

   (make-test-case
    "sublabel: shift from back"
    (assert label-equal? (make-label "hiy") (sublabel (make-label "hiya") 0 3)))

   (make-test-case
    "sublabel: double shift from back"
    (assert label-equal? (make-label "hi") (sublabel
                                            (sublabel (make-label "hiya") 0 3)
                                            0 2)))

   (make-test-case
    "sublabel: shift from back and front"
    (assert label-equal? (make-label "i") (sublabel
                                           (sublabel
                                            (sublabel (make-label "hiya") 0 3)
                                            0 2)
                                           1)))
    
   (make-test-case
    "sublabel: double shift"
    (assert label-equal? (make-label "llo")
            (sublabel
             (sublabel (make-label "hello") 1)
             1)))


   (make-test-case
    "sublabel: double sublabel"
    (assert label-equal? (make-label "cowboy bebop")
            (sublabel (make-label "the anime cowboy bebop feels like firefly") 10 22))
    (assert label-equal? (make-label "cowboy")
            (sublabel
             (sublabel (make-label "the anime cowboy bebop feels like firefly") 10 22)
             0 6)))

   
   (make-test-case
    "label-source-id"
    (let* ((label (string->label "antigone"))
           (subl (sublabel label 4)))
      (assert-equal? (label-source-id label)
                     (label-source-id subl))))

   (make-test-case
    "label-same-source?"
    (let* ((label (string->label "hello"))
           (another-label (string->label "hello")))
      (assert-true (label-same-source? (sublabel label 0 3) (sublabel label 3)))
      (assert-false (label-same-source? label another-label))))))
    



(printf "label tests~%")
(test/text-ui label-tests)
