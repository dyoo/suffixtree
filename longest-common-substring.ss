(module longest-common-substring mzscheme
  (require (planet "suffixtree.ss" ("dyoo" "suffixtree.plt" 1 0))
           (only (lib "1.ss" "srfi") find append-map lset-union)
           (lib "etc.ss")
           (lib "list.ss")
           (lib "contract.ss"))
  
  (provide/contract [longest-common-substring
                     (string? string? . -> . string?)])
  
  
  (define (sentinel? ch)
    (not (char? ch)))
  
  (define (label-length/removing-sentinel label)
    (cond
      [(= 0 (label-length label)) 0]
      [(sentinel? (label-ref label (sub1 (label-length label))))
       (sub1 (label-length label))]
      [else
       (label-length label)]))
  
  
  ;; longest-common-substring: string string -> string
  ;; Returns the longest common substring between s1 and s2.
  (define (longest-common-substring s1 s2)
    (let-values ([(tree labels annotations)
                  (make-tree/annotations (list s1 s2))])
      (local ((define (shared? node)
                (= 2 (length (hash-table-get annotations node))))
              (define best-so-far '())
              (define best-so-far-length 0))
        ;; loop: node (listof label) number -> void
        ;; The inner loop here will just walk the tree as long
        ;; as the node is shared by both.
        (let loop ([node (tree-root tree)]
                   [labels '()]
                   [len 0])
          (cond [(shared? node)
                 (for-each (lambda (n)
                             (loop n
                                   (cons (node-up-label node) labels)
                                   (+ len (label-length/removing-sentinel
                                           (node-up-label node)))))
                           (node-children node))]
                [else
                 (when (> len best-so-far-length)
                   (set! best-so-far labels)
                   (set! best-so-far-length len))]))
        (apply string-append (reverse! (map label->string best-so-far))))))
  
  
  
  ;; make-tree/annotations: (listof string) -> 
  ;;                          (values tree
  ;;                                  (listof label)
  ;;                                  (hash-table-of node (listof label)))
  (define (make-tree/annotations strings)
    (let* ((tree (make-tree))
           (labels (map string->label/with-sentinel strings))
           (_ (for-each (lambda (l) (tree-add! tree l)) labels))
           (ht (annotate-tree tree labels)))
      (values tree labels ht)))
  
  
  ;; annotate-tree: tree (listof label) -> 
  ;;                    (hash-table-of node -> (listof label))
  ;; Builds a hash table mapping nodes of the tree to the labels
  ;; that node belongs to.
  (define (annotate-tree a-tree original-labels)
    (local ((define ht (make-hash-table))
            
            (define (label->original-label l)
              (find (lambda (o) (label-source-eq? l o))
                    original-labels)))
      
      (let loop ([node (tree-root a-tree)])
        (cond [(empty? (node-children node))
               (hash-table-put!
                ht node
                (list (label->original-label (node-up-label node))))]
              [else
               (for-each loop (node-children node))
               (hash-table-put! ht node
                                (apply lset-union eq?
                                       (map
                                        (lambda (c) (hash-table-get ht c))
                                        (node-children node))))]))
      ht)))