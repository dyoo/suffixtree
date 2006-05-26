;; Small hacky script to build the planet distribution.  I need to
;; filter out all my test code (anything file whose name begins with
;; "test-"

(require (lib "util.ss" "planet"))
(require (lib "file.ss"))

(define build-dir (build-path "build"))
(define project.plt "suffixtree.plt")
(define package-dir (build-path "suffixtree"))
(define tmp-dir (build-path build-dir package-dir))
(define dist-dir (build-path "dist"))


;; Build the planet distribution, leaving the result in dist.
(define (build-planet-distribution)
  (copy-file* (build-path "doc.txt") tmp-dir)
  (shallow-copy-source-files/excluding-tests (build-path "private") (build-path tmp-dir "private"))
  (shallow-copy-source-files/excluding-tests (current-directory) tmp-dir)
  (parameterize ((current-directory (build-path "build")))
    (make-planet-archive package-dir))
  (copy-file* (build-path build-dir project.plt) dist-dir)
  (delete-directory/files tmp-dir)
  )


;; Copy a file, overriding if it already exists.
(define (copy-file* source-file-path dest-path-dir)
  (make-directory* dest-path-dir)
  (let ((basename (file-name-from-path source-file-path)))
    (unless (file-exists? (build-path dest-path-dir basename))
      (copy-file source-file-path (build-path dest-path-dir basename)))))


;; Returns #t if path is a source file that should be included in the
;; PLaneT distribution.
(define (ok-source-file-path? path)
  (let-values (((base name must-be-dir?) (split-path path)))
    (and (file-exists? path)
         (path-contains-module? path)
         (not (bytes-suffix? #"~" (path->bytes name)))
         (not (bytes-prefix? #"test-" (path->bytes name))))))


;; Returns true if the path refers to a file that looks like a module.
(define (path-contains-module? path)
  (and (file-exists? path)
       (call-with-input-file*
        path
        (lambda (ip)
          (let ((sexp (read ip)))
            (and (list? sexp)
                 (not (null? sexp))
                 (eq? 'module (car sexp))))))))
  
;; Returns #t if possible-prefix is a true prefix of s.
(define (bytes-prefix? possible-prefix s)
  (and (>= (bytes-length s) (bytes-length possible-prefix))
       (bytes=? possible-prefix (subbytes s 0 (bytes-length possible-prefix)))))


;; Returns #t if possible-suffix is a true suffix of s.
(define (bytes-suffix? possible-suffix s)
  (and (>= (bytes-length s) (bytes-length possible-suffix))
       (bytes=? possible-suffix (subbytes s (- (bytes-length s) (bytes-length possible-suffix))))))


(define (shallow-copy-source-files/excluding-tests source-path dest-path)
  (let ((files (directory-list source-path)))
    (for-each
     (lambda (f)
       (when (ok-source-file-path? (build-path source-path f))
         (copy-file* (build-path source-path f) dest-path)))
     files)))



(build-planet-distribution)
