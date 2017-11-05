
(define (make-thunk val)
  (lambda () val))

(define (mapassoc key lis)
  (map (lambda (i)
         (assoc key i))
       lis))

(define e (make-thunk 2.71828182845904523))

(define pi (make-thunk 3.14159265358979323))

(define i (make-thunk 0+i))

(define (assq-ref al k)
  (cdr (assq k al)))

(define (assv-ref al k)
  (cdr (assv k al)))

(define (assoc-ref al k)
  (cdr (assoc k al)))

(define (print . l)
  (for-each display l)
  (newline))

(define (each-assoc key alist op)
  ;; helper for memoize -- generic assoc for list keys
  (if (null? alist)
      #f
      (if (and (= (length key) (length (caar alist)))
               (let loop ((alist-key-in (caar alist))
                          (input-key-in key))
                 (cond
                  ((null? alist-key-in)
                   #t)
                  ((op (car alist-key-in) (car input-key-in))
                   (loop (cdr alist-key-in)
                         (cdr input-key-in)))
                  (else
                   #f))))
          (car alist)
          (each-assoc key (cdr alist) op))))

(define-syntax memoize
  (syntax-rules ()
    ((_ op proc)
     (let ((cache '()))
       (lambda args
         (let ((cache-reference (each-assoc args cache op)))
           (if cache-reference
               (cadr cache-reference)
               (let ((result (apply proc args)))
                 (set! cache (cons (list args result) cache))
                 result))))))))

(define-syntax memoize-testing
  (syntax-rules ()
    ((_ op proc)
     (let ((cache '()))
       (lambda args
         (write cache)
         (newline)
         (let ((cache-reference (each-assoc args cache op)))
           (if cache-reference
               (cadr cache-reference)
               (let ((result (apply proc args)))
                 (set! cache (cons (list args result) cache))
                 result))))))))

(define (atom? val)
  (not (or (null? val) (pair? val))))

(define (flatten l)
  (cond
   ((list? l) (apply append (map flatten l)))
   (else (list l))))

