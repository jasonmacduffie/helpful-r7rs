
(define (value->procedure val)
  (lambda () val))

(define (list->procedure l)
  (lambda (i)
    (list-ref l i)))

(define (vector->procedure v)
  (lambda (i)
    (vector-ref v i)))

(define (string->procedure s)
  (lambda (i)
    (string-ref s i)))

(define (assq->procedure al)
  (lambda (key)
    (cdr (assq key al))))

(define (assv->procedure al)
  (lambda (key)
    (cdr (assv key al))))

(define (assoc->procedure al)
  (lambda (key)
    (cdr (assoc key al))))

(define e (value->procedure 2.71828182845904523))

(define pi (value->procedure 3.14159265358979323))

(define i (value->procedure 0+i))

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

(define (pair-conjugate p)
  (cons (cdr p) (car p)))

(define (complex-conjugate c)
  (+ (real-part c) (* 0-i (imag-part c))))

(define (integer->hex n)
  (define hex-selection
    (assv->procedure
     '((0 . #\0) (1 . #\1) (2 . #\2) (3 . #\3) (4 . #\4)
       (5 . #\5) (6 . #\6) (7 . #\7) (8 . #\8) (9 . #\9)
       (10 . #\a) (11 . #\b) (12 . #\c) (13 . #\d)
       (14 . #\e) (15 . #\f))))
    
  (define (build-hex next-num previous-list)
    (cons (hex-selection (modulo next-num 16))
          previous-list))

  (when (or (negative? n) (not (integer? n)))
    (error "integer->hex" "Non-negative integer expected"))

  (let loop ((in n) (out '()))
    (if (= in 0)
        (list->string out)
        (loop (quotient in 16)
              (build-hex in out)))))

(define (integer->bin n)
  (define (build-bin next-num previous-list)
    (cons (if (even? next-num) #\0 #\1)
          previous-list))

  (when (or (negative? n) (not (integer? n)))
    (error "integer->bin" "Non-negative integer expected"))

  (let loop ((in n) (out '()))
    (if (= in 0)
        (if (null? out)
            "0"
            (list->string out))
        (loop (quotient in 2)
              (build-bin in out)))))

(define (hex->integer h)
  (define hex-deselection
    (assv->procedure
     '((#\0 . 0) (#\1 . 1) (#\2 . 2) (#\3 . 3) (#\4 . 4)
       (#\5 . 5) (#\6 . 6) (#\7 . 7) (#\8 . 8) (#\9 . 9)
       (#\a . 10) (#\b . 11) (#\c . 12) (#\d . 13)
       (#\e . 14) (#\f . 15))))

  (define (debuild-hex next-char previous-num)
    (+ (hex-deselection (char-downcase next-char))
       (* 16 previous-num)))

  (let loop ((in (string->list h)) (out 0))
    (if (null? in)
        out
        (loop (cdr in)
              (debuild-hex (car in) out)))))

(define (bin->integer b)
  (define (debuild-bin next-char previous-num)
    (+ (if (eqv? next-char #\0) 0 1)
       (* previous-num 2)))

  (let loop ((in (string->list b)) (out 0))
    (if (null? in)
        out
        (loop (cdr in)
              (debuild-bin (car in) out)))))

(define (bin->hex b)
  (integer->hex (bin->integer b)))

(define (hex->bin h)
  (integer->bin (hex->integer h)))

(define (pad-string-helper input-string desired-size padding-char left?)
  (let ((actual-size (string-length input-string)))
    (if (< actual-size desired-size)
        (if left?
            (string-append (make-string (- desired-size actual-size)
                                        padding-char)
                           input-string)
            (string-append input-string
                           (make-string (- desired-size actual-size)
                                        padding-char)))
        input-string)))

(define pad-left
  (case-lambda
   ((st sz)
    (pad-string-helper st sz #\space #t))
   ((st sz chr)
    (pad-string-helper st sz chr #t))))

(define pad-right
  (case-lambda
   ((st sz)
    (pad-string-helper st sz #\space #f))
   ((st sz chr)
    (pad-string-helper st sz chr #f))))

(define (color-string->triplet cs)
  (define (select-one-hex n)
    (hex->integer (string (string-ref cs n)
                          (string-ref cs n))))
  
  (define (select-two-hexes n)
    (hex->integer (string (string-ref cs n)
                          (string-ref cs (+ n 1)))))

  (cond
   ((= (string-length cs) 3)
    (list (select-one-hex 0)
          (select-one-hex 1)
          (select-one-hex 2)))
   ((= (string-length cs) 6)
    (list (select-two-hexes 0)
          (select-two-hexes 2)
          (select-two-hexes 4)))
   (else
    (error "color-string->triplet" "String must be of length 3 or 6" cs))))

(define (triplet->color-string trip)
  (unless (and (= (length trip) 3)
               (<= 0 (list-ref trip 0) 255)
               (<= 0 (list-ref trip 1) 255)
               (<= 0 (list-ref trip 2) 255)
               (integer? (list-ref trip 0))
               (integer? (list-ref trip 1))
               (integer? (list-ref trip 2)))
    (error "triplet->color-string"
      "Argument must be a triplet of integers 0 to 255" trip))

  (apply string-append (map (lambda (n)
                              (pad-left (integer->hex n) 2 #\0))
                            trip)))

(define (test-for-each? proc l . rest)
  (let ((test-result (apply map proc l rest)))
    (not (memv #f test-result))))

(define (properize p)
  (if (pair? (cdr p))
      (cons (car p) (properize (cdr p)))
      (list (car p) (cdr p))))

(define (improperize p)
  (if (pair? (cddr p))
      (cons (car p) (improperize (cdr p)))
      (cons (car p) (cadr p))))

