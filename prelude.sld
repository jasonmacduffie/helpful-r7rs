
(define-library (macduffie prelude)
  ;; A library of common procedures
  (import
    (scheme base)
    (scheme case-lambda)
    (scheme write)
    (scheme inexact))
  (export make-thunk pi e i mapassoc assq-ref assv-ref assoc-ref
          print memoize memoize-testing atom? flatten
          pair-conjugate complex-conjugate integer->hex integer->bin
          bin->integer hex->integer hex->bin bin->hex
          color-string->triplet triplet->color-string)
  (include "prelude.body.scm"))

