
(define-library (macduffie prelude)
  ;; A library of common procedures
  (import
    (scheme base)
    (scheme write)
    (scheme inexact))
  (export make-thunk pi e i mapassoc assq-ref assv-ref assoc-ref
          print memoize memoize-testing atom? flatten)
  (include "prelude.body.scm"))

