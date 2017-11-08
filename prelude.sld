
(define-library (macduffie prelude)
  ;; This is a general library of procedures I find useful that don't
  ;; fit in any particular library.
  (import
    (scheme base)
    (scheme case-lambda)
    (scheme char)
    (scheme write)
    (scheme inexact))
  (export value->procedure pi e i assq-ref assv-ref assoc-ref
          print memoize memoize-testing atom? flatten
          pair-conjugate complex-conjugate integer->hex integer->bin
          bin->integer hex->integer hex->bin bin->hex
          color-string->triplet triplet->color-string
          list->procedure vector->procedure string->procedure
          assq->procedure assv->procedure assoc->procedure)
  (include "prelude.body.scm"))

