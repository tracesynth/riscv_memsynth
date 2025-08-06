#lang racket

(require "framework.rkt" "../litmus_riscv/litmus.rkt" 
         (only-in ocelot interpret bounds-union)
         (only-in rosette solve assert sat?))

(provide allowed?)

; is test T allowed by model M?
(define (allowed? f T M)
  (define bTest (instantiate-test T))
  (define bExec (instantiate-execution f bTest))
  ;;; (displayln "bTest:")
  ;;; (pretty-print bTest)
  ;;; (displayln "bExec:")
  ;;; (pretty-print bExec)
  ;;; (displayln "f")
  ;;; (pretty-print f)
  ;;; (displayln "M")
  ;;; (pretty-print M)
  (define Allow (allow f M))
  ;;; (displayln "Allow")
  ;;; (pretty-print Allow)
  (define Allow* (interpret Allow (bounds-union bTest bExec)))
  ;;; (displayln "Allow*")
  ;;; (pretty-print Allow*)  
  
  (match Allow*
    [#t #t]
    [#f #f]
    [_ (sat? (solve (assert Allow*)))]))




