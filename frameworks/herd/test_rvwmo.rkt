#lang racket

(require "../../litmus_riscv/herd/compile.rkt")
(require racket/cmdline racket/pretty)
;;; (require "../../memsynth/verify.rkt")
(require "framework.rkt")
(require "../../litmus_riscv/litmus.rkt")
(require "sketch-model.rkt")
(require "models.rkt")
(require ocelot)
(require (only-in ocelot interpret bounds-union)
         (only-in rosette solve assert sat?))
 
(define path
(command-line
    #:args (filename)
    filename))

(define T (litmus-file->test (file->string path)))
(pretty-print T)
;;; allowed? f T M

(define bTest (instantiate-test T))
(displayln "bTest")
(pretty-print bTest)

(define bExec (instantiate-execution herd bTest))
(displayln "bExec")
(pretty-print bExec)
(match-define (memory-model _ ppo) RVWMO)
;;; (displayln "RVWMO")
;;; (pretty-print RVWMO)
(define Allow (allow herd RVWMO))

(define Allow* (interpret Allow (bounds-union bTest bExec)))
  (displayln "Allow*")
  (pretty-print Allow*)
  (match Allow*
    [#t #t]
    [#f #f]
    [_ (sat? (solve (assert Allow*)))])
