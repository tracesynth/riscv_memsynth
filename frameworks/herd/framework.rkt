#lang racket

(require "../../memsynth/memsynth.rkt" "axioms.rkt" "execution.rkt" "model.rkt")
(provide herd (all-from-out "../../memsynth/memsynth.rkt"))

(struct herd-framework ()
  #:methods gen:memsynth-framework
  [(define (instantiate-execution f test)
     (make-execution test))
   (define (allow f M)
     (match-define (memory-model _ ppo) M)
      
     (ValidExecution rf co ppo)
    )])

(define herd (herd-framework))
