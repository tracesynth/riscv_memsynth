#lang rosette

(require (only-in ocelot simplify ast->datum))
(provide (struct-out memory-model) make-model
         (rename-out [memory-model-name model-name]
                     [memory-model-ppo  model-ppo]
                     ))

;; model ----------------------------------------------------------------
; a memory model consists of five parts:
; * a name
; * a preserved program order relation, which will be intersected with program
;   order


(struct memory-model (name ppo) #:transparent
  #:methods gen:custom-write
  [(define (write-proc self port mode)
    (fprintf port "(memory-model ~a\n        ppo: ~a\n "
                  (memory-model-name self)
                  (ast->datum (simplify (memory-model-ppo self)))
                  ))])

; an anonymous model
(define (make-model ppo) (memory-model 'anon ppo))
