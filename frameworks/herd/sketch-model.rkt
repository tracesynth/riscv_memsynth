#lang rosette

(require "models.rkt" "../../litmus_riscv/litmus.rkt"
         ocelot
         rosette/lib/angelic)

(provide (all-defined-out))

(define (trivial-sketch . models)
  (apply choose* models))

(define (make-ppo-sketch depth ops terminals)
  (& po (expression-sketch depth 2 ops terminals)))


