#lang racket

(require "../../../../litmus_riscv/lang.rkt")

(provide (all-defined-out) (all-from-out "../../../../litmus_riscv/lang.rkt"))

(define-litmus-test CoRR
  (((W x 1 () () ()))
   ((R x 1 () () ())
    (R x 0 () () ()))
  )
  #:post ((x 1))
  #:allowed )

(define-litmus-test MP+fence.w.w+fri-rfi-addr
  (((W x 1 () () ())
    (F fence.w.w)
    (W y 1 () () ()))
   ((R y 2 () () ())
    (W y 2 () () ())
    (R y 2 () () ())
    (R x 0 (5) () ())))
  #:post ()
  #:allowed RVWMOherd)

(define all-rvwmo-tests
  (list CoRR
        MP+fence.w.w+fri-rfi-addr))
;;;   (list
;;;  (litmus-test
;;;   "CoRR"
;;;   (Program
;;;    (list
;;;     (Thread 0 (list (Write 0 0 0 '() '() '() 'x 1)))
;;;     (Thread
;;;      1
;;;      (list (Read 1 0 1 '() '() '() 'x 1) (Read 2 1 1 '() '() '() 'x 0)))))
;;;   '((x 1))
;;;   '())
;;;  (litmus-test
;;;   "MP+fence.w.w+fri-rfi-addr"
;;;   (Program
;;;    (list
;;;     (Thread
;;;      0
;;;      (list
;;;       (Write 0 0 0 '() '() '() 'x 1)
;;;       (Fence 1 1 0 '() '() '() 0 0 'fence.w.w)
;;;       (Write 2 2 0 '() '() '() 'y 1)))
;;;     (Thread
;;;      1
;;;      (list
;;;       (Read 3 0 1 '() '() '() 'y 1)
;;;       (Write 4 1 1 '() '() '() 'y 2)
;;;       (Read 5 2 1 '() '() '() 'y 2)
;;;       (Read 6 3 1 '(2) '() '() 'x 0)))))
;;;   '()
;;;   '('RVWMOherd)))
