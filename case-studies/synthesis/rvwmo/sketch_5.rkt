#lang rosette

(require racket/require
         (multi-in "../../../frameworks/herd" ("models.rkt" "sketch-model.rkt"))
         ocelot
         "../../../litmus_riscv/litmus.rkt")
(provide rvwmo-sketch)
(require racket/date)


(define M (declare-relation 1 "M"))
(define R (declare-relation 1 "R"))
(define W (declare-relation 1 "W"))
(define Amo (declare-relation 1 "Amo"))
;;; (define X (declare-relation 1 "X"))
(define XSc (declare-relation 1 "XSc"))
(define XLr (declare-relation 1 "XLr"))
(define AQ (declare-relation 1 "AQ"))
(define RL (declare-relation 1 "RL"))
(define Rcsc (declare-relation 1 "Rcsc"))
(define AcqRel (declare-relation 1 "AcqRel"))

(define fence.rw.rw (declare-relation 2 "fence.rw.rw"))
(define fence.rw.r (declare-relation 2 "fence.rw.r"))
(define fence.rw.w (declare-relation 2 "fence.rw.w"))
(define fence.r.rw (declare-relation 2 "fence.r.rw"))
(define fence.r.r (declare-relation 2 "fence.r.r"))
(define fence.r.w (declare-relation 2 "fence.r.w"))
(define fence.w.rw (declare-relation 2 "fence.w.rw"))
(define fence.w.r (declare-relation 2 "fence.w.r"))
(define fence.w.w (declare-relation 2 "fence.w.w"))
(define fence.tso (declare-relation 2 "fence.tso"))
(define po (declare-relation 2 "po"))
(define po-loc (declare-relation 2 "po-loc"))
(define addrdeps (declare-relation 2 "addrdeps"))
(define datadeps (declare-relation 2 "datadeps"))
(define ctrldeps (declare-relation 2 "ctrldeps"))
(define rmw (declare-relation 2 "rmw"))
(define rfi (declare-relation 2 "rfi"))
(define rfe (declare-relation 2 "rfe"))
(define coi (declare-relation 2 "coi"))
(define coe (declare-relation 2 "coe"))
(define fri (declare-relation 2 "fri"))
(define fre (declare-relation 2 "fre"))
(define rsw (declare-relation 2 "rsw"))
(define rf (declare-relation 2 "rf"))
(define co (declare-relation 2 "co"))
(define fr (declare-relation 2 "fr"))
;; Creates a RISCV sketch, in which ppo/grf/fences all have depth 4.
(define start-time (current-seconds))
(printf "start-time ~a\n" start-time)
(define ppo (make-ppo-sketch 5 (list + - <: :> join)
                               (list M R W Amo XSc XLr RL AQ Rcsc 
                               fence.rw.rw fence.rw.r fence.rw.w fence.r.rw fence.r.r fence.r.w
                               fence.w.rw fence.w.w fence.w.r fence.tso po po-loc addrdeps datadeps rmw ctrldeps rfi rfe coi coe fri fre rsw rf co fr)))
                          
;;; (define ppo_exist 
;;;    (let (
;;;         [ppo4  (let ([*fence.rw.rw (<: M (:> fence.rw.rw M))]
;;;                 [*fence.w.rw  (<: W (:> fence.w.rw M))]
;;;                 [*fence.r.rw  (<: R (:> fence.r.rw M))]
;;;                 [*fence.rw.r  (<: M (:> fence.rw.r R))]
;;;                 [*fence.w.r   (<: W (:> fence.w.r R))]
;;;                 [*fence.r.r   (<: R (:> fence.r.r R))]
;;;                 [*fence.rw.w  (<: M (:> fence.rw.w W))]
;;;                 [*fence.w.w   (<: W (:> fence.w.w W))]
;;;                 [*fence.r.w   (<: R (:> fence.r.w W))]
;;;                 [*fence.tso   (+ (<: W (:> fence.tso W))
;;;                                 (<: R (:> fence.tso M)))])
;;;             (apply + 
;;;             (list *fence.rw.rw *fence.w.rw *fence.r.rw *fence.rw.r 
;;;                     *fence.w.r *fence.r.r *fence.rw.w *fence.w.w 
;;;                     *fence.r.w *fence.tso)))]
;;;         [ppo9  (<: M (:> addrdeps M))]
;;;         )
    
;;;     (apply + (list ppo4 ppo9)))
;;; )
(define ppo_exist 
  (let (
        [ppo4  (let ([*fence.rw.rw (<: M (:> fence.rw.rw M))]
                     [*fence.w.rw  (<: W (:> fence.w.rw M))]
                     [*fence.r.rw  (<: R (:> fence.r.rw M))]
                     [*fence.rw.r  (<: M (:> fence.rw.r R))]
                     [*fence.w.r   (<: W (:> fence.w.r R))]
                     [*fence.r.r   (<: R (:> fence.r.r R))]
                     [*fence.rw.w  (<: M (:> fence.rw.w W))]
                     [*fence.w.w   (<: W (:> fence.w.w W))]
                     [*fence.r.w   (<: R (:> fence.r.w W))]
                     [*fence.tso   (+ (<: W (:> fence.tso W))
                                      (<: R (:> fence.tso M)))])
                 (apply + 
                        (list *fence.rw.rw *fence.w.rw *fence.r.rw *fence.rw.r 
                              *fence.w.r *fence.r.r *fence.rw.w *fence.w.w 
                              *fence.r.w *fence.tso)))]
        )
    ppo4))


;;; (define rvwmo-sketch (make-model ppo))
(define rvwmo-sketch (make-model (+ ppo ppo_exist)))
(define end-time (current-seconds))
(printf "end-time ~a\n" end-time)
(define duration (- end-time start-time))
(printf "Task took ~a seconds\n" duration)
;;; (pretty-print ppo)
;;; (pretty-print (symbolics rvwmo-sketch) )
;; Count the size of the search space defined by the sketch


(module+ main

    (printf "rvwmo search space: 2^~v\n" (length (symbolics rvwmo-sketch))))



