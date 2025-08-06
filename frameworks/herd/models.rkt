#lang rosette

(require ocelot "model.rkt" "../../litmus_riscv/litmus.rkt")

(provide rfi rfe RVWMO (all-from-out "model.rkt"))

;; common support for memory models --------------------------------------------


; common relations used by memory models
(define rf (declare-relation 2 "rf"))
(define rfi (declare-relation 2 "rfi"))
(define rfe (declare-relation 2 "rfe"))
;;; (define (rfi rf)  ; rf edges on the same processor
;;;   (& rf (join proc (~ proc))))
;;; (define (rfe rf)  ; rf edges not on the same processor
;;;   (- rf (join proc (~ proc))))

(define co (declare-relation 2 "co"))
;;; (define (coi co)  ; co edges on the same processor)
;;;   (& co (join proc (~ proc))))
;;; (define (coe co)  ; co edges not on the same processor)
;;;   (- co (join proc (~ proc))))



(define rsw (declare-relation 2 "rsw"))

(define rmw (declare-relation 2 "rmw"))









;; memory models ---------------------------------------------------------------

; RVWMO

(define ppo-rvwmo 
  (let ([ppo1  (<: M (:> po-loc W))]
        ;;; [ppo2  (- (- (- (<: R (:> po-loc R)) 
        ;;;                 (join (<: R (:> po-loc R)) (:> po-loc R)))
        ;;;              rsw)
        ;;;           (<: Amo (:> po-loc R)))]
        [ppo2  (- (<: R (:> po-loc R)) 
                        (join (<: R (:> po-loc W)) (:> po-loc R)))
                    ]
        [ppo3  (<: (+ Amo XSc) (:> rfi R))]
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
        [ppo5  (<: AQ (:> po M))]
        [ppo6  (<: M (:> po RL))]
        ;;; [ppo7  (<: RCsc (:> po RCsc))]
        [ppo7  (<: RL (:> po AQ))]
        [ppo8  rmw]
        [ppo9  (<: M (:> addrdeps M))]
        [ppo10 (<: M (:> datadeps W))]
        [ppo11 (<: M (:> ctrldeps W))]
        [ppo12 (join (<: R (:> (+ addrdeps datadeps) W)) (:> rfi R))]
        [ppo13 (join (<: R (:> addrdeps M)) (:> po W))]
        )
    
    (apply + (list ppo1 ppo2 ppo3 ppo4 ppo5 ppo6 ppo7 ppo8
                    ppo9 ppo10 ppo11 ppo12 ppo13))))

;;; (define ppo-rvwmo   
;;;   (let (
;;;         ;;; [ppo1  (<: M (:> po-loc W))]
;;;         ;;; [ppo2  (- (- (- (<: R (:> po-loc R)) 
;;;         ;;;                 (join (<: R (:> po-loc R)) (:> po-loc R)))
;;;         ;;;              (rsw))
;;;         ;;;           (<: Amo (:> po-loc R)))]
;;;         [ppo2  (- (<: R (:> po-loc R)) 
;;;                         (join (<: R (:> po-loc W)) (:> po-loc R)))
;;;                     ]
;;;         ;;; [ppo2 (& (- (:> (- po rf) (+ W R)) (<: (<: Rcsc W) (- fence.rw.rw co))) po)]
;;;         ;;; [ppo3  (<: (+ Amo XSc) (:> (rfi rf) R))]
;;;         [ppo4  (<: W (:> fence.w.w W))]
;;;         [ppo9  (<: M (:> addrdeps M))]
;;;         )
    
;;;     (apply + (list ppo2 ppo4 ppo9))))

;;; (define ppo-rvwmo (+ (+ (<: M (:> addrdeps M)) (<: W (:> fence.w.w W))) (& po (- (:> (+ po co) (+ M Rcsc)) (+ (:> po W) (join po-loc po)))))
;;;  )

(define RVWMO (memory-model 'RVWMO ppo-rvwmo))