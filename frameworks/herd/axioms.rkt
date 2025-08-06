#lang rosette

(require ocelot "../../litmus_riscv/litmus.rkt" "model.rkt" "execution.rkt")
(require (only-in racket/set list->set set-member?))
(provide (all-defined-out))
;; functions -------------------------------------------------------------------

;;; (define (fr rf co)
;;;   (+ (join (~ rf) co) (& (-> (- R (join W rf)) W) (join loc (~ loc)))))

;;; (define (rfe rf)
;;;   (- rf (join proc (~ proc))))

;;; (define (coe co)
;;;   (- co (join proc (~ proc))))

;;; (define (fre rf co)
;;;   (- (fr rf co) (join proc (~ proc)))) 


(define (com rf co)
  (+ rf co (fr rf co)))

(define (po_loc)
  (& po (join loc (~ loc))))


(define (po_loc_llh)
  (- (& po (join loc (~ loc))) (-> R R)))


(define (ghb rf co ppo grf ab)
  (+ ppo co (fr rf co) grf ab))

;; constraints -----------------------------------------------------------------

; rf: Write->Read
(define (WellFormed_rf rf co rfe po-loc)
  (and
   (in rf (& (-> W R) (join loc (~ loc)) (join data (~ data))))
   (no (- (join rf (~ rf)) iden))
   (all ([r (- R (join W rf))])
     (= (join r data) Zero))
  (no (& co (join rfe (~ po-loc))))   ;;;load value _axiom
  ))


(define (WellFormed_rfi rfi rf)
  (in rfi rf))

(define (WellFormed_rfe rfe rf)
  (in rfe rf))

; co: Write->Write
(define (WellFormed_co co)
  (and
   (in co (& (-> W W) (join loc (~ loc))))
   (no (& iden co))
   (in (join co co) co)
   (all ([a W])
     (all ([b (- (& W (join loc (join a loc))) a)])  ; all disj a,b : Writes & (loc.~loc) | ...
       (or (in (-> a b) co) (in (-> b a) co))))
   (in co (join loc (~ loc)))))


(define (WellFormed_coi coi co)
  (in coi co))

(define (WellFormed_coe coe co)
  (in coe co))

(define (WellFormed_fr fr co rf fr_init)
  (and 
    (in fr_init fr)
    (in fr (+ (join (~ rf) co) fr_init))
  )
  ;;; (in fr (join (~ rf) co))

)


(define (WellFormed_fri fri fr)
  (in fri fr))

(define (WellFormed_fre fre fr)
  (in fre fr))

(define (WellFormed_rsw rsw rf)
  (in rsw (join (~ rf) rf)))

(define (WellFormed_final event_loc event_val W)
  (and
    (no (join (~(join (~ (<: W event_loc)) event_val)) finalValueZero))
    (in finalValueNotZero (join (~ (<: W event_loc)) event_val))
  )
)

(define (WellFormed rf co fr rfi rfe coi coe fri fre fr_init po-loc event_loc event_val W )
  (and
   (WellFormed_rf rf co rfe po-loc)
   (WellFormed_rfi rfi rf)
   (WellFormed_rfe rfe rf)
   (WellFormed_co co)
   (WellFormed_coi coi co)
   (WellFormed_coe coe co)
   (WellFormed_fr fr co rf fr_init)
   (WellFormed_fri fri fr)
   (WellFormed_fre fre fr)
   (WellFormed_rsw rsw rf)
   (WellFormed_final event_loc event_val W)
   ))


(define (Final co)
  (all ([w W])
    (=> (and (in w (- (join univ co) (join co univ))) (some (join (join w loc) finalValue)))
        (= (join w data) (join (join w loc) finalValue)))))

;;; (define (main_axiom rfe co ppo fr)
;;;   (+ ppo co rfe fr)
;;; )

(define (ValidExecution rf co ppo)
  (and

    (WellFormed rf co fr rfi rfe coi coe fri fre fr_init po-loc event_loc event_val W )
    (Final co)
    (no (& rmw (join fre coe)))
    (no (& (^ (+ co rf fr po-loc)) iden))
    (no (& (^ (+ rfe co fr ppo)) iden))
    ))

