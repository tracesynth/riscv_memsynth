#lang rosette

(require "lang.rkt" 
         (only-in ocelot 
                  declare-relation make-exact-bound bounds universe))
(provide (all-defined-out))

;; Defines the signatures for the relations that make up a litmus test, and a
;; procedure to convert a litmus-test? to bounds on these relations

; abstract sig MemoryEvent {
(define MemoryEvent (declare-relation 1 "MemoryEvent"))
; abstract M (MemoryEvent - Fences)
(define M (declare-relation 1 "M"))
;  proc: Int
(define proc (declare-relation 2 "proc"))
;  loc: Int
(define loc (declare-relation 2 "loc"))
;  data: Int
(define data (declare-relation 2 "data"))
;  po: set MemoryEvent
(define po (declare-relation 2 "po"))
;  po: set MemoryEvent
(define po-loc (declare-relation 2 "po-loc"))
;  addrdeps: set MemoryEvent
(define addrdeps (declare-relation 2 "addrdeps"))
;  datadeps: set MemoryEvent
(define datadeps (declare-relation 2 "datadeps"))
;  ctrldeps: set MemoryEvent
(define ctrldeps (declare-relation 2 "ctrldeps"))
;  fence.rw.rw: set MemoryEvent
(define fence.rw.rw (declare-relation 2 "fence.rw.rw"))
;  fence.rw.r: set MemoryEvent
(define fence.rw.r (declare-relation 2 "fence.rw.r"))
;  fence.rw.w: set MemoryEvent
(define fence.rw.w (declare-relation 2 "fence.rw.w"))
;  fence.r.rw: set MemoryEvent
(define fence.r.rw (declare-relation 2 "fence.r.rw"))
;  fence.r.r: set MemoryEvent
(define fence.r.r (declare-relation 2 "fence.r.r"))
;  fence.r.w: set MemoryEvent
(define fence.r.w (declare-relation 2 "fence.r.w"))
;  fence.w.rw: set MemoryEvent
(define fence.w.rw (declare-relation 2 "fence.w.rw"))
;  fence.w.r: set MemoryEvent
(define fence.w.r (declare-relation 2 "fence.w.r"))
;  fence.w.w: set MemoryEvent
(define fence.w.w (declare-relation 2 "fence.w.w"))
;  fence.tso: set MemoryEvent
(define fence.tso (declare-relation 2 "fence.tso"))

(define rmw (declare-relation 2 "rmw"))
; }

; abstract sig Location {
;   finalValue: int
(define finalValue (declare-relation 2 "finalValue"))
(define finalValueNotZero (declare-relation 2 "finalValueNotZero"))
(define finalValueZero (declare-relation 2 "finalValueZero"))
; }
; abstract sig Read extends MemoryEvent
(define R (declare-relation 1 "R"))
; abstract sig Write extends MemoryEvent
(define W (declare-relation 1 "W"))
; abstract sig Fence extends MemoryEvent
(define F (declare-relation 1 "F"))
; abstract sig Amo extends MemoryEvent
(define Amo (declare-relation 1 "Amo"))
; abstract sig X extends MemoryEvent
(define X (declare-relation 1 "X"))
; abstract sig XSc extends MemoryEvent
(define XSc (declare-relation 1 "XSc"))
; abstract sig XLr extends MemoryEvent
(define XLr (declare-relation 1 "XLr"))
; abstract sig AQ extends MemoryEvent
(define AQ (declare-relation 1 "AQ"))
; abstract sig RL extends MemoryEvent
(define RL (declare-relation 1 "RL"))
; abstract sig Rcsc extends MemoryEvent
(define Rcsc (declare-relation 1 "Rcsc"))
; abstract sig AcqRel extends MemoryEvent
(define AcqRel (declare-relation 1 "AcqRel"))


; sig Int
(define Int (declare-relation 1 "Int"))
; one sig Zero extends Int
(define Zero (declare-relation 1 "Zero"))


; Instantiate a litmus-test? as bounds on the above relations.
(define (instantiate-test T)
  (define-values (P post min-ints) (canonicalize-program T))
  ;;; (displayln "canonicalize")
  ;;; (pretty-print P)
  (define actions (all-actions P))
  (define num-events (length actions))
  (define num-ints min-ints)
  (define num-atoms (max num-events num-ints))
  (define atoms (for/list ([i num-atoms]) (string->symbol (~v i))))

  (define ME-atoms (take atoms num-events))
  (define Int-atoms (take atoms num-ints))
  (define U (universe atoms))
  (define MEs (for/list ([me ME-atoms]) (list me)))

  (define bMemoryEvent (make-exact-bound MemoryEvent MEs))
  (define bProc
    (make-exact-bound proc (for/list ([a actions]) (list (list-ref ME-atoms  (Action-gid a))
                                                   (list-ref Int-atoms (Action-thd a))))))
  ;;; (displayln "bProc")
  ;;; (pretty-print bProc)
  (define bLoc
    (make-exact-bound loc (for/list ([a actions] #:unless (Fence? a)) (list (list-ref ME-atoms (Action-gid a))
                                                  (list-ref Int-atoms (Action-addr a))))))
  ;;; (displayln "bLoc")
  ;;; (pretty-print bLoc)
  (define bData
    (make-exact-bound data (for/list ([a actions] #:unless (Fence? a)) (list (list-ref ME-atoms (Action-gid a))
                                                   (list-ref Int-atoms (Action-val a))))))
  ;;; (displayln "bData")
  ;;; (pretty-print bData)
  (define bPO
    (make-exact-bound po (for*/list ([a actions][b actions]
                               #:when (and (= (Action-thd a) (Action-thd b))
                                            (< (Action-lid a) (Action-lid b))))
                     (list (list-ref ME-atoms (Action-gid a)) (list-ref ME-atoms (Action-gid b))))))
  ;;; (displayln "bPO")
  ;;; (pretty-print bPO)
  (define bPOLoc
    (make-exact-bound po-loc (for*/list ([a actions][b actions] #:unless (or (Fence? a) (Fence? b))
                               #:when (and (= (Action-thd a) (Action-thd b))
                                            (< (Action-lid a) (Action-lid b))
                                            (= (Action-addr a) (Action-addr b))))
                     (list (list-ref ME-atoms (Action-gid a)) (list-ref ME-atoms (Action-gid b))))))
  ;;; (displayln "bPOLoc")
  ;;; (pretty-print bPOLoc)
  (define bAddrDP
    (make-exact-bound addrdeps (for*/list ([a actions][b actions] #:unless (or (Fence? a) (Fence? b))
                               #:when (and (= (Action-thd a) (Action-thd b))
                                            (member (Action-lid a) (Action-addrdeps b))))
                     (list (list-ref ME-atoms (Action-gid a)) (list-ref ME-atoms (Action-gid b))))))
  (define bDataDP
    (make-exact-bound datadeps (for*/list ([a actions][b actions] #:unless (or (Fence? a) (Fence? b))
                               #:when (and (= (Action-thd a) (Action-thd b))
                                            (member (Action-lid a) (Action-datadeps b))))
                     (list (list-ref ME-atoms (Action-gid a)) (list-ref ME-atoms (Action-gid b))))))
  (define bCtrlDP
    (make-exact-bound ctrldeps (for*/list ([a actions][b actions] #:unless (or (Fence? a) (Fence? b))
                               #:when (and (= (Action-thd a) (Action-thd b))
                                            (member (Action-lid a) (Action-ctrldeps b))))
                     (list (list-ref ME-atoms (Action-gid a)) (list-ref ME-atoms (Action-gid b))))))
  ;;; (displayln "bAddrDP")
  ;;; (pretty-print bAddrDP)
  (define bfinalValue
    (make-exact-bound finalValue (for/list ([AV post])
                             (list (list-ref Int-atoms (car AV)) (list-ref Int-atoms (cdr AV))))))

  (define first-int-atom (first Int-atoms))
  (define bfinalValueNotZero
    (make-exact-bound finalValueNotZero (for/list ([AV post] #:when (not (equal? (list-ref Int-atoms (cdr AV)) first-int-atom))) 
                              (list (list-ref Int-atoms (car AV)) (list-ref Int-atoms (cdr AV))))))
  ;;; (displayln "bfinalValueNotZero")
  ;;; (pretty-print bfinalValueNotZero)

    (define bfinalValueZero
    (make-exact-bound finalValueZero (for/list ([AV post] #:when (equal? (list-ref Int-atoms (cdr AV)) first-int-atom)) 
                              (list (list-ref Int-atoms (car AV)) (list-ref Int-atoms (cdr AV))))))
  ;;; (displayln "bfinalValueZero")
  ;;; (pretty-print bfinalValueZero)

  (define bM  
    (make-exact-bound M (for/list ([a actions] #:when (or (Read? a) (Write? a )))
                        (list (list-ref ME-atoms (Action-gid a))))))
  (define bRead
    (make-exact-bound R (for/list ([a actions] #:when (Read? a)) (list (list-ref ME-atoms (Action-gid a))))))
  (define bWrite
    (make-exact-bound W (for/list ([a actions] #:when (Write? a))
     (list (list-ref ME-atoms (Action-gid a))))))

  (define bFence.rw.rw
    (make-exact-bound fence.rw.rw (for*/list ([a actions][b actions][c actions]
                               #:when (and (= (Action-thd a) (Action-thd b))
                                           (= (Action-thd a) (Action-thd c))
                                           (and (Fence? c) (eq? (Fence-type c) 'fence.rw.rw))
                                           (< (Action-lid a) (Action-lid c)) (< (Action-lid c) (Action-lid b))
                                      ))
                     (list (list-ref ME-atoms (Action-gid a)) (list-ref ME-atoms (Action-gid b))))))

  (define bFence.rw.r
    (make-exact-bound fence.rw.r (for*/list ([a actions][b actions][c actions]
                               #:when (and (= (Action-thd a) (Action-thd b))
                                           (= (Action-thd a) (Action-thd c))
                                           (and (Fence? c) (eq? (Fence-type c) 'fence.rw.r))
                                           (< (Action-lid a) (Action-lid c)) (< (Action-lid c) (Action-lid b))
                                      ))
                     (list (list-ref ME-atoms (Action-gid a)) (list-ref ME-atoms (Action-gid b))))))
  ;;; (displayln "bFence.rw.r")
  ;;; (pretty-print bFence.rw.r)
  (define bFence.rw.w
    (make-exact-bound fence.rw.w (for*/list ([a actions][b actions][c actions]
                               #:when (and (= (Action-thd a) (Action-thd b))
                                           (= (Action-thd a) (Action-thd c))
                                           (and (Fence? c) (eq? (Fence-type c) 'fence.rw.w))
                                           (< (Action-lid a) (Action-lid c)) (< (Action-lid c) (Action-lid b))
                                      ))
                     (list (list-ref ME-atoms (Action-gid a)) (list-ref ME-atoms (Action-gid b))))))
  (define bFence.r.rw
    (make-exact-bound fence.r.rw (for*/list ([a actions][b actions][c actions]
                               #:when (and (= (Action-thd a) (Action-thd b))
                                           (= (Action-thd a) (Action-thd c))
                                           (and (Fence? c) (eq? (Fence-type c) 'fence.r.rw))
                                           (< (Action-lid a) (Action-lid c)) (< (Action-lid c) (Action-lid b))
                                      ))
                     (list (list-ref ME-atoms (Action-gid a)) (list-ref ME-atoms (Action-gid b))))))
  (define bFence.r.r
    (make-exact-bound fence.r.r (for*/list ([a actions][b actions][c actions]
                               #:when (and (= (Action-thd a) (Action-thd b))
                                           (= (Action-thd a) (Action-thd c))
                                           (and (Fence? c) (eq? (Fence-type c) 'fence.r.r))
                                           (< (Action-lid a) (Action-lid c)) (< (Action-lid c) (Action-lid b))
                    
                                      ))
                     (list (list-ref ME-atoms (Action-gid a)) (list-ref ME-atoms (Action-gid b))))))
  (define bFence.r.w
    (make-exact-bound fence.r.w (for*/list ([a actions][b actions][c actions]
                               #:when (and (= (Action-thd a) (Action-thd b))
                                           (= (Action-thd a) (Action-thd c))
                                           (and (Fence? c) (eq? (Fence-type c) 'fence.r.w))
                                           (< (Action-lid a) (Action-lid c)) (< (Action-lid c) (Action-lid b))
                                      ))
                     (list (list-ref ME-atoms (Action-gid a)) (list-ref ME-atoms (Action-gid b))))))
  (define bFence.w.rw
    (make-exact-bound fence.w.rw (for*/list ([a actions][b actions][c actions]
                               #:when (and (= (Action-thd a) (Action-thd b))
                                           (= (Action-thd a) (Action-thd c))
                                           (and (Fence? c) (eq? (Fence-type c) 'fence.w.rw))
                                           (< (Action-lid a) (Action-lid c)) (< (Action-lid c) (Action-lid b))
                                      ))
                     (list (list-ref ME-atoms (Action-gid a)) (list-ref ME-atoms (Action-gid b))))))
  (define bFence.w.r
    (make-exact-bound fence.w.r (for*/list ([a actions][b actions][c actions]
                               #:when (and (= (Action-thd a) (Action-thd b))
                                           (= (Action-thd a) (Action-thd c))
                                           (and (Fence? c) (eq? (Fence-type c) 'fence.w.r))
                                           (< (Action-lid a) (Action-lid c)) (< (Action-lid c) (Action-lid b))
                                      ))
                     (list (list-ref ME-atoms (Action-gid a)) (list-ref ME-atoms (Action-gid b))))))              
  (define bFence.w.w
    (make-exact-bound fence.w.w (for*/list ([a actions][b actions][c actions]
                               #:when (and (= (Action-thd a) (Action-thd b))
                                           (= (Action-thd a) (Action-thd c))
                                           (and (Fence? c) (eq? (Fence-type c) 'fence.w.w))
                                           (< (Action-lid a) (Action-lid c)) (< (Action-lid c) (Action-lid b))
                                      ))
                     (list (list-ref ME-atoms (Action-gid a)) (list-ref ME-atoms (Action-gid b))))))      
  (define bFence.tso
    (make-exact-bound fence.tso (for*/list ([a actions][b actions][c actions]
                               #:when (and (= (Action-thd a) (Action-thd b))
                                           (= (Action-thd a) (Action-thd c))
                                           (and (Fence? c) (eq? (Fence-type c) 'fence.tso))
                                           (< (Action-lid a) (Action-lid c)) (< (Action-lid c) (Action-lid b))
                                      ))
                     (list (list-ref ME-atoms (Action-gid a)) (list-ref ME-atoms (Action-gid b))))))   
  ;;; (displayln "bFence.tso")
  ;;; (pretty-print bFence.tso)    

  (define brmw 
    (make-exact-bound rmw (for*/list ([a actions][b actions]
                          #:when (or
                                  ;;; AMO
                                  (and 
                                      (= (Action-thd a) (Action-thd b))
                                      (= (Action-lid a) (Action-lid b))
                                      (< (Action-gid a) (Action-gid b)))
                                  ;;; X 
                                  (and 
                                      (= (Action-thd a) (Action-thd b))
                                      (< (Action-gid a) (Action-gid b))
                                      (and (XLrInst? a) (XScInst? b))
                                      ;;; (= (add1 (list-ref Int-atoms (Action-gid a))) (list-ref Int-atoms (Action-gid b)))
                                      (= (add1 (Action-gid a)) (Action-gid b))
                                  )
                                 ))
                      (list (list-ref ME-atoms (Action-gid a)) (list-ref ME-atoms (Action-gid b))))))

  ;;; (define bAmo 
  ;;;   (make-exact-bound Amo (for/list ([a actions] #:when (AmoInst? a)) (list (list-ref ME-atoms (Action-gid a))))))
  (define bAmo 
    (make-exact-bound Amo (for*/list ([a actions][b actions]
                          #:when (and 
                                      (= (Action-thd a) (Action-thd b))
                                      (= (Action-lid a) (Action-lid b))
                                      (or (< (Action-gid a) (Action-gid b)) (> (Action-gid a) (Action-gid b)))
                                 ))
                      (list (list-ref ME-atoms (Action-gid a))))))

  (define bXLr
    (make-exact-bound XLr (for/list ([a actions] #:when (XLrInst? a)) (list (list-ref ME-atoms (Action-gid a))))))
  
  (define bXSc
    (make-exact-bound XSc (for/list ([a actions] #:when (XScInst? a)) (list (list-ref ME-atoms (Action-gid a))))))
  
  (define bAQ
    (make-exact-bound AQ (for/list ([a actions] #:when (or (and (Write? a) (equal? (Write-type a) 'AQ))
                                                            (and (Read? a) (eq? (Read-type a) 'AQ))
                                                            (and (Write? a) (eq? (Write-type a) 'AQRL))
                                                            (and (Read? a) (eq? (Read-type a) 'AQRL))))
    (list (list-ref ME-atoms (Action-gid a))))))
  
  (define bRL
    (make-exact-bound RL (for/list ([a actions] #:when (or (and (Write? a) (eq? (Write-type a) 'RL))
                                                            (and (Read? a) (eq? (Read-type a) 'RL))
                                                            (and (Write? a) (eq? (Write-type a) 'AQRL))
                                                            (and (Read? a) (eq? (Read-type a) 'AQRL))))
    (list (list-ref ME-atoms (Action-gid a))))))
  
  (define bRcsc
    (make-exact-bound Rcsc (for/list ([a actions] #:when (or (and (Read? a) (eq? (Read-type a) 'RL))
                                                            (and (Write? a) (eq? (Write-type a) 'RL)) 
                                                            (and (Read? a) (eq? (Read-type a) 'AQ))
                                                            (and (Write? a) (eq? (Write-type a) 'AQ))
                                                            (and (Read? a) (eq? (Read-type a) 'AQRL))
                                                            (and (Write? a) (eq? (Write-type a) 'AQRL))
                                                            
                                                            ))
    (list (list-ref ME-atoms (Action-gid a))))))
  
  (define bAcqRel
    (make-exact-bound AcqRel (for/list ([a actions] #:when (or 
                                                            (and (Read? a) (eq? (Read-type a) 'AQRL))
                                                            (and (Write? a) (eq? (Write-type a) 'AQRL))
                                                            
                                                            ))
    (list (list-ref ME-atoms (Action-gid a))))))

  



  (define bInt (make-exact-bound Int (for/list ([i Int-atoms]) (list i))))
  (define bZero (make-exact-bound Zero (list (list (first Int-atoms)))))
  ;;; (displayln "end")
  (bounds U (list bMemoryEvent bProc bLoc bData bPO bPOLoc bAddrDP bDataDP bCtrlDP bfinalValue 
              bM bRead bWrite bFence.rw.rw bFence.rw.r bFence.rw.w bFence.r.rw bFence.r.r
              bFence.r.w bFence.w.rw bFence.w.r bFence.w.w bFence.tso brmw bAmo bXLr bXSc bAQ bRL
              bRcsc bAcqRel bInt bZero bfinalValueNotZero bfinalValueZero
  ;;; (bounds U (list bMemoryEvent bProc bLoc bData bPO bPOLoc bAddrDP bDataDP bCtrlDP bfinalValue
  ;;;             bM bRead bWrite bAmo bXLr bXSc bAQ bRL
  ;;;             bRcsc bAcqRel bInt bZero
  )))


; Canonicalize a litmus test so that it can be represented as relations.
; Each address and value are canonicalized to integers.
(define (canonicalize-program T)
  (define P (litmus-test-program T))

  ; create a map of addresses -> integers
  (define all-locs
    (remove-duplicates
     (for*/list ([thd (Program-threads P)][a (Thread-actions thd)] #:unless (Fence? a)) (Action-addr a))))
  (when (null? all-locs)
    (set! all-locs '(0)))
  (define locs
    (for/hash ([(loc i) (in-indexed all-locs)]) (values loc i)))
  ; create a map of values -> integers
  ; we must ensure 0 is in the map and is first, because it is used for initial values
  (define all-values
    (remove-duplicates
     (for*/list ([thd (Program-threads P)][a (Thread-actions thd)] #:unless (Fence? a)) (Action-val a))))
  (set! all-values (append (list 0) (remove 0 all-values)))
  (define vals
    (for/hash ([(v i) (in-indexed all-values)]) (values v i)))
  ; rewrite the program with integer addresses and values
  (define P*
    (Program
     (for/list ([thd (Program-threads P)])
       (Thread (Thread-tid thd)
        (for/list ([a (Thread-actions thd)])
          (match a
            [(XLrInst gid lid tid addrdeps datadeps ctrldeps addr val type)     (XLrInst gid lid tid addrdeps datadeps ctrldeps (hash-ref locs addr) (hash-ref vals val) type)]
            [(XScInst gid lid tid addrdeps datadeps ctrldeps addr val type)     (XScInst gid lid tid addrdeps datadeps ctrldeps (hash-ref locs addr) (hash-ref vals val) type)]

            [(Read  gid lid tid addrdeps datadeps ctrldeps addr val type)      (Read  gid lid tid addrdeps datadeps ctrldeps (hash-ref locs addr) (hash-ref vals val) type)]
            [(Write gid lid tid addrdeps datadeps ctrldeps addr val type)     (Write gid lid tid addrdeps datadeps ctrldeps (hash-ref locs addr) (hash-ref vals val) type)]
            [(Fence gid lid tid addrdeps datadeps ctrldeps addr val type)      (Fence gid lid tid addrdeps datadeps ctrldeps (first all-locs) (hash-ref vals 0) type)]
            ))))))
  ; the number of integers required
  (define num-ints (max (hash-count locs) (length (Program-threads P*)) (hash-count vals)))
  ; rewrite the test's postcondition using the locs map
  (define post (for/list ([AV (litmus-test-post T)])
                 (cons (hash-ref locs (first AV)) (hash-ref vals (second AV)))))
  (values P* post num-ints))
