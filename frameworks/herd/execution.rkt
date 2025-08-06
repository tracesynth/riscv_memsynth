#lang racket

(require "../../litmus_riscv/litmus.rkt" ocelot
         (rename-in (only-in racket set) [set $set]))
(provide (all-defined-out))
(require (only-in racket/set list->set set-member? set-intersect))

(define rf (declare-relation 2 "rf"))
(define rfi (declare-relation 2 "rfi"))
(define rfe (declare-relation 2 "rfe"))
(define co (declare-relation 2 "co"))
(define coi (declare-relation 2 "coi"))
(define coe (declare-relation 2 "coe"))
(define fr (declare-relation 2 "fr"))
(define fr_init (declare-relation 2 "fr_init"))
(define fri (declare-relation 2 "fri"))
(define fre (declare-relation 2 "fre"))
(define rsw (declare-relation 2 "rsw"))
(define event_loc (declare-relation 2 "event_loc"))
(define event_val (declare-relation 2 "event_val"))
; Instantiate an execution given a set of (possibly symbolic) bounds for a litmus test.
; An execution is two relations rf : Write->Read and co : Write->Write.
; a model M allows a test T if there exists an execution that satisfies the
; ValidExecution predicate.
(define (make-execution bnds)
  ;;; (displayln "bnds: ")
  ;;; (pretty-print bnds)
  (define reads (get-upper-bound bnds R))
  ;;; (displayln "reads: ")
  ;;; (pretty-print reads)
  
  (define writes (get-upper-bound bnds W))
  ;;; (displayln "writes:")
  ;;; (pretty-print writes)
  (define zero (first (first (get-upper-bound bnds Zero))))

  ; map events to set of locs they can touch
  (define loc-ts (get-upper-bound bnds loc))
  (define event->locs
    (for/fold ([ret (hash)]) ([el loc-ts])
      (hash-set ret (first el) (set-add (hash-ref ret (first el) $set) (second el)))))
  (define (locs t) (hash-ref event->locs (first t)))  ; lookup function
  ;;; (displayln "event->locs:")
  ;;; (pretty-print event->locs)

  ; map events to proc id
  (define pid-ts (get-upper-bound bnds proc))
  (define event->pids
    (for/fold ([ret (hash)]) ([el pid-ts])
      (hash-set ret (first el) (set-add (hash-ref ret (first el) $set) (second el)))))
  (define (pids t) (hash-ref event->pids t)) ; notes: different from other
  ; map events to set of values they read/write
  (define val-ts (get-upper-bound bnds data))
  (define event->vals
    (for/fold ([ret (hash)]) ([el val-ts])
      (hash-set ret (first el) (set-add (hash-ref ret (first el) $set) (second el)))))
  ;;; (displayln "event->vals:")
  ;;; (pretty-print event->vals)
  (define (vals t) (hash-ref event->vals (first t)))  ; lookup function
  ; map locs to their possible final values
  (define fv-ts (get-upper-bound bnds finalValue))
  (define loc->fv
    (for/fold ([ret (hash)]) ([el fv-ts])
      (hash-set ret (first el) (set-add (hash-ref ret (first el) $set) (second el)))))
  (define (fvs l) (hash-ref loc->fv l ($set)))
  ; map (loc, val) pairs to set of all write events that can generate that pair
  (define lv->writes
    (for*/fold ([ret (hash)]) ([(w ls) event->locs] #:when (member (list w) writes)
                               [v (hash-ref event->vals w '())]
                               [l ls])
      (hash-set ret (cons l v) (set-add (hash-ref ret (cons l v) $set) w))))
  ;;; (displayln "lv->writes:")
  ;;; (pretty-print lv->writes)
  (define (writes-visible r)
    (let ([actual (for*/set ([l (locs r)][v (vals r)][w (hash-ref lv->writes (cons l v) '())])
                    w)])
      (if (set-member? (vals r) zero) (set-add actual 'init) actual)))

  (define event_loc_U
    (for*/list ([event-id (in-hash-keys event->locs)]
                [loc (in-set (hash-ref event->locs event-id))])
      (list event-id loc)))
  ;;; (displayln "event_loc_U:")
  ;;; (pretty-print event_loc_U)

  (define event_loc_L
    (for*/list ([event-id (in-hash-keys event->locs)]
                [loc (in-set (hash-ref event->locs event-id))])
      (list event-id loc)))
  ;;; (displayln "event_loc_L:")
  ;;; (pretty-print event_loc_L)
  (define bevent_loc (make-bound event_loc event_loc_L event_loc_U))

  (define event_val_U
    (for*/list ([event-id (in-hash-keys event->vals)]
                [val (in-set (hash-ref event->vals event-id))])
      (list event-id val)))
  ;;; (displayln "event_val_U:")
  ;;; (pretty-print event_val_U)

  (define event_val_L
    (for*/list ([event-id (in-hash-keys event->vals)]
                [val (in-set (hash-ref event->vals event-id))])
      (list event-id val)))
  ;;; (displayln "event_val_L:")
  ;;; (pretty-print event_val_L)
  (define bevent_val (make-bound event_val event_val_L event_val_U))
  
  ; create bounds for rf ⊂ (loc.~loc) & (val.~val)
  (define rf_U (for*/list ([w writes][r reads]
                           #:when (and 
                                       (not (equal? (first w) (first r)))
                                       (not (set-empty? (set-intersect (locs w) (locs r))))
                                       (not (set-empty? (set-intersect (vals w) (vals r))))))
                 (list (first w) (first r))))
  (define rf_L (for*/list ([r reads] #:when (and (= (set-count (writes-visible r)) 1)
                                                 (not (equal? (set-first (writes-visible r)) (first r)))
                                                 (not (eq? (set-first (writes-visible r)) 'init))))
                 (list (set-first (writes-visible r)) (first r))))
  (define brf (make-bound rf rf_L rf_U))

  ;;; read from init
  (define rf_targets_L (list->set (map second rf_L)))

  (define rf_targets_U (list->set (map second rf_U)))

  (define rf_init_L
      (filter (lambda (r) (not (set-member? rf_targets_U (first r))))
          reads))
  ;;; (displayln "read from init L")
  ;;; (pretty-print rf_init_L)
  (define rf_init_U
      (filter (lambda (r) (not (set-member? rf_targets_L (first r))))
          reads))
  ;;; (displayln "read from init U")
  ;;; (pretty-print rf_init_U)
  ;;; fr to init
  (define fr_init_L (for*/list ([w writes][r rf_init_L]
                          #:when (and 
                                      (not (equal? (first w) (first r)))
                                      (not (set-empty? (set-intersect (locs w) (locs r))))))
                (list (first r) (first w))))
  (define fr_init_U (for*/list ([w writes][r rf_init_U]
                          #:when (and 
                                      (not (equal? (first w) (first r)))
                                      (not (set-empty? (set-intersect (locs w) (locs r))))))
                (list (first r) (first w))))
  ;;; (displayln "from read init L")
  ;;; (pretty-print fr_init_L)
  ;;; (displayln "from read init U")
  ;;; (pretty-print fr_init_U)
    ;;; fri to init
  (define fri_init_L (for*/list ([w writes][r rf_init_L]
                          #:when (and 
                                      (not (equal? (first w) (first r)))
                                      (equal? (pids (first w)) (pids (first r)))
                                      (not (set-empty? (set-intersect (locs w) (locs r))))))
                (list (first r) (first w))))
  (define fri_init_U (for*/list ([w writes][r rf_init_U]
                          #:when (and 
                                      (not (equal? (first w) (first r)))
                                      (equal? (pids (first w)) (pids (first r)))
                                      (not (set-empty? (set-intersect (locs w) (locs r))))))
                (list (first r) (first w))))
  ;;; (displayln "from read initi L")
  ;;; (pretty-print fri_init_L)
  ;;; (displayln "from read initi U")
  ;;; (pretty-print fri_init_U)
    ;;; fre to init
  (define fre_init_L (for*/list ([w writes][r rf_init_L]
                          #:when (and 
                                      (not (equal? (first w) (first r)))
                                      (not (equal? (pids (first w)) (pids (first r))))
                                      (not (set-empty? (set-intersect (locs w) (locs r))))))
                (list (first r) (first w))))
  (define fre_init_U (for*/list ([w writes][r rf_init_U]
                          #:when (and 
                                      (not (equal? (first w) (first r)))
                                      (not (equal? (pids (first w)) (pids (first r))))
                                      (not (set-empty? (set-intersect (locs w) (locs r))))))
                (list (first r) (first w))))
  ;;; (displayln "from read inite L")
  ;;; (pretty-print fre_init_L)
  ;;; (displayln "from read inite U")
  ;;; (pretty-print fre_init_U)

  (define bfr_init (make-bound fr_init fr_init_L fr_init_U))
  
  ;;; rfi
  (define rfi_U (for*/list ([w writes][r reads]
                           #:when (and 
                                       (equal? (pids (first w)) (pids (first r)))
                                       (not (equal? (first w) (first r)))
                                       (not (set-empty? (set-intersect (locs w) (locs r))))
                                       (not (set-empty? (set-intersect (vals w) (vals r))))))

                 (list (first w) (first r))))
  ;;; (displayln "rfi_U:")
  ;;; (pretty-print rfi_U)
  (define rfi_L (for*/list ([r reads] #:when (and 
                                                 (= (set-count (writes-visible r)) 1)
                                                 (not (equal? (set-first (writes-visible r)) (first r)))
                                                 (not (eq? (set-first (writes-visible r)) 'init))
                                                 (equal? (pids (set-first (writes-visible r))) (pids (first r)))))
                  ;;;  (displayln "rfi_L:")
                  ;;;  (pretty-print (set-first (writes-visible r))) 
                 (list (set-first (writes-visible r)) (first r))))
  (define brfi (make-bound rfi rfi_L rfi_U))
  
  rfe
  (define rfe_U (for*/list ([w writes][r reads]
                           #:when (and 
                                       (not (equal? (pids (first w)) (pids (first r))))
                                       (not (equal? (first w) (first r)))
                                       (not (set-empty? (set-intersect (locs w) (locs r))))
                                       (not (set-empty? (set-intersect (vals w) (vals r))))))
                 (list (first w) (first r))))
  (define rfe_L (for*/list ([r reads] #:when (and 
                                                 (= (set-count (writes-visible r)) 1)
                                                 (not (equal? (set-first (writes-visible r)) (first r)))
                                                 (not (eq? (set-first (writes-visible r)) 'init))
                                                 (not (equal? (pids (set-first (writes-visible r))) (pids (first r))))))
                  ;;;  (displayln "rfi_L:")
                  ;;;  (pretty-print (set-first (writes-visible r))) 
                 (list (set-first (writes-visible r)) (first r))))
  (define brfe (make-bound rfe rfe_L rfe_U))
  

  ; create bounds for co ⊂ (loc.~loc)-iden
  ; we also handle finalValues here:
  ;  all disj a,b: Write { loc[a] = loc[b] and data[a] = finalValue[loc[a]] and data[b] != finalValue[loc[b]]
  ;                         => a->b not in co }
  ;  in other words,
  ;  we must allow w1->w2 ∈ co if there is some location that w2 can write to that either
  ;  has no finalValue (so any value is allowed) or that has a finalValue w2 can write.
  (define co_U (for*/list ([w1 writes][w2 writes]
                           #:when (and (not (set-empty? (set-intersect (locs w1) (locs w2))))
                                       (not (equal? w1 w2))
                                       (or 
                                        (for/or ([l (locs w2)])  ; if there is some location w2 can write ...
                                          (or (set-empty? (fvs l))  ; that either has no finalValue
                                              (not (set-empty? (set-intersect (vals w2) (fvs l))))))
                                        (for/or ([l (locs w1)])
                                          (or (set-empty? (fvs l))
                                              (set-empty? (set-intersect (vals w1) (fvs l)))
                                          )
                                        )
                                       )
                                  ))  ; or has a finalValue w2 can write
                 (list (first w1) (first w2))))
  ; if only one write event w can write the final value to a given location,
  ; then every write event that definitely writes to that same location must
  ; happen before w
  (define co_L (for*/list ([w writes] #:when (and (= (set-count (locs w)) 1)
                                                  (= (set-count (vals w)) 1)
                                                  (equal? (fvs (set-first (locs w))) (vals w)))
                           [w2 writes] #:when (and (not (equal? w w2)) (equal? (locs w) (locs w2))))
                 (list (first w2) (first w))))
  (define bco (make-bound co co_L co_U))
  ;;; coi
  (define coi_U (for*/list ([w1 writes][w2 writes]
                           #:when (and (not (set-empty? (set-intersect (locs w1) (locs w2))))
                                       (equal? (pids (first w1)) (pids (first w2)))
                                       (not (equal? w1 w2))
                                       (for/or ([l (locs w2)])  ; if there is some location w2 can write ...
                                         (or (set-empty? (fvs l))  ; that either has no finalValue
                                             (not (set-empty? (set-intersect (vals w2) (fvs l))))))))  ; or has a finalValue w2 can write
                 (list (first w1) (first w2))))
  (define coi_L (for*/list ([w writes] #:when (and (= (set-count (locs w)) 1)
                                                  (= (set-count (vals w)) 1)
                                                  (equal? (fvs (set-first (locs w))) (vals w)))
                           [w2 writes] #:when (and (not (equal? w w2)) 
                                                   (equal? (locs w) (locs w2))
                                                   (equal? (pids (first w)) (pids (first w2)))
                                                   ))
                 (list (first w2) (first w))))
  (define bcoi (make-bound coi coi_L coi_U))
  
  ;;; coe
  (define coe_U (for*/list ([w1 writes][w2 writes]
                           #:when (and (not (set-empty? (set-intersect (locs w1) (locs w2))))
                                       (not (equal? (pids (first w1)) (pids (first w2))))
                                       (not (equal? w1 w2))
                                       (for/or ([l (locs w2)])  ; if there is some location w2 can write ...
                                         (or (set-empty? (fvs l))  ; that either has no finalValue
                                             (not (set-empty? (set-intersect (vals w2) (fvs l))))))))  ; or has a finalValue w2 can write
                 (list (first w1) (first w2))))
  (define coe_L (for*/list ([w writes] #:when (and (= (set-count (locs w)) 1)
                                                  (= (set-count (vals w)) 1)
                                                  (equal? (fvs (set-first (locs w))) (vals w)))
                           [w2 writes] #:when (and (not (equal? w w2)) 
                                                   (equal? (locs w) (locs w2))
                                                   (not (equal? (pids (first w)) (pids (first w2))))
                                                   ))
                 (list (first w2) (first w))))
  (define bcoe (make-bound coe coe_L coe_U)) 
  ;;; fr
  (define fr_L (for*/list ([co1 co_L][rf1 rf_L]
                            #:when (and 
                            (equal? (first co1) (first rf1))
                            ))
                (list (second rf1) (second co1))))

  (define fr_U (for*/list ([co1 co_U][rf1 rf_U]
                          #:when (and 
                          (equal? (first co1) (first rf1))
                          ))
              (list (second rf1) (second co1))))
  (define bfr (make-bound fr (set-union fr_L fr_init_L) (set-union fr_U fr_init_U))) 

    ;;; fri
  (define fri_L (for*/list ([co1 co_L][rf1 rf_L]
                            #:when (and 
                            (equal? (first co1) (first rf1))
                            (equal? (pids (second rf1)) (pids (second co1)))
                            ))
                (list (second rf1) (second co1))))

  (define fri_U (for*/list ([co1 co_U][rf1 rf_U]
                          #:when (and 
                          (equal? (first co1) (first rf1))
                         (equal? (pids (second rf1)) (pids (second co1)))
                          ))
              (list (second rf1) (second co1))))
  (define bfri (make-bound fri (set-union fri_L fri_init_L) (set-union fri_U fri_init_L))) 

    ;;; fre
  (define fre_L (for*/list ([co1 co_L][rf1 rf_L]
                            #:when (and 
                            (equal? (first co1) (first rf1))
                           (not (equal? (pids (second rf1)) (pids (second co1))))
                            ))
                (list (second rf1) (second co1))))

  (define fre_U (for*/list ([co1 co_U][rf1 rf_U]
                          #:when (and 
                          (equal? (first co1) (first rf1))
                          (not (equal? (pids (second rf1)) (pids (second co1))))
                          ))
              (list (second rf1) (second co1))))
  (define bfre (make-bound fre (set-union fre_L fre_init_L) (set-union fre_U fre_init_U))) 

  ;;; rsw
  (define rsw_L (for*/list ([rf1 rf_L][rf2 rf_L]
                          #:when (and 
                          (equal? (first rf1) (first rf2))
                          ;;; (equal? (pids (second rf1)) (pids (second rf2)))
                          (not (equal? (second rf1) (second rf2)))
                          ))
              (list (second rf1) (second rf2))))

  (define rsw_U (for*/list ([rf1 rf_U][rf2 rf_U]
                          #:when (and 
                          (equal? (first rf1) (first rf2))
                          ;;; (equal? (pids (second rf1)) (pids (second rf2)))
                          (not (equal? (second rf1) (second rf2)))
                          ))
              (list (second rf1) (second rf2))))
  (define brsw (make-bound rsw rsw_L rsw_U)) 

  
  ;;; (bounds (bounds-universe bnds) (list brf bco brfi brfe bcoi bcoe bfri bfre)))

  (bounds (bounds-universe bnds) (list brf bco brfi brfe bcoi bcoe bfr bfri bfre brsw bfr_init bevent_loc bevent_val)))
