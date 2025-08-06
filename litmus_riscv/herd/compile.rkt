#lang racket

(require "parse.rkt" "../lang.rkt")
(provide compile/riscv litmus-file->test)


;; Given a string containing a Herd-formatting litmus test,
;; convert it into our litmus-test struct.
;; This won't populate the litmus-test's allowed list.


;;; amoswap.w.aq.rl
;;; lr.w.aq.rl
;;; lw
;;; xor
;;; add
;;; sw
;;; fence
;;; bne
;;; ori
;;; andi ×
;;; amoadd.w
;;; amoadd.w.rl
;;; amoadd.w.aq
;;; amoswap.w
;;; sd
;;; ld
;;; li
;;; amoor.d.aq
;;; fence.tso
;;; lr.w
;;; sc.w
;;; amoswap.w.rl
;;; beq
;;; amoor.w.aq.rl
;;; amoswap.w.aq
;;; addi
;;; amoadd.w.aq.rl
;;; lr.d
;;; sc.d
;;; amoswap.d.rl
;;; amoor.w
;;; amoor.w.aq

(define (litmus-file->test contents)
  (define ltest (parse-litmus-test contents))
  (match (string-upcase (litmus-file-cpu ltest))
    ["RISCV" (compile/riscv ltest)]
    [_ (error 'litmus-file->test "unknown architecture ~a" (litmus-file-cpu ltest))]))


(define (ssplit s [sep #px"\\s+"])
  (map string-trim (string-split s sep)))

(define (ssubstr s n [k (string-length s)])
  (string-trim (substring s n k)))


;; Compile a litmus-file struct into a litmus test,
;; treating the litmus-file as a Riscv litmus test.
(define (compile/riscv test)
  (define pre (litmus-file-pre test))
  (define insns (litmus-file-insns test))
  (define post (litmus-file-post test))
  (define allowed (litmus-file-allowed test))

  (define procs (sort (hash-keys insns) <))
  (define gid (let ([g 0]) (thunk (begin0 g (set! g (add1 g))))))

  (define prog
    (Program
     (for/list ([(p tid) (in-indexed procs)])
       (define code (hash-ref insns p))
       (define lid (let ([g 0]) (thunk (begin0 g (set! g (add1 g))))))

       (define regs ; my registers
         (make-hash (for/list ([(r v) (hash-ref pre p '())]) (cons r (list v '() #f)))))
       (hash-set! regs '"x0" (list 0 '() #f))
       (define mem (make-hash)) ; local memory
       (Thread tid
               (for/fold ([ir '()]) ([insn code])
                 (match-define (list op args)
                   (map string-trim
                        (let ([i (string-index insn #\ )])
                          (if i
                              (list (substring insn 0 i) (substring insn (add1 i)))
                              (list insn "")))))

                 (match op
                    ;;; li rd,immedidate
                    ["li" (match-define (list reg val) (ssplit args ","))
                          (hash-set! regs reg (list val '() #f))
                          ir 
                    ]
                    [(or "sw")
                    ;;; sw rs2,offset(rs1)
                      (define-values (addr val addrdeps datadeps ctrldeps)
                        (case op
                          [("sw")  (match-define (list src dst) (ssplit args ","))
                                    (define n (string-index dst #\())
                                    (define off (ssubstr dst 0 n))
                                    (define dst* (ssubstr dst (add1 n) (- (string-length dst) 1)))
                                    ; get src and dst
                                    (match-define (list val datadeps _) (hash-ref regs src))
                                    (match-define (list addr addrdeps _) (hash-ref regs dst*))
                                    (values addr val addrdeps datadeps '())]
                      ))
                      (hash-set! mem addr val)
                      (append ir
                              (list (Write (gid) (lid) p addrdeps datadeps ctrldeps (string->symbol addr) (string->number val) 'P)))
                    ]

                    [(or "sc.w" "sc.w.aq" "sc.w.rl" "sc.w.aq.rl")
                    ;;; sw rs2,offset(rs1)
                      (define-values (flag addr val addrdeps datadeps ctrldeps)
                        (case op
                          [("sc.w") (match-define (list flag src dst) (ssplit args ","))
                                    (define n (string-index dst #\())
                                    (define off (ssubstr dst 0 n))
                                    (define dst* (ssubstr dst (add1 n) (- (string-length dst) 1)))
                                    ; get src and dst
                                    (match-define (list val datadeps _) (hash-ref regs src))
                                    (unless (hash-has-key? (hash-ref post p) flag)
                                        (error 'compile/riscv "don't know how to process sc.w reg not in post"))
                                    (match-define (list addr addrdeps _) (hash-ref regs dst*))
                                    (values flag addr val addrdeps datadeps '())]
                          [("sc.w.aq") (match-define (list flag src dst) (ssplit args ","))
                                    (define n (string-index dst #\())
                                    (define off (ssubstr dst 0 n))
                                    (define dst* (ssubstr dst (add1 n) (- (string-length dst) 1)))
                                    ; get src and dst
                                    (match-define (list val datadeps _) (hash-ref regs src))
                                    (unless (hash-has-key? (hash-ref post p) flag)
                                        (error 'compile/riscv "don't know how to process sc.w reg not in post"))
                                    (match-define (list addr addrdeps _) (hash-ref regs dst*))
                                    (values flag addr val addrdeps datadeps '())]
                          [("sc.w.rl") (match-define (list flag src dst) (ssplit args ","))
                                    (define n (string-index dst #\())
                                    (define off (ssubstr dst 0 n))
                                    (define dst* (ssubstr dst (add1 n) (- (string-length dst) 1)))
                                    ; get src and dst
                                    (match-define (list val datadeps _) (hash-ref regs src))
                                    (unless (hash-has-key? (hash-ref post p) flag)
                                        (error 'compile/riscv "don't know how to process sc.w reg not in post"))
                                    (match-define (list addr addrdeps _) (hash-ref regs dst*))
                                    (values flag addr val addrdeps datadeps '())]
                          [("sc.w.aq.rl") (match-define (list flag src dst) (ssplit args ","))
                                    (define n (string-index dst #\())
                                    (define off (ssubstr dst 0 n))
                                    (define dst* (ssubstr dst (add1 n) (- (string-length dst) 1)))
                                    ; get src and dst
                                    (match-define (list val datadeps _) (hash-ref regs src))
                                    (unless (hash-has-key? (hash-ref post p) flag)
                                        (error 'compile/riscv "don't know how to process sc.w reg not in post"))
                                    (match-define (list addr addrdeps _) (hash-ref regs dst*))
                                    (values flag addr val addrdeps datadeps '())]
                          
                      ))
                      (define flagval (hash-ref (hash-ref post p) flag))
                      ;;; (displayln "flagval")
                      ;;; (pretty-print flagval)
                      (hash-set! mem addr val)
                      (define l (lid))
                      (define type
                       (cond
                         [(equal? op "sc.w") 'P]
                         [(equal? op "sc.w.aq") 'AQ]
                         [(equal? op "sc.w.rl") 'RL]
                         [(equal? op "sc.w.aq.rl") 'AQRL]))
                      (cond [
                        (equal? "0" flagval)
                        
                        (hash-set! regs flag (list 0 '() l))
                        (append ir
                            (list (XScInst (gid) l p addrdeps datadeps ctrldeps (string->symbol addr) (string->number val) type)))
                        ]
                        [else 
                        (hash-set! regs flag (list 1 '() l))
                        ir]
                        ) 
                    ]


                    [(or "amoswap.w" "amoswap.w.aq" "amoswap.w.rl" "amoswap.w.aq.rl")
                     (define-values (flag addr val addrdeps datadeps ctrldeps)
                        (case op
                          [("amoswap.w") (match-define (list flag src dst) (ssplit args ","))
                                         (define n (string-index dst #\())
                                         (define off (ssubstr dst 0 n))
                                         (define dst* (ssubstr dst (add1 n) (- (string-length dst) 1)))
                                         (match-define (list val datadeps _) (hash-ref regs src))
                                          (match-define (list addr addrdeps _) (hash-ref regs dst*))
                                    (values flag addr val addrdeps datadeps '())]
                          [("amoswap.w.aq") (match-define (list flag src dst) (ssplit args ","))
                                         (define n (string-index dst #\())
                                         (define off (ssubstr dst 0 n))
                                         (define dst* (ssubstr dst (add1 n) (- (string-length dst) 1)))
                                         (match-define (list val datadeps _) (hash-ref regs src))
                                          (match-define (list addr addrdeps _) (hash-ref regs dst*))
                                    (values flag addr val addrdeps datadeps '())]
                          [("amoswap.w.rl") (match-define (list flag src dst) (ssplit args ","))
                                        ;;;  (displayln "src")
                                        ;;;  (pretty-print src)
                                        ;;;  (displayln "dst")
                                        ;;;  (pretty-print dst)
                                        ;;;  (displayln "flag")
                                        ;;;  (pretty-print flag)
                                         (define n (string-index dst #\())
                                         (define off (ssubstr dst 0 n))
                                         (define dst* (ssubstr dst (add1 n) (- (string-length dst) 1)))
                                         (match-define (list val datadeps _) (hash-ref regs src))
                                          (match-define (list addr addrdeps _) (hash-ref regs dst*))
                                    (values flag addr val addrdeps datadeps '())]
                          [("amoswap.w.aq.rl") (match-define (list flag src dst) (ssplit args ","))
                                         (define n (string-index dst #\())
                                         (define off (ssubstr dst 0 n))
                                         (define dst* (ssubstr dst (add1 n) (- (string-length dst) 1)))
                                         (match-define (list val datadeps _) (hash-ref regs src))
                                          (match-define (list addr addrdeps _) (hash-ref regs dst*))
                                    (values flag addr val addrdeps datadeps '())]
                          ))

                     (define type
                       (cond
                         [(equal? op "amoswap.w") 'P]
                         [(equal? op "amoswap.w.aq") 'AQ]
                         [(equal? op "amoswap.w.rl") 'RL]
                         [(equal? op "amoswap.w.aq.rl") 'AQRL]))
                    ;;; store part
                     (hash-set! mem addr val)
                    ;;; load part
                    ;;; (displayln "addr")
                    ;;; (pretty-print addr)
                    ;;; (displayln "flag")
                    ;;; (pretty-print flag)
                    (define loadval
                      (cond
                        [(and (hash-has-key? post p)
                              (hash-has-key? (hash-ref post p) flag))
                        (hash-ref (hash-ref post p) flag)]
                        ;;; [(hash-has-key? mem addr)
                        ;;; (hash-ref mem addr)]
                        [else #f]))
                    ;;; (displayln "loadval")
                    ;;;  (pretty-print loadval)
                     (define l (lid))
                    ;;;  (define g (gid))
                     (cond [loadval
                           (hash-set! regs flag (list loadval '() l))
                            (append ir 
                            (list 
                            (Read (gid) l p addrdeps '() ctrldeps (string->symbol addr) (string->number loadval) type)
                            (Write (gid) l p '() datadeps ctrldeps (string->symbol addr) (string->number val) type)
                            ))
                     ][
                      else 
                          (append ir 
                          (list 
                            (Write (gid) l p '() datadeps ctrldeps (string->symbol addr) (string->number val) type)
                            ))
                     ])

                    ;;;  (append ir 
                    ;;;       (list (Write g l p '() datadeps ctrldeps (string->symbol addr) (string->number val) type)))
                    ]
                   [(or "lw")
                  ;;; lw rd,offset(rs1)  
                    (define-values (dst addr addrdeps datadeps ctrldeps)
                      (case op
                        [("lw")  (match-define (list dst src) (ssplit args ","))
                                  (define n (string-index src #\())
                                  (define off (ssubstr src n))
                                  (define src* (ssubstr src (add1 n) (- (string-length src) 1)))
                                  ; get src
                                  (match-define (list addr addrdeps _) (hash-ref regs src*))
                                  (values dst addr addrdeps '() '())]
                        ))
                      (define val
                        (cond [(hash-has-key? (hash-ref post p) dst)
                              (hash-ref (hash-ref post p) dst)]
                              [(hash-has-key? mem addr)
                              (hash-ref mem addr)]
                              [else #f]))
                      ;;; (displayln "val")
                      ;;; (pretty-print val)
                      (cond [val
                            (define l (lid))
                            (hash-set! regs dst (list val addrdeps l))
                            (append ir
                                    (list (Read (gid) l p addrdeps datadeps ctrldeps (string->symbol addr) (string->number val) 'P)))]
                            [else ir])
                    ]
                   [(or "lr.w" "lr.w.aq" "lr.w.rl" "lr.w.aq.rl")
                  ;;; lw rd,offset(rs1)  
                    (define-values (dst addr addrdeps datadeps ctrldeps)
                      (case op
                        [("lr.w")  (match-define (list dst src) (ssplit args ","))
                                  (define n (string-index src #\())
                                  (define off (ssubstr src n))
                                  (define src* (ssubstr src (add1 n) (- (string-length src) 1)))
                                  ; get src
                                  (match-define (list addr addrdeps _) (hash-ref regs src*))
                                  (values dst addr addrdeps '() '())]
                        [("lr.w.aq")  (match-define (list dst src) (ssplit args ","))
                                  (define n (string-index src #\())
                                  (define off (ssubstr src n))
                                  (define src* (ssubstr src (add1 n) (- (string-length src) 1)))
                                  ; get src
                                  (match-define (list addr addrdeps _) (hash-ref regs src*))
                                  (values dst addr addrdeps '() '())]
                        [("lr.w.rl")  (match-define (list dst src) (ssplit args ","))
                                  (define n (string-index src #\())
                                  (define off (ssubstr src n))
                                  (define src* (ssubstr src (add1 n) (- (string-length src) 1)))
                                  ; get src
                                  (match-define (list addr addrdeps _) (hash-ref regs src*))
                                  (values dst addr addrdeps '() '())]
                        [("lr.w.aq.rl")  (match-define (list dst src) (ssplit args ","))
                                  (define n (string-index src #\())
                                  (define off (ssubstr src n))
                                  (define src* (ssubstr src (add1 n) (- (string-length src) 1)))
                                  ; get src
                                  (match-define (list addr addrdeps _) (hash-ref regs src*))
                                  (values dst addr addrdeps '() '())]
                        ))
                      (define type
                       (cond
                         [(equal? op "lr.w") 'P]
                         [(equal? op "lr.w.aq") 'AQ]
                         [(equal? op "lr.w.rl") 'RL]
                         [(equal? op "lr.w.aq.rl") 'AQRL]))
                      (define val
                        (cond [(hash-has-key? (hash-ref post p) dst)
                              (hash-ref (hash-ref post p) dst)]
                              [(hash-has-key? mem addr)
                              (hash-ref mem addr)]
                              [else #f]))
                      (cond [val
                            (define l (lid))
                            (hash-set! regs dst (list val addrdeps l))
                            (append ir
                                    (list (XLrInst (gid) l p addrdeps datadeps ctrldeps (string->symbol addr) (string->number val) type)))]
                            [else ir])
                    ]
                    ;;; xor rd,rs1,rs2 -> xor x7,x5,x5
                    ["xor" (match-define (list rd rs1 rs2) (ssplit args ","))
                            (match-define (list addra adeps asrc) (hash-ref regs rs1))
                            (match-define (list addrb bdeps bsrc) (hash-ref regs rs2))
                            (unless (equal? addra addrb)
                              (error 'compile/riscv "don't know how to xor different addrs"))
                            (define deps (append adeps bdeps (or (list asrc) '())))
                            (hash-set! regs rd (list 0 deps #f))
                            ir] 
                    ;;; add rd,rs1,rs2 -> add x10,x9,x7(x7=0) addr
                    ["add" (match-define (list rd rs1 rs2) (ssplit args ","))
                            (match-define (list addra adeps asrc) (hash-ref regs rs1))
                            (match-define (list addrb bdeps bsrc) (hash-ref regs rs2))
                            (unless (equal? addrb 0)
                              (error 'compile/riscv "don't know how to add addrs and non-zero"))
                            ;;; ref diy7,we can change deps to delete #f
                            ;;; (define deps (append adeps bdeps (or (list asrc) '())))
                            (define deps (append adeps bdeps ))
                            (hash-set! regs rd (list addra deps #f))
                            ir]
                    ;;; ori rd,rs1,immedidate -> ori x7,x7,1 data ori x8,x0,1 value
                    ["ori" (match-define (list rd rs1 rs2) (ssplit args ","))
                            (match-define (list addra adeps asrc) (hash-ref regs rs1))
                            (unless (or (equal? rd rs1) (equal? rs1 '"x0"))
                              (error 'compile/riscv "don't know how to this ori format"))
                            ;;; (define deps (append adeps (or (list asrc) '())))
                            (hash-set! regs rd (list rs2 adeps #f))
                            ir]
                    ["fence"
                          (match-define (list pre suc) (ssplit args ","))
                            (match (list pre suc)
                              [(list "rw" "rw") (append ir (list (Fence (gid) (lid) p '() '() '() 0 0 'fence.rw.rw)))]
                              [(list "rw" "w") (append ir (list (Fence (gid) (lid) p '() '() '() 0 0 'fence.rw.w)))]
                              [(list "rw" "r") (append ir (list (Fence (gid) (lid) p '() '() '() 0 0 'fence.rw.r)))]
                              [(list "r" "rw") (append ir (list (Fence (gid) (lid) p '() '() '() 0 0 'fence.r.rw)))]
                              [(list "r" "w") (append ir (list (Fence (gid) (lid) p '() '() '() 0 0 'fence.r.w)))]
                              [(list "r" "r") (append ir (list (Fence (gid) (lid) p '() '() '() 0 0 'fence.r.r)))]
                              [(list "w" "rw") (append ir (list (Fence (gid) (lid) p '() '() '() 0 0 'fence.w.rw)))]
                              [(list "w" "w") (append ir (list (Fence (gid) (lid) p '() '() '() 0 0 'fence.w.w)))]
                              [(list "w" "r") (append ir (list (Fence (gid) (lid) p '() '() '() 0 0 'fence.w.r)))])
                    ]
                    
                    [_ (error 'compile/riscv "unknown instruction ~v" insn)]))))))
  (define postcond
    (for/list ([(var val) post] #:unless (hash? val))
      (list (string->symbol var) (string->number val))))

  (litmus-test (litmus-file-name test) prog postcond allowed))


;;; (module+ main
;;;   (require racket/cmdline racket/pretty)
;;;   (define path
;;;     (command-line
;;;      #:args (filename)
;;;      filename))
;;;   (pretty-print (litmus-file->test (file->string path))))



