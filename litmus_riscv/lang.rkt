#lang s-exp rosette

(provide (all-defined-out))

;; programs --------------------------------------------------------------------

; a program (actually an execution) is a list of actions.
; a thread is a list of actions.
(struct Program (threads) #:transparent)
(struct Thread (tid actions) #:transparent)  ; actions, thread ID
(struct Action (gid lid thd addrdeps datadeps ctrldeps addr val) #:transparent)  ; global ID, thread-local ID, thread ID, local deps
(struct Read Action (type) #:transparent)
(struct Write Action (type) #:transparent)
(struct Fence Action (type) #:transparent)
(struct XLrInst Read () #:transparent) ; an Lr
(struct XScInst Write () #:transparent) ; an Sc

; get all actions in a program
(define (all-actions P)
  (for*/list ([thd (Program-threads P)][act (Thread-actions thd)]) act)) 


;; tests -----------------------------------------------------------------------

; A litmus test consists of a test name, a program? struct, a postcondition
; and list of models that allow the test.
(struct litmus-test (name program post allowed) #:transparent)

(define-syntax define-litmus-test
  (syntax-rules ()
    [(_ name (thd ...))
     (define-litmus-test name (thd ...) #:post () #:allowed)]
    [(_ name (thd ...) #:allowed a ...)
     (define-litmus-test name (thd ...) #:post () #:allowed a ...)]
    [(_ name (thd ...) #:post p)
     (define-litmus-test name (thd ...) #:post p #:allowed)]
    [(_ name (thd ...) #:post p #:allowed a ...)
     (define name (litmus-test 'name (read-test (list 'thd ...)) 'p (list 'a ...)))]))

(define (litmus-test-allowed? model test)
  (or (equal? model 'any)
      (not (false? (member model (litmus-test-allowed test))))))

; parse a test into a program struct
;;; (define (read-test t)
;;;   (define gid 0)
;;;   (Program
;;;    (for/list ([thd t][tid (length t)])
;;;      (Thread tid
;;;              (for/list ([a thd][lid (length thd)])
;;;                (define addrdeps (match a
;;;                               [(list _ _ _ ad _ _ _ _) ad]
;;;                               [_ '()]))
;;;                (define datadeps (match a
;;;                               [(list _ _ _ _ dd _ _ _) dd]
;;;                               [_ '()]))
;;;                (define ctrldeps (match a
;;;                               [(list _ _ _ _ _ cd _ _) cd]
;;;                               [_ '()])) 
;;;                (begin0
;;;                  (match a
;;;                    [(list 'R addr val _ ...) (Read  gid lid tid addrdeps datadeps ctrldeps addr val 'P)]
;;;                    [(list 'W addr val _ ...) (Write gid lid tid addrdeps datadeps ctrldeps addr val 'P)]
;;;                    [(list 'F type)           (Fence gid lid tid '()  '() '() 0    0   type)]
;;;                    [(list 'F)                (Fence gid lid tid '()  '() '() 0    0   'Fence.rw.rw)]
;;;                    [(list 'Amo addr val type _ ...) (Read gid lid tid addrdeps datadeps ctrldeps addr val type)]
;;;                    [(list 'XLr addr val type _ ...) (XLrInst gid lid tid addrdeps datadeps ctrldeps addr val type)]
;;;                    [(list 'XSc addr val type _ ...) (XScInst gid lid tid addrdeps datadeps ctrldeps addr val type)])
;;;                  (set! gid (add1 gid))))))))
(define (read-test t)
  (define gid 0)
  (Program
   (for/list ([thd t] [tid (in-naturals)])
     (Thread tid
             (apply append
                    (for/list ([a thd] [lid (in-naturals)])
                      (define addrdeps (match a
                                        [(list _ _ _ ad _ _ _ _) ad]
                                        [_ '()]))
                      (define datadeps (match a
                                        [(list _ _ _ _ dd _ _ _) dd]
                                        [_ '()]))
                      (define ctrldeps (match a
                                        [(list _ _ _ _ _ cd _ _) cd]
                                        [_ '()]))
                      (define instrs
                        (match a
                          [(list 'R addr val _ ...)
                           (list (Read gid lid tid addrdeps datadeps ctrldeps addr val 'P))]
                          [(list 'W addr val _ ...)
                           (list (Write gid lid tid addrdeps datadeps ctrldeps addr val 'P))]
                          [(list 'F type)
                           (list (Fence gid lid tid '() '() '() 0 0 type))]
                          [(list 'F)
                           (list (Fence gid lid tid '() '() '() 0 0 'Fence.rw.rw))]
                          [(list 'Amo addr val type _ ...)
                           (list
                            (Read  gid         lid tid addrdeps datadeps ctrldeps addr val type)
                            (Write (add1 gid)  lid tid addrdeps datadeps ctrldeps addr val type))]
                          [(list 'XLr addr val type _ ...)
                           (list (XLrInst gid lid tid addrdeps datadeps ctrldeps addr val type))]
                          [(list 'XSc addr val type _ ...)
                           (list (XScInst gid lid tid addrdeps datadeps ctrldeps addr val type))]
                          [_ (error "Unknown instruction" a)]))
                      (set! gid (+ gid (length instrs)))
                      instrs))))))

; export a test into a human-readable string

;;; (define (test->string t)
;;;   (match-define (litmus-test name prog final _) t)
;;;   (define header (format "Test ~a" name))
;;;   (define acts (all-actions prog))
;;;   (define locs
;;;     (remove-duplicates (for/list ([a acts] #:unless (Fence? a)) (Action-addr a))))
;;;   (define post '())

;;;   (define fresh-reg
;;;     (let ([regs '(x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20
;;;                   x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31)])
;;;       (lambda () (begin0 (car regs) (set! regs (cdr regs))))))
;;;   (define insns
;;;     (for/list ([(t tidx) (in-indexed (Program-threads prog))])
;;;       (define dsts (make-hash))
;;;       (for/fold ([is '()]) ([a (Thread-actions t)])
;;;         (match a
;;;           [(Read _ lid _ deps addr val)
;;;            (cond [(null? deps)
;;;                   (define dst (fresh-reg))
;;;                   (set! post (cons (list dst val) post))
;;;                   (hash-set! dsts lid dst)
;;;                   (append is (list (format "~a <- [~a]" dst addr)))]
;;;                  [else
;;;                   (define dep (first deps))
;;;                   (define dep-src (hash-ref dsts dep))
;;;                   (define dep-dst (fresh-reg))
;;;                   (define dst (fresh-reg))
;;;                   (set! post (cons (list dst val) post))
;;;                   (hash-set! dsts lid dst)
;;;                   (append is (list (format "~a <- ~a ^ ~a" dep-dst dep-src dep-src)
;;;                                    (format "~a <- [~a+~a]" dst addr dep-dst)))])]
;;;           [(Write _ _ _ deps addr val)
;;;            (cond [(null? deps)
;;;                   (append is (list (format "~a[~a] <- ~a" (if (Atomic? a) "LOCK " "") addr val)))]
;;;                  [else
;;;                   (define dep (first deps))
;;;                   (define dep-src (hash-ref dsts dep))
;;;                   (define dep-dst (fresh-reg))
;;;                   (append is (list (format "~a <- ~a ^ ~a" dep-dst dep-src dep-src)
;;;                                    (format "~a[~a+~a] <- ~a" (if (Atomic? a) "LOCK " "") addr dep-dst val)))])]
;;;           [(Fence _ _ _ _ _ _ type)
;;;            (append is (list (symbol->string type)))]))))
;;;   (define max-length (apply max (map length insns)))
;;;   (define max-width (apply max (for*/list ([t insns][i t]) (string-length i))))
;;;   (define thds
;;;     (string-join (for/list ([(t i) (in-indexed (Program-threads prog))])
;;;                    (string-append (format "P~a" i) (make-string (- max-width 2) #\ )))
;;;                  " | "))
;;;   (define insn-lines
;;;     (for/list ([i max-length])
;;;       (string-join (for/list ([t insns])
;;;                      (if (< i (length t))
;;;                          (let ([insn (list-ref t i)])
;;;                            (string-append insn (make-string (- max-width (string-length insn)) #\ )))
;;;                          (make-string max-width #\ )))
;;;                    " | ")))

;;;   (define post-mem (for/list ([p final])
;;;                      (format "[~a]=~a" (first p) (second p))))
;;;   (define post-reg (for/list ([p (reverse post)])
;;;                      (match-define (list reg val) p)
;;;                      (format "~a=~a" reg val)))
;;;   (define post-cond
;;;     (string-join (append post-mem post-reg)
;;;                  " /\\ "))
;;;   (define max-line-width
;;;     (let ([lines (append (list header thds) insn-lines (list post-cond))])
;;;       (apply max (map string-length lines))))
;;;   (define ====== (make-string max-line-width #\=))
;;;   (define ------ (make-string max-line-width #\-))
;;;   (string-join (append (list header
;;;                              ======
;;;                              thds
;;;                              ------)
;;;                        insn-lines
;;;                        (list ======
;;;                              post-cond))
;;;                "\n"))
