#lang rosette

(require "../../../memsynth/log.rkt"
         "../synthesis.rkt"
         "sketch_5.rkt"
         "../../../litmus_riscv/herd/compile.rkt"
         "../../../litmus_riscv/litmus.rkt")
;;; (require "ppo2_alloy/rvwmo-all.rkt")
(require racket/cmdline racket/pretty)
(provide synthesize-RVWMO_0)


;; The reference model to use
(define spec 'RVWMOherd)

(define litmus-files
  (for/list ([p (in-directory "../../../litmus_align_memsynth")]
             #:when (and (file-exists? p)
                         (regexp-match #rx"\\.litmus$" (path->string p))))
    p))

;;; (define litmus-files
;;;   (for/list ([p (in-directory "/home/whq/Desktop/code_list/memsynth/case-studies/synthesis/rvwmo/result_test")]
;;;              #:when (and (file-exists? p)
;;;                          (regexp-match #rx"\\.litmus$" (path->string p))))
;;;     p))
;;; (define allow-values (list '(RVWMOherd) '()))



;;; (define rvwmo-tests
;;;   (for/list ([p litmus-files])
;;;     (litmus-file->test (file->string p))))

;;; (define all-rvwmo-tests
;;;   (for/list ([test rvwmo-tests]
;;;              [allowed allow-values])
;;;     (litmus-test
;;;      (litmus-test-name test)
;;;      (litmus-test-program test)
;;;      (litmus-test-post test)
;;;      allowed)))

(define all-rvwmo-tests
  (for/list ([p litmus-files])
    (litmus-file->test (file->string p))))

;; The tests to use
(define tests (sort all-rvwmo-tests < #:key (lambda (T) (length (all-actions (litmus-test-program T))))))


(displayln "tests")
(pretty-print tests)

;; The sketch to use
(define sketch rvwmo-sketch)


;; Synthesize RVWMO_0
(define (synthesize-RVWMO_0)
  (run-synthesis-experiment spec tests sketch))


;; Run the synthesis
(module+ main
  (when (vector-member "-v" (current-command-line-arguments))
    (log-types '(synth)))
  (printf "===== RVWMO_0: synthesis experiment =====\n")
  (define synth-start-time (current-seconds))
  (printf "synth-start-time  ~a\n" synth-start-time )
  (define RVWMO_0 (synthesize-RVWMO_0)))
  (define synth-end-time (current-seconds))
  (printf "synth-end-time ~a\n" synth-end-time)