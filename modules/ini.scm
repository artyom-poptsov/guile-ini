(define-module (ini)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 textual-ports)
  #:use-module (oop goops)
  #:use-module (smc fsm)
  #:use-module (ini fsm)
  #:use-module (ini fsm-implementation)
  #:export (ini->scm))

(define* (ini->scm port #:key (debug-mode? #f))
  (let ((fsm (make <ini-fsm>)))
    (fsm-debug-mode-set! fsm debug-mode?)
    (let loop ((context (make <ini-context>
                          #:module (list (resolve-module '(smc guards char))
                                         (resolve-module '(smc puml))
                                         (resolve-module '(smc fsm))))))
      (receive (new-state new-context)
          (fsm-run! fsm (get-char port) context)
        (if new-state
            (loop new-context)
            (ini-context-result new-context))))))

