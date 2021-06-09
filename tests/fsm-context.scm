(use-modules (srfi srfi-64)
             (srfi srfi-26)
             (oop goops)
             (ini fsm-context)
             (smc core stack))

(define %test-name "fsm-context")


(test-begin %test-name)

(test-equal "stanza->list-of-strings"
  '("hello" "world")
  (let ((stack (make <stack>)))
    (stack-push! stack '(#\h #\e #\l #\l #\o))
    (stack-push! stack '(#\w #\o #\r #\l #\d))
    (stanza->list-of-strings stack)))

(test-equal "buffer->string"
  "hello"
  (let ((stack (make <stack>)))
    (stack-push! stack #\h)
    (stack-push! stack #\e)
    (stack-push! stack #\l)
    (stack-push! stack #\l)
    (stack-push! stack #\o)
    (buffer->string stack)))

(test-assert "guard:comment?"
  (guard:comment? #\; '()))

(test-assert "guard:comment/read?: #t"
  (let ((ctx (make <ini-context> #:read-comments? #t)))
    (guard:comment? #\; ctx)))

(test-assert "guard:comment/read?: #f"
  (let ((ctx (make <ini-context> #:read-comments? #f)))
    (guard:comment? #\; ctx)))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end %test-name)

(exit exit-status)
