(use-modules (srfi srfi-64)
             (srfi srfi-26)
             (oop goops)
             (ini fsm-context-ini)
             (ini fsm-context))

(define %test-name "fsm-context")


(test-begin %test-name)

(test-equal "stanza->list-of-strings"
  '("hello" "world")
  (let ((stanza (cons '(#\w #\o #\r #\l #\d)
                      (cons '(#\h #\e #\l #\l #\o) '()))))
    (stanza->list-of-strings stanza)))

(test-equal "buffer->string"
  "hello"
  (let ((buffer '(#\o #\l #\l #\e #\h)))
    (buffer->string buffer)))

(test-assert "guard:comment?"
  (guard:comment? '() #\;))

(test-assert "guard:comment/read?: #t"
  (let ((ctx (make <ini-context> #:read-comments? #t)))
    (guard:comment? ctx #\;)))

(test-assert "guard:comment/read?: #f"
  (let ((ctx (make <ini-context> #:read-comments? #f)))
    (guard:comment? ctx #\;)))

(test-equal "action:start-section"
  '(("test"))
  (let ((ctx (make <ini-context>)))
    (action:store ctx #\t)
    (action:store ctx #\e)
    (action:store ctx #\s)
    (action:store ctx #\t)
    (action:start-section ctx #\nul)
    (ini-context-result ctx)))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end %test-name)

(exit exit-status)
