(add-to-load-path (getenv "abs_top_srcdir"))

(use-modules (srfi srfi-64)
             (srfi srfi-26)
             (oop goops)
             (tests common)
             (ini fsm-context-ini)
             (ini fsm-context))

(define %test-name "fsm-context")

(configure-test-logging! %test-name)


(test-begin %test-name)

(test-equal "stanza->list-of-strings"
  '("hello" "world")
  (let ((stanza (cons '(#\w #\o #\r #\l #\d)
                      (cons '(#\h #\e #\l #\l #\o) '()))))
    (stanza->list-of-strings stanza)))

(test-equal "guard:comment/read?: #t"
  #t
  (let ((ctx (make <ini-context> #:read-comments? #t)))
    (ini:comment/read? ctx #\;)))

(test-equal "guard:comment/read?: #f"
  #f
  (let ((ctx (make <ini-context> #:read-comments? #f)))
    (ini:comment/read? ctx #\;)))

(test-equal "guard:comment/read?: Custom prefix"
  #t
  (let ((ctx (make <ini-context>
               #:read-comments? #t
               #:comment-prefix #\#)))
    (ini:comment/read? ctx #\#)))

(test-equal "action:start-section"
  '(("test"))
  (let ((ctx (make <ini-context>)))
    (push-event-to-buffer ctx #\t)
    (push-event-to-buffer ctx #\e)
    (push-event-to-buffer ctx #\s)
    (push-event-to-buffer ctx #\t)
    (action:start-section ctx #\nul)
    (context-result ctx)))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end %test-name)

(exit exit-status)
