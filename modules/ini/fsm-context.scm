(define-module (ini fsm-context)
  #:use-module (oop goops)
  #:use-module (smc core stack)
  #:use-module (smc context char-context)
  #:re-export (char-context-update-counters!)
  #:export (<ini-context>
            ini-context-result
            stanza->list-of-strings
            guard:comment/read?
            guard:comment?
            action:start-section
            action:append-property
            action:append-comment))

(define-class <ini-context> (<char-context>)
  (read-comments?
   #:init-keyword #:read-comments?
   #:init-value   #t
   #:getter       ini-context-read-comments?)

  (result
   #:init-value '()
   #:getter     ini-context-result
   #:setter     ini-context-result-set!))



(define-method (stanza->list-of-strings (stanza <stack>))
  (map (lambda (elem)
         (list->string elem))
       (stack-content/reversed stanza)))

(define-method (buffer->string (buffer <stack>))
  (list->string (stack-content/reversed buffer)))



(define (guard:comment? ch ctx)
  (char=? ch #\;))

(define (guard:comment/read? ch ctx)
  (and (char=? ch #\;)
       (ini-context-read-comments? ctx)))



(define (action:start-section ch ctx)
  (let ((title (buffer->string (context-buffer ctx))))
    (ini-context-result-set! ctx (cons (list title)
                                       (ini-context-result ctx)))
    (context-buffer-clear! ctx)
    ctx))

(define (action:append-property ch ctx)
  (let ((stanza (stanza->list-of-strings (context-stanza ctx)))
        (buffer (context-buffer ctx))
        (result (ini-context-result ctx)))
    (ini-context-result-set! ctx
                             (cons
                              (append (if (null? result)
                                          (list #f)
                                          (car result))
                                      (list (cons (car stanza)
                                                  (buffer->string buffer))))
                              (if (null? result)
                                  '()
                                  (cdr result))))
    (context-buffer-clear! ctx)
    (context-stanza-clear! ctx)
    ctx))

(define (action:append-comment ch ctx)
  (let ((buffer (context-buffer ctx))
        (result (ini-context-result ctx)))
    (ini-context-result-set! ctx
                             (cons
                              (append (if (null? result)
                                          (list #f)
                                          (car result))
                                      (list (cons 'comment
                                                  (buffer->string buffer))))
                              (if (null? result)
                                  '()
                                  (cdr result))))
    (context-buffer-clear! ctx)
    (context-stanza-clear! ctx)
    ctx))
