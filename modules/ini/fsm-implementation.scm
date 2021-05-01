(define-module (ini fsm-implementation)
  #:use-module (oop goops)
  #:use-module (smc core stack)
  #:use-module (smc core context)
  #:export (<ini-context>
            ini-context-result
            stanza->list-of-strings
            action:syntax-error
            action:start-section
            action:append-property))

(define-class <ini-context> (<context>)
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



(define (action:syntax-error ch ctx)
  (error "Syntax error"))

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
    (format (current-error-port) "result: ~a~%" result)
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
