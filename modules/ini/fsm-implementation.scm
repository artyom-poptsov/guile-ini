(define-module (ini fsm-implementation)
  #:use-module (oop goops)
  #:use-module (smc core stack)
  #:use-module (smc core context)
  #:export (<ini-context>
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
    ctx))

(define (action:append-property ch ctx)
  (let ((stanza (stanza->list-of-strings (context-stanza ctx)))
        (result (ini-context-result ctx)))
    (ini-context-result-set! ctx
                             (cons
                              (append (car result)
                                      (list (cons (list-ref stanza 0)
                                                  (list-ref stanza 1))))
                              (cdr result)))
    ctx))
