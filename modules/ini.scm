(define-module (ini)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (oop goops)
  #:use-module (smc fsm)
  #:use-module (ini fsm)
  #:use-module (ini fsm-context)
  #:export (ini->scm
            scm->ini
            %default-comment-writer))

(define* (ini->scm port
                   #:key
                   (read-comments? #t)
                   (debug-mode? #f))
  (let ((fsm (make <ini-fsm>)))
    (fsm-debug-mode-set! fsm debug-mode?)
    (let loop ((context (make <ini-context>
                          #:read-comments? read-comments?
                          #:module (list (resolve-module '(smc guards char))
                                         (resolve-module '(smc puml))
                                         (resolve-module '(smc fsm))))))
      (receive (new-state new-context)
          (fsm-run! fsm (get-char port) context)
        (if new-state
            (loop new-context)
            (begin
              (when debug-mode?
                (pretty-print (fsm-statistics fsm) (current-error-port)))
              (reverse (ini-context-result new-context))))))))



(define (%default-comment-writer comment port)
  "This writer adds '; ' before a COMMENT and writes it to a PORT."
  (format port "; ~a~%" comment))

(define (%write-section section port comment-writer)
  (let ((title (car section))
        (props (cdr section)))
    (when title
      (format port "[~a]~%" title))
    (for-each (lambda (prop)
                (cond
                 ((string? (car prop))
                  (format port "~a=~a~%" (car prop) (cdr prop)))
                 ((equal? (car prop) 'comment)
                  (comment-writer (cdr prop) port))
                 ((equal? (car prop) 'newline)
                  (newline port))
                 (else
                  (error "Unknown property type" prop))))
              props))
  (newline port))

(define* (scm->ini data
                   #:key
                   (port           (current-output-port))
                   (comment-writer %default-comment-writer))
  "Write DATA to a PORT in the INI format. "
  (let* ((global (find (lambda (section)
                         (not (car section)))
                       data))
         (data   (if global
                     (delete global data equal?)
                     data)))

    (when global
      (%write-section global port comment-writer))

    (for-each (lambda (section)
                (%write-section section port comment-writer))
              data)))
