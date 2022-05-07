;;; Generated by Guile-SMC 0.4.0
;;; <https://github.com/artyom-poptsov/guile-smc>


(define-module
  (ini fsm-context)
  #:use-module
  (oop goops)
  #:use-module
  (logging logger)
  #:use-module
  (logging rotating-log)
  #:use-module
  (scheme documentation)
  #:use-module
  (ice-9 textual-ports)
  #:use-module
  (ice-9 format)
  #:export
  (<char-context>
    char-context-port
    char-context-counter
    char-context-row
    char-context-col
    char-context-update-counters!
    event-source
    action:syntax-error
    context-log-error
    context-log-warning
    context-log-info
    context-log-debug
    make-char-guard
    make-charset-guard
    <context>
    context?
    context-debug-mode?
    context-debug-mode-set!
    context-stanza
    context-stanza-set!
    context-stanza-add!
    context-stanza-clear!
    context-buffer
    context-buffer-set!
    context-buffer-add!
    context-buffer-clear!
    context-clear!
    #{guard:#t}#
    action:no-op
    action:store
    action:clear-buffer
    action:update-stanza
    <precise-logger>
    <syslog>
    <port-log/us>
    precise-logger?
    port-log/us?
    syslog?
    log-add-handler!
    log-clear-handlers!
    log
    log-error
    log-warning
    log-info
    log-debug
    log-use-stderr!
    %precise-log-formatter
    %precise-log-helper
    define-method-with-docs
    object-address/hex-string
    safe-module-ref
    safe-module-list-ref))


;;; Code from (smc core common)


(define (safe-module-ref module proc-name)
  (catch #t
         (lambda () (module-ref module proc-name))
         (const #f)))

(define (safe-module-list-ref modules proc-name)
  "Try to find a PROC-NAME in a MODULES list.  Return a pair that consists of\na procedure name and a procedure itself when the procedure is found, or #f\notherwise."
  (let loop ((mods modules))
    (if (null? mods)
      #f
      (let ((proc (safe-module-ref (car mods) proc-name)))
        (if proc
          (cons (car mods) proc)
          (loop (cdr mods)))))))

(define (object-address/hex-string object)
  (number->string (object-address object) 16))

(define-macro-with-docs
  (define-method-with-docs
    name-and-args
    docs
    .
    body)
  "Define a method with documentation."
  `(begin
     (define-method ,name-and-args ,@body)
     (set-object-property!
       ,(car name-and-args)
       'documentation
       ,docs)
     *unspecified*))


;;; Code from (smc core log)


(define-class-with-docs
  <precise-logger>
  (<logger>)
  "Guile-SMC precise logger that prints log with microsecond accuracy.")

(define (precise-logger? x)
  "Check if X is a <precise-logger> instance."
  (is-a? x <precise-logger>))

(define-class-with-docs
  <syslog>
  (<log-handler>)
  "This is a log handler which writes logs to the syslog."
  (logger
    #:init-value
    "/gnu/store/1wc9sy8hm0x7l7n93j67c2z82m3rx7ph-inetutils-2.0/bin/logger"
    #:getter
    syslog-logger)
  (tag #:init-keyword #:tag #:getter syslog-tag)
  (use-stderr?
    #:init-value
    #f
    #:init-keyword
    #:use-stderr?
    #:getter
    syslog-use-stderr?
    #:setter
    syslog-use-stderr!))

(define (syslog? x) (is-a? x <syslog>))

(define (%precise-log-formatter lvl time str)
  (with-output-to-string
    (lambda ()
      (display
        (strftime "%F %H:%M:%S" (localtime (car time))))
      (display ".")
      (format #t "~6,'0d" (cdr time))
      (display " (")
      (display (symbol->string lvl))
      (display "): ")
      (display str)
      (newline))))

(define-class-with-docs
  <port-log/us>
  (<log-handler>)
  "Microsecond version of <port-log> from (logging port-log)."
  (port #:init-keyword
        #:port
        #:init-value
        #f
        #:accessor
        port))

(define (port-log/us? x) (is-a? x <port-log/us>))

(define-method
  (initialize (self <port-log/us>) args)
  (next-method)
  (slot-set!
    self
    'formatter
    %precise-log-formatter))

(define (level-enabled? lgr lvl)
  "Check if a log level LVL is enabled for a logger LGR."
  (hashq-ref (slot-ref lgr 'levels) lvl #t))

(define-method
  (%precise-log-helper
    (self <precise-logger>)
    level
    objs)
  (when (level-enabled? self level)
        (let ((cur-time (gettimeofday)))
          (for-each
            (lambda (str)
              (unless
                (string-null? str)
                (for-each
                  (lambda (handler)
                    (accept-log handler level cur-time str))
                  (slot-ref self 'log-handlers))))
            (string-split
              (with-output-to-string
                (lambda ()
                  (for-each (lambda (o) (display o)) objs)))
              #\newline)))))

(define-method
  (log-msg (lgr <precise-logger>) lvl . objs)
  (%precise-log-helper lgr lvl objs))

(define-method
  (emit-log (self <port-log/us>) str)
  (display str (port self)))

(define-method
  (flush-log (self <port-log/us>))
  (force-output (port self)))

(define-method
  (close-log! (self <port-log/us>))
  (close-port (port self))
  (set! (port self) (%make-void-port "w")))

(define-method
  (accept-log (log <syslog>) level time str)
  (let* ((command
           (format
             #f
             "~a ~a -p 'user.~a' -t '~a' '~a'"
             (syslog-logger log)
             (if (syslog-use-stderr? log) "-s" "")
             level
             (syslog-tag log)
             str))
         (result (system command)))
    (unless
      (zero? result)
      (error "Could not log a message"))))

(define-with-docs
  %syslog
  "Default syslog handler instance for Guile-SMC."
  (make <syslog> #:tag "guile-smc"))

(define-with-docs
  %logger
  "Default logger instance for Guile-SMC."
  (make <logger>))

(add-handler!
  %logger
  (make <rotating-log> #:file-name "/tmp/smc.log"))

(set-default-logger! %logger)

(open-log! %logger)

(define-method
  (log-add-handler! (handler <log-handler>))
  (add-handler! %logger handler))

(define-method
  (log-clear-handlers!)
  (slot-set! %logger 'log-handlers '()))

(define-method-with-docs
  (log-use-stderr! (value <boolean>))
  "Enable or disable logging to stderr."
  (syslog-use-stderr! %syslog value))

(define (log level fmt . args)
  (let ((message (apply format #f fmt args)))
    (log-msg %logger level message)))

(define (log-error fmt . args)
  "Log a formatted error message."
  (apply log 'ERROR fmt args))

(define (log-warning fmt . args)
  "Log a formatted warning message."
  (apply log 'WARNING fmt args))

(define (log-info fmt . args)
  "Log a formatted informational message."
  (apply log 'INFO fmt args))

(define (log-debug fmt . args)
  "Log a formatted debug message."
  (apply log 'DEBUG fmt args))


;;; Code from (smc context context)


(define-class
  <context>
  ()
  (debug-mode?
    #:init-value
    #f
    #:init-keyword
    #:debug-mode?
    #:getter
    context-debug-mode?
    #:setter
    context-debug-mode-set!)
  (buffer
    #:init-value
    '()
    #:getter
    context-buffer
    #:setter
    context-buffer-set!)
  (stanza
    #:init-value
    '()
    #:getter
    context-stanza
    #:setter
    context-stanza-set!))

(define-method
  (context? x)
  "Check if an X is a <context> instance."
  (is-a? x <context>))

(define-method
  (context-buffer-clear! (ctx <context>))
  (context-buffer-set! ctx '()))

(define-method
  (context-buffer-add! (ctx <context>) value)
  (context-buffer-set!
    ctx
    (cons value (context-buffer ctx))))

(define-method
  (context-stanza-clear! (ctx <context>))
  (context-stanza-set! ctx '()))

(define-method
  (context-stanza-add! (ctx <context>) value)
  (context-stanza-set!
    ctx
    (cons value (context-stanza ctx))))

(define-method
  (context-clear! (ctx <context>))
  (context-buffer-clear! ctx)
  (context-stanza-clear! ctx))

(define (action:no-op ctx event) ctx)

(define (action:store ctx event)
  (when (context-debug-mode? ctx)
        (log-debug
          "action:store: event: ~a; buffer: ~a"
          event
          (context-buffer ctx)))
  (context-buffer-add! ctx event)
  ctx)

(define (action:clear-buffer ctx event)
  "Clear the context CTX buffer."
  (context-buffer-clear! ctx)
  ctx)

(define (action:update-stanza ctx event)
  (let ((buf (reverse (context-buffer ctx))))
    (unless
      (null? buf)
      (when (context-debug-mode? ctx)
            (let ((stanza (reverse (context-stanza ctx))))
              (log-debug
                "action:update-stanza: event: ~a; buffer: ~a; stanza: ~a"
                event
                buf
                stanza)))
      (context-stanza-add! ctx buf)
      (context-buffer-clear! ctx))
    ctx))

(define (#{guard:#t}# ctx event)
  "This guard is always returns #t."
  #t)


;;; Code from (smc context char-context)


(define-class
  <char-context>
  (<context>)
  (port #:init-keyword
        #:port
        #:getter
        char-context-port)
  (counter
    #:init-value
    0
    #:getter
    char-context-counter
    #:setter
    char-context-counter-set!)
  (col-number
    #:init-value
    0
    #:getter
    char-context-col
    #:setter
    char-context-col-set!)
  (row-number
    #:init-value
    0
    #:getter
    char-context-row
    #:setter
    char-context-row-set!))

(define-method
  (%counter++! (ctx <char-context>))
  (char-context-counter-set!
    ctx
    (+ (char-context-counter ctx) 1)))

(define-method
  (%col++! (ctx <char-context>))
  (char-context-col-set!
    ctx
    (+ (char-context-col ctx) 1)))

(define-method
  (%col-reset! (ctx <char-context>))
  (char-context-col-set! ctx 0))

(define-method
  (%row++! (ctx <char-context>))
  (char-context-row-set!
    ctx
    (+ (char-context-row ctx) 1))
  (%col-reset! ctx))

(define-method
  (char-context-update-counters!
    (ctx <char-context>)
    ch)
  (unless
    (eof-object? ch)
    (%counter++! ctx)
    (%col++! ctx)
    (when (char=? ch #\newline) (%row++! ctx))))

(define-method
  (event-source (context <char-context>))
  (let ((ch (get-char (char-context-port context))))
    (char-context-update-counters! context ch)
    ch))

(define-syntax-rule
  (make-char-guard name ch1)
  (define-public (name ctx ch2) (char=? ch1 ch2)))

(define-syntax-rule
  (make-charset-guard name charset)
  (define-public (name ctx ch)
    (char-set-contains? charset ch)))

(make-char-guard guard:nul? #\nul)

(make-char-guard guard:soh? #\soh)

(make-char-guard guard:stx? #\stx)

(make-char-guard guard:etx? #\etx)

(make-char-guard guard:eot? #\eot)

(make-char-guard guard:enq? #\enq)

(make-char-guard guard:ack? #\ack)

(make-char-guard guard:bel? #\alarm)

(make-char-guard guard:bs? #\backspace)

(make-char-guard guard:tab? #\tab)

(make-char-guard guard:lf? #\newline)

(make-char-guard guard:vt? #\vtab)

(make-char-guard guard:ff? #\page)

(make-char-guard guard:cr? #\return)

(make-char-guard guard:so? #\so)

(make-char-guard guard:si? #\si)

(make-char-guard guard:dle? #\dle)

(make-char-guard guard:dc1? #\dc1)

(make-char-guard guard:dc2? #\dc2)

(make-char-guard guard:dc3? #\dc3)

(make-char-guard guard:dc4? #\dc4)

(make-char-guard guard:nak? #\nak)

(make-char-guard guard:syn? #\syn)

(make-char-guard guard:etb? #\etb)

(make-char-guard guard:can? #\can)

(make-char-guard guard:em? #\em)

(make-char-guard guard:sub? #\sub)

(make-char-guard guard:esc? #\esc)

(make-char-guard guard:fs? #\fs)

(make-char-guard guard:gs? #\gs)

(make-char-guard guard:rs? #\rs)

(make-char-guard guard:us? #\us)

(make-char-guard guard:space? #\space)

(make-char-guard guard:exclamation-mark? #\!)

(make-char-guard guard:double-quote? #\")

(make-char-guard guard:number-sign? #\#)

(make-char-guard guard:dollar-sign? #\$)

(make-char-guard guard:percent-sign? #\%)

(make-char-guard guard:ampersand? #\&)

(make-char-guard guard:single-quote? #\')

(make-char-guard guard:left-parenthesis? #\()

(make-char-guard guard:right-parenthesis? #\))

(make-char-guard guard:asterisk? #\*)

(make-char-guard guard:plus-sign? #\+)

(make-char-guard guard:comma? #\,)

(make-char-guard guard:hyphen-minus? #\-)

(make-char-guard guard:full-stop? #\.)

(make-char-guard guard:solidus? #\/)

(make-char-guard guard:digit-zero? #\0)

(make-char-guard guard:digit-one? #\1)

(make-char-guard guard:digit-two? #\2)

(make-char-guard guard:digit-three? #\3)

(make-char-guard guard:digit-four? #\4)

(make-char-guard guard:digit-five? #\5)

(make-char-guard guard:digit-six? #\6)

(make-char-guard guard:digit-seven? #\7)

(make-char-guard guard:digit-eight? #\8)

(make-char-guard guard:digit-nine? #\9)

(make-char-guard guard:colon? #\:)

(make-char-guard guard:semicolon? #\;)

(make-char-guard guard:less-than-sign? #\<)

(make-char-guard guard:equals-sign? #\=)

(make-char-guard guard:more-than-sign? #\>)

(make-char-guard guard:question-mark? #\?)

(make-char-guard guard:at-symbol? #\@)

(make-char-guard guard:letter-A? #\A)

(make-char-guard guard:letter-B? #\B)

(make-char-guard guard:letter-C? #\C)

(make-char-guard guard:letter-D? #\D)

(make-char-guard guard:letter-E? #\E)

(make-char-guard guard:letter-F? #\F)

(make-char-guard guard:letter-G? #\G)

(make-char-guard guard:letter-H? #\H)

(make-char-guard guard:letter-I? #\I)

(make-char-guard guard:letter-J? #\J)

(make-char-guard guard:letter-K? #\K)

(make-char-guard guard:letter-L? #\L)

(make-char-guard guard:letter-M? #\M)

(make-char-guard guard:letter-N? #\N)

(make-char-guard guard:letter-O? #\O)

(make-char-guard guard:letter-P? #\P)

(make-char-guard guard:letter-Q? #\Q)

(make-char-guard guard:letter-R? #\R)

(make-char-guard guard:letter-S? #\S)

(make-char-guard guard:letter-T? #\T)

(make-char-guard guard:letter-U? #\U)

(make-char-guard guard:letter-V? #\V)

(make-char-guard guard:letter-W? #\W)

(make-char-guard guard:letter-X? #\X)

(make-char-guard guard:letter-Y? #\Y)

(make-char-guard guard:letter-Z? #\Z)

(make-char-guard guard:left-square-bracket? #\[)

(make-char-guard guard:reverse-solidus? #\\)

(make-char-guard guard:right-square-bracket? #\])

(make-char-guard guard:circumflex-accent? #\^)

(make-char-guard guard:low-line? #\_)

(make-char-guard guard:grave-accent? #\`)

(make-char-guard guard:letter-a? #\a)

(make-char-guard guard:letter-b? #\b)

(make-char-guard guard:letter-c? #\c)

(make-char-guard guard:letter-d? #\d)

(make-char-guard guard:letter-e? #\e)

(make-char-guard guard:letter-f? #\f)

(make-char-guard guard:letter-g? #\g)

(make-char-guard guard:letter-h? #\h)

(make-char-guard guard:letter-i? #\i)

(make-char-guard guard:letter-j? #\j)

(make-char-guard guard:letter-k? #\k)

(make-char-guard guard:letter-l? #\l)

(make-char-guard guard:letter-m? #\m)

(make-char-guard guard:letter-n? #\n)

(make-char-guard guard:letter-o? #\o)

(make-char-guard guard:letter-p? #\p)

(make-char-guard guard:letter-q? #\q)

(make-char-guard guard:letter-r? #\r)

(make-char-guard guard:letter-s? #\s)

(make-char-guard guard:letter-t? #\t)

(make-char-guard guard:letter-u? #\u)

(make-char-guard guard:letter-v? #\v)

(make-char-guard guard:letter-w? #\w)

(make-char-guard guard:letter-x? #\x)

(make-char-guard guard:letter-y? #\y)

(make-char-guard guard:letter-z? #\z)

(make-char-guard guard:left-curly-bracket? #\{)

(make-char-guard guard:vertical-line? #\|)

(make-char-guard guard:right-curly-bracket? #\})

(make-char-guard guard:tilde? #\~)

(make-char-guard guard:del? (integer->char 127))

(make-charset-guard
  guard:letter?
  char-set:letter)

(make-charset-guard
  guard:lower-case?
  char-set:lower-case)

(make-charset-guard
  guard:upper-case?
  char-set:upper-case)

(make-charset-guard guard:digit? char-set:digit)

(make-charset-guard
  guard:letter+digit?
  char-set:letter+digit)

(make-charset-guard
  guard:graphic?
  char-set:graphic)

(make-charset-guard
  guard:printing?
  char-set:printing)

(make-charset-guard
  guard:whitespace?
  char-set:whitespace)

(make-charset-guard guard:blank? char-set:blank)

(make-charset-guard
  guard:punctuation?
  char-set:punctuation)

(make-charset-guard
  guard:symbol?
  char-set:symbol)

(make-charset-guard
  guard:hex-digit?
  char-set:hex-digit)

(make-charset-guard guard:ascii? char-set:ascii)

(make-char-guard guard:newline? #\newline)

(define-public (guard:eof-object? ctx ch)
  (eof-object? ch))

(define (action:syntax-error ctx ch)
  (error "Syntax error"
         (char-context-port ctx)
         (char-context-row ctx)
         (char-context-col ctx)
         ch
         ctx))

(define (%current-position-prefix ctx)
  (format
    #f
    "~a:~a:~a: "
    (char-context-port ctx)
    (char-context-row ctx)
    (char-context-col ctx)))

(define (context-log-error ctx fmt . rest)
  (apply log-error
         (string-append
           (%current-position-prefix ctx)
           fmt)
         rest))

(define (context-log-warning ctx fmt . rest)
  (apply log-warning
         (string-append
           (%current-position-prefix ctx)
           fmt)
         rest))

(define (context-log-info ctx fmt . rest)
  (apply log-info
         (string-append
           (%current-position-prefix ctx)
           fmt)
         rest))

(define (context-log-debug ctx fmt . rest)
  (apply log-debug
         (string-append
           (%current-position-prefix ctx)
           fmt)
         rest))



;;; Guile-SMC context ends here.
