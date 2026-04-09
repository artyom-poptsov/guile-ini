(add-to-load-path (getenv "abs_top_srcdir"))

(use-modules (srfi srfi-2)
             (srfi srfi-64)
             (srfi srfi-26)
             (oop goops)
             (tests common)
             (ini))

(define %test-name "ini")

(configure-test-logging! %test-name)


(test-begin %test-name)


;; Taken from Wikipedia:
;;   <https://en.wikipedia.org/wiki/INI_file>
(define %test-ini
  "
; last modified 1 April 2001 by John Doe
[owner]
name=John Doe
organization=Acme Widgets Inc.

[database]
; use IP address in case network name resolution is not working
server=192.0.2.62
port=143
file=\"payroll.dat\"
")

;; See
;; <https://github.com/artyom-poptsov/guile-ini/issues/5>
(define %test-ini-custom-comment-prefix
  "
# last modified 1 April 2001 by John Doe
[owner]
name=John Doe
organization=Acme Widgets Inc.

[database]
# use IP address in case network name resolution is not working
server=192.0.2.62
port=143
file=\"payroll.dat\"
")



(test-assert "ini->scm: Don't read comments"
  (with-input-from-string
      %test-ini
    (lambda ()
      (let ((data (ini->scm (current-input-port) #:read-comments? #f)))
        data))))

(test-assert "ini->scm: Read comments"
  (with-input-from-string
      %test-ini
    (lambda ()
      (let* ((data   (ini->scm (current-input-port)))
             (global (cdar data)))
        (and (equal? (caar global) 'comment)
             (equal? (cdar global) " last modified 1 April 2001 by John Doe"))))))

;; Read properties that don't belong to any section.
(test-equal "ini->scm: Read global properties"
  '((#f ("comment" . "Fred's Service")))
  (with-input-from-string
      "comment = Fred's Service\n"
    (lambda ()
      (ini->scm (current-input-port)))))

(test-equal "ini->scm: Read values with single quotes"
  '(("fredsdir" ("comment" . "Fred's Service")))
  (with-input-from-string
      (string-join
       (list
        "[fredsdir]"
        "comment = Fred's Service")
       "\n")
    (lambda ()
      (ini->scm (current-input-port)))))

;; See <https://github.com/artyom-poptsov/guile-ini/issues/6>.
(test-assert "ini->scm: No trailing newline"
  (with-input-from-string
      (string-drop-right %test-ini 1)
    (lambda ()
      (and-let* ((data (ini->scm (current-input-port) #:read-comments? #f))
                 (database (assoc-ref data "database"))
                 (file (assoc-ref database "file")))
        (equal? "\"payroll.dat\"" file)))))

;; See
;; <https://github.com/artyom-poptsov/guile-ini/issues/5>
(test-assert "ini->scm: Custom comment prefix"
  (with-input-from-string
      %test-ini-custom-comment-prefix
    (lambda ()
      (let* ((data   (ini->scm (current-input-port)
                               #:comment-prefix #\#))
             (global (cdar data)))
        (and (equal? (caar global) 'comment)
             (equal? (cdar global) " last modified 1 April 2001 by John Doe"))))))

(test-equal "ini->scm: Comment prefix specified as a character set"
  '((#f (comment . " test 1")
        (comment . " test 2")))
  (with-input-from-string
      (string-append
       "; test 1\n"
       "# test 2\n")
    (lambda ()
      (ini->scm (current-input-port)
                #:comment-prefix (list->char-set '(#\# #\;))))))

(test-equal "scm->ini"
  ;; TODO: Fix the INI generator as currently it adds extra newlines and
  ;;       spaces.
  (string-append
   ";  last modified 1 April 2001 by John Doe\n\n"
   "[owner]\n"
   "name=John Doe\n"
   "organization=Acme Widgets Inc.\n\n"
   "[database]\n"
   ";  use IP address in case network name resolution is not working\n"
   "server=192.0.2.62\n"
   "port=143\n"
   "file=\"payroll.dat\"\n"
   "\n")
  (with-input-from-string
      %test-ini
    (lambda ()
      (let ((data   (ini->scm (current-input-port))))
        (with-output-to-string
          (lambda ()
            (scm->ini data)))))))

(test-equal "scm->ini: Custom comment prefix"
  (string-append
   "#  last modified 1 April 2001 by John Doe\n\n"
   "[owner]\n"
   "name=John Doe\n"
   "organization=Acme Widgets Inc.\n\n"
   "[database]\n"
   "#  use IP address in case network name resolution is not working\n"
   "server=192.0.2.62\n"
   "port=143\n"
   "file=\"payroll.dat\"\n"
   "\n")
  (with-input-from-string
      %test-ini
    (lambda ()
      (let ((data   (ini->scm (current-input-port))))
        (with-output-to-string
          (lambda ()
            (scm->ini data #:comment-prefix #\#)))))))

(test-equal "scm->ini: Skip custom comments"
  (string-append
   "[owner]\n"
   "name=John Doe\n"
   "organization=Acme Widgets Inc.\n\n"
   "[database]\n"
   "server=192.0.2.62\n"
   "port=143\n"
   "file=\"payroll.dat\"\n"
   "\n")
  (with-input-from-string
      %test-ini-custom-comment-prefix
    (lambda ()
      (let ((data (ini->scm (current-input-port)
                            #:comment-prefix #\#
                            #:read-comments? #f)))
        (with-output-to-string
          (lambda ()
            (scm->ini data)))))))


;; Taken from an example "smb.conf" file.

(define %test-ini-smb
  "
#============================ Share Definitions ==============================
[homes]
   comment = Home Directories
   browsable = no
   writable = yes
")

(test-equal "scm->ini: smb.conf: single comment style"
  (string-append
   "# ============================ Share Definitions ==============================\n\n"
   "[homes]\n"
   "comment=Home Directories\n"
   "browsable=no\n"
   "writable=yes\n"
   "\n")
  (with-input-from-string
      %test-ini-smb
    (lambda ()
      (let ((data   (ini->scm (current-input-port)
                              #:comment-prefix #\#)))
        (with-output-to-string
          (lambda ()
            (scm->ini data #:comment-prefix #\#)))))))

(define %test-ini-smb-with-different-comment-styles
  "
#============================ Share Definitions ==============================
[homes]
   comment = Home Directories
   browsable = no
   writable = yes

# Un-comment the following and create the netlogon directory for Domain Logons
; [netlogon]
;   comment = Network Logon Service
;   path = /usr/local/samba/lib/netlogon
;   guest ok = yes
;   writable = no
;   share modes = no
")

;; Read INI-file with mixed comment prefixes.
(test-equal "scm->ini: smb.conf: read a mixed comment style"
  (string-append
   "# ============================ Share Definitions ==============================\n\n"
   "[homes]\n"
   "comment=Home Directories\n"
   "browsable=no\n"
   "writable=yes\n"
   "#  Un-comment the following and create the netlogon directory for Domain Logons\n"
   "#  [netlogon]\n"
   "#    comment = Network Logon Service\n"
   "#    path = /usr/local/samba/lib/netlogon\n"
   "#    guest ok = yes\n"
   "#    writable = no\n"
   "#    share modes = no\n\n")
  (with-input-from-string
      %test-ini-smb-with-different-comment-styles
    (lambda ()
      (let ((data (ini->scm (current-input-port)
                            #:comment-prefix '(#\# #\;))))
        ;; data))))
        (with-output-to-string
          (lambda ()
            (scm->ini data #:comment-prefix #\#)))))))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end %test-name)

(exit exit-status)
