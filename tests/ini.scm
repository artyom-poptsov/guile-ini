(add-to-load-path (getenv "abs_top_srcdir"))

(use-modules (srfi srfi-64)
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


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end %test-name)

(exit exit-status)
