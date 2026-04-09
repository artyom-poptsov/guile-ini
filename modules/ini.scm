;;; ini.scm -- Guile INI format parser.  The main module.

;; Copyright (C) 2021-2026 Artyom V. Poptsov <poptsov.artyom@gmail.com>
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; The program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with the program.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; This module contains procedures to read data in INI format to scheme
;; representation and vice versa.


;;; Code:

(define-module (ini)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (oop goops)
  #:use-module (ini fsm)
  #:use-module (ini fsm-context-ini)
  #:use-module (ini fsm-context)
  #:export (ini->scm
            scm->ini
            %default-comment-writer))


(define* (ini->scm port
                   #:key
                   (read-comments? #t)
                   (comment-prefix %default-comment-prefix)
                   (debug-mode? #f)
                   (log-driver  #f)
                   (log-opt     '()))
  "Read INI data from a PORT and convert it to the scheme representation.
Optional arguments include:
- READ-COMMENTS? (boolean) controls if the commentaries from the input data
must be read or not (#t by default);
- COMMENT-PREFIX (char or a list or a character set) controls the commentary
prefix for the input data (';' by default);
- DEBUG-MODE? (boolean) enables or disables the debug mode (#f by default);
- LOG-DRIVER (boolean) sets the logging driver (#f by default); LOG-OPT (list)
sets the logger options (an empty list by default.)"

  (when log-driver
    (smc-log-init! log-driver log-opt))

  (let ((fsm (make <ini-fsm> #:debug-mode? debug-mode?))
        (comment-prefix
         (cond
          ((char? comment-prefix)
           comment-prefix)
          ((list? comment-prefix)
           (list->char-set comment-prefix))
          ((character-set? comment-prefix)
           comment-prefix)
          (else
           (throw 'guile-ini-error
                  "Wrong prefix type (expecting a symbol or a list)"
                  comment-prefix)))))
    (let ((context (fsm-run! fsm (make <ini-context>
                                   #:comment-prefix comment-prefix
                                   #:port           port
                                   #:read-comments? read-comments?))))
      (when debug-mode?
        (pretty-print ((@ (smc fsm) fsm-statistics) fsm)
                      (current-error-port)))
      (reverse (context-result context)))))



(define (%default-comment-writer prefix comment port)
  "This writer adds a comment PREFIX before a COMMENT and writes it to a PORT."
  (format port "~a ~a~%" prefix comment))

(define (%write-section section port comment-prefix comment-writer)
  "Write a new INI SECTION to the output PORT with the specified COMMENT-PREFIX
and COMMENT-WRITER."
  (let ((title (car section))
        (props (cdr section)))
    (when title
      (format port "[~a]~%" title))
    (for-each (lambda (prop)
                (cond
                 ((string? (car prop))
                  (format port "~a=~a~%" (car prop) (cdr prop)))
                 ((equal? (car prop) 'comment)
                  (comment-writer comment-prefix (cdr prop) port))
                 ((equal? (car prop) 'newline)
                  (newline port))
                 (else
                  (error "Unknown property type" prop))))
              props))
  (newline port))

(define* (scm->ini data
                   #:key
                   (port           (current-output-port))
                   (comment-prefix %default-comment-prefix)
                   (comment-writer %default-comment-writer))
  "Write DATA to a PORT in the INI format.  Optional parameter COMMENT-PREFIX
specifies the comment prefix (a character) that will be used to write
comments.  The behavior of the INI writer in relation to comments can be
changed by specifying a custom COMMENT-WRITER.  A COMMENT-WRITER is called as
follows:
  (comment-writer prefix comment port)

Where PREFIX is the comment prefix specified to 'scm->ini' procedure, COMMENT
is the comment string to write and PORT is a port to write the comment to."
  (let* ((global (find (lambda (section)
                         (not (car section)))
                       data))
         (data   (if global
                     (delete global data equal?)
                     data)))

    (when global
      (%write-section global port comment-prefix comment-writer))

    (for-each (lambda (section)
                (%write-section section port comment-prefix comment-writer))
              data)))

;;; ini.scm ends here.
