;;; ini.scm -- Guile INI format parser.  The main module.

;; Copyright (C) 2021-2023 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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
                   (debug-mode? #f)
                   (log-driver  #f)
                   (log-opt     '()))
  "Read INI data from a PORT and convert it to the scheme representation."

  (when log-driver
    (smc-log-init! log-driver log-opt))

  (let ((fsm (make <ini-fsm> #:debug-mode? debug-mode?)))
    (let ((context (fsm-run! fsm (make <ini-context>
                                   #:port           port
                                   #:read-comments? read-comments?))))
      (when debug-mode?
        (pretty-print ((@ (smc fsm) fsm-statistics) fsm)
                      (current-error-port)))
      (reverse (context-result context)))))



(define (%default-comment-writer comment port)
  "This writer adds '; ' before a COMMENT and writes it to a PORT."
  (format port "; ~a~%" comment))

(define (%write-section section port comment-writer)
  "Write a new INI section to the output port."
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

;;; ini.scm ends here.
