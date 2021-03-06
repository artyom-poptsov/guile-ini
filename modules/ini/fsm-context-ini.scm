;;; fsm-context-ini.scm -- Finite State Machine context for INI parsing.

;; Copyright (C) 2021 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

;; This module contains classes and procedures for a finite state machine that
;; parses data in INI format.


;;; Code:

(define-module (ini fsm-context-ini)
  #:use-module (scheme documentation)
  #:use-module (oop goops)
  #:use-module (ini fsm-context)
  #:re-export (char-context-update-counters!)
  #:export (<ini-context>
            ini-context-result
            buffer->string
            stanza->list-of-strings
            guard:comment/read?
            guard:comment?
            action:start-section
            action:append-property
            action:append-comment))

(define-class-with-docs <ini-context> (<char-context>)
  "This class describes a context for INI parser finite-state machine."

  ;; Whether the parser should read the comments or skip them.
  ;;
  ;; <boolean>
  (read-comments?
   #:init-keyword #:read-comments?
   #:init-value   #t
   #:getter       ini-context-read-comments?)

  ;; The result of parsing.
  ;;
  ;; <list>
  (result
   #:init-value '()
   #:getter     ini-context-result
   #:setter     ini-context-result-set!))



(define-method (stanza->list-of-strings (stanza <list>))
  (map (lambda (elem)
         (list->string elem))
       (reverse stanza)))

(define-method (buffer->string (buffer <list>))
  "Convert a BUFFER to a string."
  (list->string (reverse buffer)))



(define (guard:comment? ctx ch)
  "Check if a character CH is a comment symbol."
  (char=? ch #\;))

(define (guard:comment/read? ctx ch)
  "Check if a character CH is a comment symbol and we must read the comment."
  (and (char=? ch #\;)
       (ini-context-read-comments? ctx)))



(define (action:start-section ctx ch)
  "Start a new section."
  (let ((title (buffer->string (context-buffer ctx))))
    (ini-context-result-set! ctx (cons (list title)
                                       (ini-context-result ctx)))
    (context-buffer-clear! ctx)
    ctx))

(define (action:append-property ctx ch)
  "Append a new property to the parsing result."
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

(define (action:append-comment ctx ch)
  "Append a commentary to the result of parsing."
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

;;; fsm-context-ini.scm ends here.
