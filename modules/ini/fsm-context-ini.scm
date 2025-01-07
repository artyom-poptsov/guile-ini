;;; fsm-context-ini.scm -- Finite State Machine context for INI parsing.

;; Copyright (C) 2021-2025 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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
  #:export (<ini-context>
            stanza->list-of-strings
            ini:comment/read?
            action:start-section
            action:append-property
            action:append-comment

            %default-comment-prefix))


(define-with-docs %default-comment-prefix
  "Default comment prefix for INI format."
  #\;)



(define-class-with-docs <ini-context> (<char-context>)
  "This class describes a context for INI parser finite-state machine."

  ;; The commentary prefix.
  ;;
  ;; <symbol>
  (comment-prefix
   #:init-keyword #:comment-prefix
   #:init-value   %default-comment-prefix
   #:getter       ini-context-comment-prefix)

  ;; Whether the parser should read the comments or skip them.
  ;;
  ;; <boolean>
  (read-comments?
   #:init-keyword #:read-comments?
   #:init-value   #t
   #:getter       ini-context-read-comments?))



(define-method (stanza->list-of-strings (stanza <list>))
  (map (lambda (elem)
         (list->string elem))
       (reverse stanza)))



(define (ini:comment/read? ctx ch)
  "Check if a character CH is a comment symbol and we must read the comment."
  (and (char=? ch (ini-context-comment-prefix ctx))
       (ini-context-read-comments? ctx)))



(define (action:start-section ctx ch)
  "Start a new section."
  (let ((title (list->string (context-buffer/reversed ctx))))
    (clear-buffer (push-event-to-result ctx (list title)))))

(define (action:append-property ctx ch)
  "Append a new property to the parsing result."
  (let ((stanza (stanza->list-of-strings (context-stanza ctx)))
        (buffer (context-buffer/reversed ctx))
        (result (context-result ctx)))
    (context-result-set! ctx
                         (cons
                          (append (if (null? result)
                                      (list #f)
                                      (car result))
                                  (list (cons (car stanza)
                                              (list->string buffer))))
                          (if (null? result)
                              '()
                              (cdr result))))
    (clear-buffer ctx)
    (clear-stanza ctx)))

(define (action:append-comment ctx ch)
  "Append a commentary to the result of parsing."
  (let ((buffer (context-buffer/reversed ctx))
        (result (context-result ctx)))
    (context-result-set! ctx
                         (cons
                          (append (if (null? result)
                                      (list #f)
                                      (car result))
                                  (list (cons 'comment
                                              (list->string buffer))))
                          (if (null? result)
                              '()
                              (cdr result))))
    (clear-buffer ctx)
    (clear-stanza ctx)))

;;; fsm-context-ini.scm ends here.
