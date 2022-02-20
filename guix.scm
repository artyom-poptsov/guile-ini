;; guix.scm --- GNU Guix package recipe    -*- coding: utf-8 -*-
;;
;; Copyright (C) 2022 Artyom V. Poptsov <poptsov.artyom@gmail.com>
;;
;; Author: Artyom V. Poptsov <poptsov.artyom@gmail.com>
;; Created: 20 February 2022
;;
;; This file is part of Guile-INI.
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
;;
;; GNU Guix development package. To use as the basis for a development
;; environment, run:
;;
;;  guix environment --pure --container -l guix.scm
;;
;; In the new shell, run:
;;
;;  autoreconf -vif && ./configure && make check
;;
;;; Code:


(use-modules (guix gexp)
             (guix packages)
             (guix licenses)
             (guix git-download)
             (guix build-system gnu)
             (gnu packages autotools)
             (gnu packages guile)
             (gnu packages guile-xyz)
             (gnu packages bash)
             (gnu packages pkg-config)
             (gnu packages texinfo))


(define %source-dir (dirname (current-filename)))


(package
    (name "guile-ini")
    (version "git")
    (source (local-file %source-dir
                        #:recursive? #t
                        #:select? (git-predicate %source-dir)))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags '("GUILE_AUTO_COMPILE=0")))     ;to prevent guild warnings
    (native-inputs
     (list autoconf automake pkg-config texinfo))
    (inputs
     `(("bash" ,bash-minimal)
       ("guile" ,guile-3.0)
       ("guile-lib" ,guile-lib)))
    (propagated-inputs
     (list guile-smc))
    (home-page "https://github.com/artyom-poptsov/guile-ini")
    (synopsis "Guile library for INI format support")
    (description
     "@code{guile-ini} is a GNU Guile library for working with the
@url{https://en.wikipedia.org/wiki/INI_file, INI format}.  This library
provides API for reading and writing INI data.")
    (license gpl3))

;;; guix.scm ends here.
