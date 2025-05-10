;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2015, 2018 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2025 Maxim Cournoyer <maxim.cournoyer@gmail.com>

(define-module (nongnu packages)
  #:use-module (gnu packages)
  #:use-module (guix diagnostics)
  #:use-module (guix discovery)
  #:use-module (guix i18n)
  #:use-module (guix memoization)
  #:use-module (guix packages)
  #:use-module (guix ui)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-34)
  #:replace (%patch-path
             search-patch)
  #:export (nongnu-patches
            %nongnu-package-module-path
            all-nongnu-packages))

;;; Commentary:
;;;
;;; This module refines the default value of some parameters from (gnu
;;; packages) and the syntax/procedures using those.  This allows
;;; 'search-paths' and friends to work without any user intervention.
;;;
;;; Code:

(define %nongnu-root-directory
  ;; This is like %distro-root-directory from (gnu packages), with adjusted
  ;; paths.
  (letrec-syntax ((dirname* (syntax-rules ()
                              ((_ file)
                               (dirname file))
                              ((_ file head tail ...)
                               (dirname (dirname* file tail ...)))))
                  (try      (syntax-rules ()
                              ((_ (file things ...) rest ...)
                               (match (search-path %load-path file)
                                 (#f
                                  (try rest ...))
                                 (absolute
                                  (dirname* absolute things ...))))
                              ((_)
                               #f))))
    (try ("nongnu/packages/firmware.scm" nongnu/ packages/)
         ("nongnu/ci.scm" nongnu/))))

(define %nongnu-package-module-path
  `((,%nongnu-root-directory . "nongnu/packages")))

(define %patch-path
  ;; Define it after '%package-module-path' so that '%load-path' contains user
  ;; directories, allowing patches in $GUIX_PACKAGE_PATH to be found.
  (make-parameter
   (map (lambda (directory)
          (if (string=? directory %nongnu-root-directory)
              (string-append directory "/nongnu/packages/patches")
              directory))
        %load-path)))

;;; XXX: The following must be redefined to make use of the overridden
;;; %patch-path parameter above.
(define (search-patch file-name)
  "Search the patch FILE-NAME.  Raise an error if not found."
  (or (search-path (%patch-path) file-name)
      (raise (formatted-message (G_ "~a: patch not found")
                                file-name))))

;;; XXX: `search-patches' being syntax, it can't be overridden by the module
;;; system, or so it seems, so we simply rename it.
(define-syntax-rule (nongnu-patches file-name ...)
  "Return the list of absolute file names corresponding to each
FILE-NAME found in %PATCH-PATH."
  (list (search-patch file-name) ...))

;; Adapted from (@ (gnu packages) all-packages).
(define all-nongnu-packages
  (mlambda ()
    "Return the list of all public packages, including replacements and hidden
packages, excluding superseded packages."
    ;; Note: 'fold-packages' never traverses the same package twice but
    ;; replacements break that (they may or may not be visible to
    ;; 'fold-packages'), hence this hash table to track visited packages.
    (define visited (make-hash-table))

    (fold-packages (lambda (package result)
                     (if (hashq-ref visited package)
                         result
                         (begin
                           (hashq-set! visited package #t)
                           (match (package-replacement package)
                             ((? package? replacement)
                              (hashq-set! visited replacement #t)
                              (cons* replacement package result))
                             (#f
                              (cons package result))))))
                   '()
                   (all-modules %nongnu-package-module-path #:warn warn-about-load-error)
                   ;; Dismiss deprecated packages but keep hidden packages.
                   #:select? (negate package-superseded))))
