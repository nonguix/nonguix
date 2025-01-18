;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2019 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2020 Jonathan Brielmaier <jonathan.brielmaier@web.de>

(define-module (nonguix utils)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-43)
  #:use-module (ice-9 match)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 popen)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (gnu services)
  #:export (with-transformation))

(define-public (to32 package64)
  "Build package for i686-linux.
Only x86_64-linux and i686-linux are supported.
- If i686-linux, return the package unchanged.
- If x86_64-linux, return the 32-bit version of the package."
  (match (%current-system)
    ("x86_64-linux"
     (package
       (inherit package64)
       (arguments `(#:system "i686-linux"
                    ,@(package-arguments package64)))))
    (_ package64)))

;; For concerns and direction of improvement, see this thread:
;; https://lists.gnu.org/archive/html/guix-devel/2024-06/msg00275.html
(define* (with-transformation proc obj #:optional (pred package?))
  "Recursing into child elements, apply PROC to every element of OBJ that
matches PRED."
  (match obj
    ((? pred)
     (proc obj))
    ((? procedure?)
     (lambda args
       (apply values
              (map (cut with-transformation proc <> pred)
                   (call-with-values
                       (lambda ()
                         (apply obj args))
                     list)))))
    ((a . b)
     (cons (with-transformation proc a pred)
           (with-transformation proc b pred)))
    ((_ ...)
     (map (cut with-transformation proc <> pred)
          obj))
    (#(_ ...)
     (vector-map (lambda (vec elt)
                   (with-transformation proc elt pred))
                 obj))
    ;; `<service-type>' and `<origin>' record types are expected to not be
    ;; modified. Altering them causes very difficult to debug run-time errors.
    ((or (? service-type?)
         (? origin?))
     obj)
    ((? record?)
     (let* ((record-type (record-type-descriptor obj))
            (record-fields (record-type-fields record-type)))
       (apply (record-constructor record-type)
              (map (lambda (field)
                     (let* ((accessor (record-accessor record-type field))
                            (obj (accessor obj)))
                       (with-transformation proc obj pred)))
                   record-fields))))
    (_ obj)))
