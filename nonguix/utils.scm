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
  #:export (package-input-grafting
            package-with-alias
            with-transformation))

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

(define (package-input-grafting replacements)
  "Return a procedure that, when passed a package, grafts its direct and
indirect dependencies, including implicit inputs, according to REPLACEMENTS.
REPLACEMENTS is a list of package pairs; the first element of each pair is the
package to replace, and the second one is the replacement.

Name and version of replacement packages will be padded to meet graft
requirement."
  (package-input-rewriting
   (map (match-lambda
          ((old . new)
           `(,old . ,(package
                       (inherit old)
                       (replacement
                        (package
                          (inherit new)
                          (name
                           (string-pad-right
                            (package-name new)
                            (string-length (package-name old))
                            #\0))
                          (version
                           (string-pad-right
                            (package-version new)
                            (string-length (package-version old))
                            #\0))))))))
        replacements)))

;; For concerns and direction of improvement, see this thread:
;; https://lists.gnu.org/archive/html/guix-devel/2024-06/msg00275.html
(define* (with-transformation proc obj #:optional (pred package?))
  "Recursing into child elements, apply PROC to every element of OBJ that
matches PRED."
  (match obj
    ((? pred)
     (proc obj))
    ;; TODO: Check if this can be handled as well.
    ((? parameter?)
     obj)
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
     (cond
      ;; Both ‘file-systems’ and ‘boot-file-system-utilities’ services extends
      ;; ‘profile-service-type’ with the same package, however information of
      ;; the former one is hidden from us, causing conflict in the resulted
      ;; profile.
      ((and (service? obj)
            (eq? 'boot-file-system-utilities
                 (service-type-name (service-kind obj))))
       obj)
      (else
       (let* ((record-type (record-type-descriptor obj))
              (record-fields (record-type-fields record-type)))
         (apply (record-constructor record-type)
                (map (lambda (field)
                       (let* ((accessor (record-accessor record-type field))
                              (obj (accessor obj)))
                         (with-transformation proc obj pred)))
                     record-fields))))))
    (_ obj)))

(define (package-with-alias alias pkg)
  "Return a copy of package PKG called ALIAS."
  (package
    (inherit pkg)
    (name alias)))
