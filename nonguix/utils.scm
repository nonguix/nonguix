;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2019 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2020 Jonathan Brielmaier <jonathan.brielmaier@web.de>

(define-module (nonguix utils)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-43)
  #:use-module (ice-9 match)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 popen)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix platform)
  #:use-module (guix build-system trivial)
  #:use-module (gnu services)
  #:use-module (gnu packages base)
  #:export (package-input-grafting
            package-with-alias
            with-transformation

            %binary-source
            binary-package-from-sources))

(define-public (to32 package64)
  "Build package for i686-linux.
Only x86_64-linux and i686-linux are supported.
- If i686-linux, return the package unchanged.
- If x86_64-linux, return the 32-bit version of the package."
  (match (%current-system)
    ("x86_64-linux"
     (package
       (inherit package64)
       (arguments `(#:system "i686-linux" ,@arguments))))
    (_ package64)))

(define (package-input-grafting replacements)
  "Return a procedure that, when passed a package, grafts its direct and
indirect dependencies, including implicit inputs, according to REPLACEMENTS.
REPLACEMENTS is a list of package pairs; the first element of each pair is the
package to replace, and the second one is the replacement.

Name and version of replacement packages will be padded to meet graft
requirement."
  (define (graft-string old new)
    (string-pad-right new (string-length old) #\0))

  (define (string-length=? a b)
    (= (string-length a)
       (string-length b)))

  (define (graft-package old new)
    (let ((old-name (package-name old))
          (new-name (package-name new))
          (old-version (package-version old))
          (new-version (package-version new)))
      (package
        (inherit old)
        (replacement
         (if (and (string-length=? old-name new-name)
                  (string-length=? old-version new-version))
             new
             (package
               (inherit new)
               (name (graft-string old-name new-name))
               (version (graft-string old-version new-version))
               (build-system trivial-build-system)
               (arguments (list #:builder #~(symlink #$new #$output)))
               (native-inputs '())
               (inputs '())
               (propagated-inputs '())
               (outputs '("out"))))))))

  (package-input-rewriting
   (map (match-lambda
          ((old . new)
           `(,old . ,(graft-package old new))))
        replacements)))

;; For concerns and direction of improvement, see this thread:
;; https://lists.gnu.org/archive/html/guix-devel/2024-06/msg00275.html
(define* (with-transformation proc obj #:optional (pred package?))
  "Recursing into child elements, apply PROC to every element of OBJ that
matches PRED."
  (match obj
    ((? pred)
     (proc obj))
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
    (_ obj)))

(define (package-with-alias alias pkg)
  "Return a copy of package PKG called ALIAS."
  (package
    (inherit pkg)
    (name alias)))

;; Using a package here to support ‘guix refresh’.
(define %binary-source
  (hidden-package
   (package
     (inherit hello)
     (name "binary-source")
     (version "0.0.0")
     (supported-systems '())
     (home-page "")
     (synopsis "Binary package source (internal use)")
     (description "")
     (license #f))))

(define* (binary-package-from-sources
          source-mapping p #:optional (default-system "x86_64-linux"))
  "Create package with system-dependent sources.  SOURCE-MAPPING should at least
contain source for DEFAULT-SYSTEM.  Taking the following package as example:

(binary-package-from-sources
 `((\"x86_64-linux\"  . ,source-aaa)
   (\"aarch64-linux\" . ,source-bbb))
 (package
   ...))

It unpacks source from source-bbb when building for aarch64-linux targets, and
source-aaa for all others."
  (package
    (inherit p)
    (version (package-version (assoc-ref source-mapping default-system)))
    (source #f)
    (arguments
     (substitute-keyword-arguments arguments
       ((#:phases phases #~%standard-phases)
        #~(modify-phases #$phases
            (replace 'unpack
              (lambda _
                ((assoc-ref #$phases 'unpack)
                 #:source
                 #+(package-source
                    (let ((system
                           (or (and=> (%current-target-system)
                                      platform-target->system)
                               (%current-system))))
                      (or (assoc-ref source-mapping system)
                          (assoc-ref source-mapping default-system)))))))))))
    (location (package-location p))))
