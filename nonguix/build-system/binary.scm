;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2021 Josselin Poiret <dev@jpoiret.xyz>
;;;
;;; This file is not part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (nonguix build-system binary)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (guix derivations)
  #:use-module (guix search-paths)
  #:use-module (guix build-system)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system copy)
  #:use-module (guix packages)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (nonguix utils)
  #:export (%binary-build-system-modules
            default-patchelf
            default-glibc
            lower
            binary-build
            binary-build-system))

;; Commentary:
;;
;; Standard build procedure for binary packages.  This is implemented as an
;; extension of `copy-build-system'.
;;
;; Code:

(define %binary-build-system-modules
  ;; Build-side modules imported by default.
  `((nonguix build binary-build-system)
    (nonguix build utils)
    ,@%copy-build-system-modules))

(define (default-patchelf)
  "Return the default patchelf package."

  ;; Do not use `@' to avoid introducing circular dependencies.
  (let ((module (resolve-interface '(gnu packages elf))))
    (module-ref module 'patchelf)))

(define (default-glibc)
  "Return the default glibc package."
  ;; Do not use `@' to avoid introducing circular dependencies.
  (let ((module (resolve-interface '(gnu packages base))))
    (module-ref module 'glibc)))

(define* (lower name
                #:key source inputs native-inputs outputs system target
                (patchelf (default-patchelf))
                (glibc (default-glibc))
                #:allow-other-keys
                #:rest arguments)
  "Return a bag for NAME."
  (define private-keywords
    '(#:source #:target #:patchelf #:inputs #:native-inputs))

  (and (not target)                               ;XXX: no cross-compilation
       (bag
         (name name)
         (system system)
         (host-inputs `(,@(if source
                              `(("source" ,source))
                              '())
                        ,@inputs
                        ;; Keep the standard inputs of 'gnu-build-system'.
                        ,@(standard-packages)))
         (build-inputs `(("patchelf" ,patchelf)
                         ,@native-inputs
                         ;; If current system is i686, the *32 packages will be the
                         ;; same as the non-32, but that's OK.
                         ("libc32" ,(to32 glibc))))
         (outputs outputs)
         (build binary-build)
         (arguments (strip-keyword-arguments private-keywords arguments)))))

(define* (binary-build name inputs
                       #:key
		       guile source
                       (outputs '("out"))
                       (patchelf-plan ''())
                       (install-plan ''(("." "./")))
                       (search-paths '())
                       (out-of-source? #t)
                       (validate-runpath? #t)
                       (patch-shebangs? #t)
                       (strip-binaries? #t)
                       (strip-flags ''("--strip-debug"))
                       (strip-directories ''("lib" "lib64" "libexec"
                                             "bin" "sbin"))
                       (phases '(@ (nonguix build binary-build-system)
                                   %standard-phases))
                       (system (%current-system))
                       (imported-modules %binary-build-system-modules)
                       (modules '((nonguix build binary-build-system)
                                  (guix build utils)
                                  (nonguix build utils)))
                       (substitutable? #t)
                       allowed-references
                       disallowed-references)
  "Build SOURCE using PATCHELF, and with INPUTS. This assumes that SOURCE
provides its own binaries."
  (define builder
    (with-imported-modules imported-modules
      #~(begin
	  (use-modules #$@modules)

	  #$(with-build-variables inputs outputs
	      #~(binary-build #:source #+source
			      #:system #$system
			      #:outputs %outputs
			      #:inputs %build-inputs
			      #:patchelf-plan #$patchelf-plan
			      #:install-plan #$install-plan
			      #:search-paths '#$(map search-path-specification->sexp
						     search-paths)
			      #:phases #$phases
			      #:out-of-source? #$out-of-source?
			      #:validate-runpath? #$validate-runpath?
			      #:patch-shebangs? #$patch-shebangs?
			      #:strip-binaries? #$strip-binaries?
			      #:strip-flags #$strip-flags
			      #:strip-directories #$strip-directories)))))

  (mlet %store-monad ((guile (package->derivation (or guile (default-guile))
                                                  system #:graft? #f)))
    (gexp->derivation name builder
                      #:system system
                      #:target #f
                      #:substitutable? substitutable?
                      #:allowed-references allowed-references
                      #:disallowed-references disallowed-references
                      #:guile-for-build guile)))

(define binary-build-system
  (build-system
    (name 'binary)
    (description "The standard binary build system")
    (lower lower)))

;;; binary.scm ends here
