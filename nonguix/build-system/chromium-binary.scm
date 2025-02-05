;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2023 Giacomo Leidi <goodoldpaul@autistici.org>

(define-module (nonguix build-system chromium-binary)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages kerberos)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xml)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (guix derivations)
  #:use-module (guix search-paths)
  #:use-module (guix build-system)
  #:use-module (guix build-system gnu)
  #:use-module (guix packages)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (nonguix build-system binary)
  #:use-module (nonguix utils)
  #:export (%chromium-binary-build-system-modules
            lower
            chromium-binary-build
            chromium-binary-build-system))

;; Commentary:
;;
;; Standard build procedure for Chromium based binary packages.  This is
;; implemented as an extension of `binary-build-system'.
;;
;; Code:

(define %chromium-binary-build-system-modules
  ;; Build-side modules imported by default.
  `((nonguix build chromium-binary-build-system)
    (nonguix build utils)
    ,@%binary-build-system-modules))

(define (build-patchelf-plan wrapper-plan inputs)
  #~(let ((patchelf-inputs
           (list #$@(map car inputs))))
      (map (lambda (file)
             ;; Either an entry in WRAPPER-PLAN is just a string which can be
             ;; used directly, or it is a list where the second element is a
             ;; list of additional inputs for patchelf-plan.
             (if (list? file)
                 (cons (car file) (list (append patchelf-inputs (cadr file))))
                 (cons file (list patchelf-inputs))))
           #$wrapper-plan)))

(define* (lower name
                #:key source inputs native-inputs outputs system target
                (patchelf (default-patchelf))
                (glibc (default-glibc))
                #:allow-other-keys
                #:rest arguments)
  "Return a bag for NAME."
  (define private-keywords
    '(#:target #:patchelf #:inputs #:native-inputs))
  (define host-inputs
    `(,@(if source
            `(("source" ,source))
            '())

      ("alsa-lib" ,alsa-lib)
      ("atk" ,atk)
      ("at-spi2-atk" ,at-spi2-atk)
      ("at-spi2-core" ,at-spi2-core)
      ("bash-minimal" ,bash-minimal)
      ("cairo" ,cairo)
      ("cups" ,cups)
      ("dbus" ,dbus)
      ("eudev" ,eudev)
      ("expat" ,expat)
      ("fontconfig" ,fontconfig)
      ("freetype" ,freetype)
      ("gcc:lib" ,gcc "lib")
      ("glib" ,glib)
      ("gtk+" ,gtk+)
      ("libdrm" ,libdrm)
      ("libnotify" ,libnotify)
      ("librsvg" ,librsvg)
      ("libsecret" ,libsecret)
      ("libx11" ,libx11)
      ("libxcb" ,libxcb)
      ("libxcomposite" ,libxcomposite)
      ("libxcursor" ,libxcursor)
      ("libxdamage" ,libxdamage)
      ("libxext" ,libxext)
      ("libxfixes" ,libxfixes)
      ("libxi" ,libxi)
      ("libxkbcommon" ,libxkbcommon)
      ("libxkbfile" ,libxkbfile)
      ("libxrandr" ,libxrandr)
      ("libxrender" ,libxrender)
      ("libxshmfence" ,libxshmfence)
      ("libxtst" ,libxtst)
      ("mesa" ,mesa)
      ("mit-krb5" ,mit-krb5)
      ("nspr" ,nspr)
      ("nss" ,nss)
      ("pango" ,pango)
      ("pulseaudio" ,pulseaudio)
      ("sqlcipher" ,sqlcipher)
      ("xcb-util" ,xcb-util)
      ("xcb-util-image" ,xcb-util-image)
      ("xcb-util-keysyms" ,xcb-util-keysyms)
      ("xcb-util-renderutil" ,xcb-util-renderutil)
      ("xcb-util-wm" ,xcb-util-wm)
      ("zlib" ,zlib)

      ,@inputs
      ;; Keep the standard inputs of 'gnu-build-system'.
      ,@(standard-packages)))

  (and (not target)                     ;XXX: no cross-compilation
       (bag
         (name name)
         (system system)
         (host-inputs host-inputs)
         (build-inputs `(("patchelf" ,patchelf)
                         ,@native-inputs
                         ;; If current system is i686, the *32 packages will be the
                         ;; same as the non-32, but that's OK.
                         ("libc32" ,(to32 glibc))))
         (outputs outputs)
         (build chromium-binary-build)
         (arguments (append
                     (strip-keyword-arguments private-keywords arguments)
                     (list #:wrap-inputs (alist-delete "source" host-inputs)))))))

(define* (chromium-binary-build name inputs
                       #:key
		       guile source wrap-inputs
                       (outputs '("out"))
                       (wrapper-plan ''())
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
                       (phases '(@ (nonguix build chromium-binary-build-system)
                                   %standard-phases))
                       (system (%current-system))
                       (imported-modules %chromium-binary-build-system-modules)
                       (modules '((nonguix build chromium-binary-build-system)
                                  (guix build utils)
                                  (nonguix build utils)))
                       (substitutable? #t)
                       allowed-references
                       disallowed-references)
  "Build SOURCE using binary-build-system.  WRAPPER-PLAN is a list of strings for
files which patchelf will add the INPUTS (which implicitly includes the base
packages needed for chromium-based binaries) to RPATH and wrap with needed
environment variables.  Optionally, an entry can be a list with the first
entry the file to be patched and the second a list of additional inputs for
patchelf, like PATCHELF-PLAN in binary-build-system.  PATCHELF-PLAN itself is
ignored if WRAPPER-PLAN is not '()."
  (define builder
    (with-imported-modules imported-modules
      #~(begin
	  (use-modules #$@modules)

	  #$(with-build-variables inputs outputs
	      #~(chromium-binary-build #:source #+source
			      #:system #$system
			      #:outputs %outputs
			      #:inputs %build-inputs
			      #:patchelf-plan
                              #$(if (equal? wrapper-plan ''())
                                    patchelf-plan
                                    (build-patchelf-plan wrapper-plan
                                                         wrap-inputs))
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

(define chromium-binary-build-system
  (build-system
    (name 'chromium-binary)
    (description "The Chromium based binary build system")
    (lower lower)))

;;; chromium-binary.scm ends here
