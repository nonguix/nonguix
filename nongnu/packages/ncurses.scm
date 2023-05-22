;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2012, 2013, 2014, 2015, 2017, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014, 2016 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015, 2017 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016 Nikita <nikita@n0.is>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Jan Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2017, 2019, 2020, 2021 Marius Bakke <marius@gnu.org>
;;; Copyright © 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020 Michael Rohleder <mike@rohleder.de>
;;; Copyright © 2022-2023 B. Wilson <x@wilsonb.com>

(define-module (nongnu packages ncurses)
  #:use-module (gnu packages)
  #:use-module (guix gexp)
  #:use-module (guix licenses)
  #:use-module (guix memoization)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system perl)
  #:use-module (gnu packages)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages linux)
  #:use-module (guix utils)
  #:use-module (ice-9 match))

(define ncurses-rollup-patch
  (mlambda (version hash)
    (origin
      (method url-fetch)
      (uri (match (string-split (version-major+minor+point version) #\.)
             ((major minor point)
              (string-append "https://invisible-mirror.net/archives"
                             "/ncurses/" major "." minor "/ncurses-"
                             major "." minor "-" point "-patch.sh.bz2"))))
      (sha256 (base32 hash)))))

(define-public ncurses-5
  (package
    (name "ncurses")
    (version "5.9.20141206")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/ncurses/ncurses-"
                                  (version-major+minor version)
                                  ".tar.gz"))
              (sha256
               (base32
                "0fsn7xis81za62afan0vvm38bvgzg5wfmv1m86flqcj0nj7jjilh"))))
    (build-system gnu-build-system)
    (outputs '("out"
               "doc"))                ;1 MiB of man pages
    (arguments
     (let ((patch-makefile-phase
            #~(lambda _
                (for-each patch-makefile-SHELL
                          (find-files "." "Makefile.in"))))
           (configure-phase
            ;; The 'configure' script does not understand '--docdir', so we must
            ;; override that and use '--mandir' instead.
            #~(lambda* (#:key build target outputs configure-flags
                        #:allow-other-keys)
                (let ((out (assoc-ref outputs "out"))
                      (doc (assoc-ref outputs "doc")))
                  (apply invoke "./configure"
                         (string-append "SHELL=" (which "sh"))
                         (string-append "--build=" build)
                         (string-append "--prefix=" out)
                         (string-append "--mandir=" doc "/share/man")
                         (if target
                             (cons (string-append "--host=" target)
                                   configure-flags)
                             configure-flags)))))
           (apply-rollup-patch-phase
            ;; Ncurses distributes "stable" patchsets to be applied on top
            ;; of the release tarball.  These are only available as shell
            ;; scripts(!) so we decompress and apply them in a phase.
            ;; See <https://invisible-mirror.net/archives/ncurses/6.1/README>.
            #~(lambda* (#:key inputs native-inputs #:allow-other-keys)
                (let ((rollup-patch #$(ncurses-rollup-patch
                                       (package-version this-package)
                                       "16ny892yhimy6r4mmsgw3rcl0i15570ifn9c54g1ndyrk7kpmlgs")))
                  (copy-file rollup-patch
                             (string-append (getcwd) "/rollup-patch.sh.bz2"))
                  (invoke "bzip2" "-d" "rollup-patch.sh.bz2")
                  (invoke "sh" "rollup-patch.sh"))))
           (remove-shebang-phase
            #~(lambda _
                ;; To avoid retaining a reference to the bootstrap Bash via the
                ;; shebang of the 'ncursesw6-config' script, simply remove that
                ;; shebang: it'll work just as well without it.  Likewise, do not
                ;; retain a reference to the "doc" output.
                (substitute* "misc/ncurses-config.in"
                  (("#!@SHELL@")
                   "# No shebang here, use /bin/sh!\n")
                  (("@SHELL@ \\$0")
                   "$0")
                  (("mandir=.*$")
                   "mandir=share/man"))))
           (post-install-phase
            #~(lambda* (#:key outputs #:allow-other-keys)
                (let ((out (assoc-ref outputs "out")))
                  ;; When building a wide-character (Unicode) build, create backward
                  ;; compatibility links from the the "normal" libraries to the
                  ;; wide-character ones (e.g. libncurses.so to libncursesw.so).
                  #$@(if (target-mingw?)
                         `( ;; TODO: create .la files to link to the .dll?
                           (with-directory-excursion (string-append out "/bin")
                             (for-each
                              (lambda (lib)
                                (define lib.dll
                                  (string-append "lib" lib ".dll"))
                                (define libwx.dll
                                  (string-append "lib" lib "w"
                                                 ,(version-major version) ".dll"))

                                (when (file-exists? libwx.dll)
                                  (format #t "creating symlinks for `lib~a'~%" lib)
                                  (symlink libw6.dll lib.dll)))
                              '("curses" "ncurses" "form" "panel" "menu"))))
                         #~())
                  (with-directory-excursion (string-append out "/lib")
                    (for-each (lambda (lib)
                                (define libw.a
                                  (string-append "lib" lib "w.a"))
                                (define lib.a
                                  (string-append "lib" lib ".a"))

                                #$@(if (not (target-mingw?))
                                       #~((define libw.so.x
                                            (string-append "lib" lib "w.so."
                                                           #$(version-major version)))
                                          (define lib.so.x
                                            (string-append "lib" lib ".so."
                                                           #$(version-major version)))
                                          (define lib.so
                                            (string-append "lib" lib ".so"))
                                          (define packagew.pc
                                            (string-append lib "w.pc"))
                                          (define package.pc
                                            (string-append lib ".pc")))
                                       #~())

                                (when (file-exists? libw.a)
                                  (format #t "creating symlinks for `lib~a'~%" lib)
                                  (symlink libw.a lib.a)
                                  #$@(if (not (target-mingw?))
                                         '((symlink libw.so.x lib.so.x)
                                           (false-if-exception (delete-file lib.so))
                                           (call-with-output-file lib.so
                                             (lambda (p)
                                               (format p "INPUT (-l~aw)~%" lib)))
                                           (with-directory-excursion "pkgconfig"
                                             (format #t "creating symlink for `~a'~%"
                                                     package.pc)
                                             (when (file-exists? packagew.pc)
                                               (symlink packagew.pc package.pc))))
                                         #~())))
                              '("curses" "ncurses" "form" "panel" "menu")))))))
       (list #:configure-flags
             #~`("--with-shared" "--without-debug" "--enable-widec"

                 "--enable-pc-files" "--with-versioned-syms=yes"
                 ,(string-append "--with-pkg-config-libdir="
                                 #$output "/lib/pkgconfig")

                 ;; By default headers land in an `ncursesw' subdir, which is not
                 ;; what users expect.
                 ,(string-append "--includedir=" #$output "/include")
                 "--enable-overwrite"                ;really honor --includedir

                 ;; Make sure programs like 'tic', 'reset', and 'clear' have a
                 ;; correct RUNPATH.
                 ,(string-append "LDFLAGS=-Wl,-rpath=" #$output "/lib")

                 ;; Starting from ncurses 6.1, "make install" runs "install -s"
                 ;; by default, which doesn't work for cross-compiled binaries
                 ;; because it invokes 'strip' instead of 'TRIPLET-strip'.  Work
                 ;; around this.
                 #$@(if (%current-target-system) #~("--disable-stripping") #~())

                 ;; Do not assume a default search path in ld, even if it is only to
                 ;; filter it out in ncurses-config.  Mainly because otherwise it ends
                 ;; up using the libdir from binutils, which makes little sense and
                 ;; causes an unnecessary runtime dependency.
                 "cf_cv_ld_searchpath=/no-ld-searchpath"

                 ;; MinGW: Use term-driver created for the MinGW port.
                 #$@(if (target-mingw?) #~("--enable-term-driver") #~())
                 "CXXFLAGS=-std=c++11")
             #:tests? #f                          ; no "check" target
             #:phases #~(modify-phases %standard-phases
                          (add-after 'unpack 'apply-rollup-patch
                            #$apply-rollup-patch-phase)
                          (replace 'configure #$configure-phase)
                          (add-after 'install 'post-install
                            #$post-install-phase)
                          (add-before 'configure 'patch-makefile-SHELL
                            #$patch-makefile-phase)
                          (add-before 'patch-source-shebangs 'remove-unneeded-shebang
                            #$remove-shebang-phase)))))
    (native-inputs
     (if (%current-target-system)
         (list pkg-config this-package)           ;for 'tic'
         (list pkg-config)))
    (native-search-paths
     (list (search-path-specification
            (variable "TERMINFO_DIRS")
            (files '("share/terminfo")))))
    (synopsis "Terminal emulation (termcap, terminfo) library")
    (description
     "GNU Ncurses is a library which provides capabilities to write text to
a terminal in a terminal-independent manner.  It supports pads and color as
well as multiple highlights and forms characters.  It is typically used to
implement user interfaces for command-line applications.  The accompanying
ncursesw library provides wide character support.")
    (license x11)
    (home-page "https://www.gnu.org/software/ncurses/")))

(define-public ncurses/tinfo-5
  (package/inherit ncurses-5
    (name "ncurses-with-tinfo")
    (arguments
     (substitute-keyword-arguments (package-arguments ncurses-5)
       ((#:configure-flags cf)
        #~(cons "--with-termlib=tinfo" #$cf))))))
