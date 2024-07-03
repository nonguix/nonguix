;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2022 B. Wilson <x@wilsonb.com>

(define-module (nongnu packages dyalog)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xml)
  #:use-module (guix build utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (guix utils)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 match)
  #:use-module (nongnu packages dotnet)
  #:use-module ((nonguix licenses) #:prefix license:))

(define-public dyalog
  (package
    (name "dyalog")
    (version "19.0.50027")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://www.dyalog.com/uploads/php/"
               "download.dyalog.com/download.php?file=19.0/linux_64_" version
               "_unicode.x86_64.deb"))
        (sha256
          (base32 "1h18lq2fgq2fgj4zbq1nssi421xhw4r7pxlq06mdklgbc79pbq6y"))))
    (build-system gnu-build-system)
    (outputs '("out" "fonts"))
    (inputs (list alsa-lib
                  at-spi2-atk
                  at-spi2-core
                  atk
                  cairo
                  coreutils
                  cups
                  dbus
                  dotnet
                  fontconfig
                  eudev
                  expat
                  (list gcc "lib")
                  gdk-pixbuf
                  glib
                  glibc
                  gtk+
                  icu4c-70 ; incompatible with 71
                  libdrm
                  libx11
                  libxcb
                  libxcomposite
                  libxcursor
                  libxdamage
                  libxext
                  libxfixes
                  libxi
                  libxkbcommon
                  libxrender
                  libxscrnsaver
                  libxtst
                  libxrandr
                  mesa
                  ncurses/tinfo
                  nspr
                  nss
                  pango
                  pciutils
                  sed
                  unixodbc))
    (native-inputs (list binutils bzip2 patchelf tar))
    (arguments
     `(#:modules (((guix build gremlin) #:select (file-runpath))
                  ((guix elf) #:select (elf-segments parse-elf PT_INTERP))
                  ((ice-9 binary-ports) #:select (get-bytevector-n))
                  ((srfi srfi-1) #:select (last))
                  ,@%default-gnu-imported-modules)
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'unpack
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((source (assoc-ref inputs "source")))
               (invoke "ar" "x" source)
               (invoke "tar" "-xzvf" "data.tar.gz"))))
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (lib (string-append out "/lib/dyalog"))
                    (in (string-append "opt/mdyalog/"
                                       ,(version-major+minor version)
                                       "/64/unicode/")))
               (substitute* (string-append in "mapl")
                 (("\"\\$\\{DYALOG\\}/dyalog\"" dyalog)
                  (string-append "exec -a dyalog " dyalog)))
               (substitute* (string-append in "scriptbin/dyalogscript")
                 (("^INSTALLDIR=.*") (format #f "INSTALLDIR=\"~a\"~%" lib))))))
         (delete 'build)
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (fonts (assoc-ref outputs "fonts"))
                    (lib (string-append out "/lib/dyalog"))
                    (bin (string-append out "/bin"))
                    (truetype (string-append fonts "/share/fonts/truetype"))
                    (dotnet (assoc-ref inputs "dotnet"))
                    (dotnet-root (string-append dotnet "/share/dotnet"))
                    (in (string-append "opt/mdyalog/"
                                       ,(version-major+minor version)
                                       "/64/unicode/"))
                    (subdir (lambda (dir)
                              (lambda (pkg) (string-append pkg dir)))))
               (mkdir-p lib)
               (copy-recursively in lib)
               (delete-file-recursively (string-append lib "/fonts"))

               (mkdir-p truetype)
               (install-file (string-append in "/fonts/Apl385.ttf") truetype)
               (install-file (string-append in "/fonts/APL333.ttf") truetype)

               (mkdir-p bin)
               (symlink (string-append lib "/mapl")
                        (string-append bin "/dyalog"))
               (symlink (string-append lib "/scriptbin/dyalogscript")
                        (string-append bin "/dyalogscript"))
               (wrap-program (string-append lib "/dyalog")
                 `("DOTNET_ROOT" = (,dotnet-root))
                 `("LD_LIBRARY_PATH" ":" suffix
                   (,@(map (subdir "/lib")
                           (list (assoc-ref inputs "eudev")
                                 (assoc-ref inputs "gdk-pixbuf")
                                 (assoc-ref inputs "gtk+")
                                 (assoc-ref inputs "icu4c")
                                 (assoc-ref inputs "ncurses-with-tinfo")
                                 (assoc-ref inputs "libx11")
                                 (assoc-ref inputs "mesa")
                                 (assoc-ref inputs "pciutils"))))))
               (wrap-program (string-append lib "/mapl")
                 `("PATH" = (,@(map (subdir "/bin")
                                    (list (assoc-ref inputs "coreutils")
                                          (assoc-ref inputs "sed"))))))
               #t)))
         (add-after 'install 'patch-elf-files
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (lib (string-append out "/lib/dyalog"))
                    (glibc (assoc-ref inputs "glibc"))
                    (ld.so (string-append glibc ,(glibc-dynamic-linker)))
                    (rpath (string-join
                             (cons* lib
                                    (string-append lib "/lib")
                                    (string-append (assoc-ref inputs "nss")
                                                   "/lib/nss")
                                    (map (lambda (pkg)
                                           (string-append (assoc-ref inputs pkg)
                                                          "/lib"))
                                         '("alsa-lib"
                                           "at-spi2-atk"
                                           "at-spi2-core"
                                           "atk"
                                           "cairo"
                                           "cups"
                                           "dbus"
                                           "expat"
                                           "fontconfig-minimal"
                                           "gcc"
                                           "glib"
                                           "glibc"
                                           "gtk+"
                                           "libdrm"
                                           "libx11"
                                           "libxcb"
                                           "libxcomposite"
                                           "libxcursor"
                                           "libxdamage"
                                           "libxext"
                                           "libxfixes"
                                           "libxi"
                                           "libxkbcommon"
                                           "libxrender"
                                           "libxscrnsaver"
                                           "libxtst"
                                           "libxrandr"
                                           "mesa"
                                           "ncurses-with-tinfo"
                                           "nspr"
                                           "pango"
                                           "unixodbc")))
                             ":"))
                    (elf-file?* (lambda (file stat) (elf-file? file))))

               (define* (file-segments file #:key type)
                 (let* ((bv (call-with-input-file file
                              (lambda (port)
                                (get-bytevector-n port 4096))
                              #:binary #t #:guess-encoding #f))
                        (segments (elf-segments (parse-elf bv)))
                        (select? (lambda (elf)
                                   (eq? (elf-segment-type elf) type))))
                   (if type
                     (filter select? segments)
                     segments)))

               (define* (set-runpath file #:optional (runpath rpath))
                 (if (file-runpath file)
                   (begin
                     (format #f "Setting RUNPATH: ~a~&" file)
                     (invoke "patchelf" "--set-rpath" runpath file)
                     (invoke "patchelf" "--shrink-rpath" file))))

               (define* (set-interpreter file #:optional (interp ld.so))
                 (if (not (null? (file-segments file #:type PT_INTERP)))
                   (begin
                     (format #f "Setting interpreter: ~a~%" file)
                     (invoke "patchelf" "--set-interpreter" interp file))))

               (define (patch-elf file)
                 (begin
                   (set-runpath file)
                   (set-interpreter file)))

               (for-each patch-elf (find-files out elf-file?*))
               #t))))))
     (home-page "https://www.dyalog.com/")
     (synopsis "Dyalog APL interpreter and programming language environment")
     (description "Dyalog APL is de facto the most widely deployed dialect of
APL in the wild.  The interpreter boasts world-class performance benchmarks,
excellent tooling integration, and support for modern APL features.")
     (license (license:nonfree
                "https://www.dyalog.com/prices-and-licences.htm"))))

(define-public dyalog-apl
  (deprecated-package "dyalog-apl" dyalog))
