;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2019 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2025 Maxim Cournoyer <maxim@guixotic.coop>

(define-module (nongnu packages emulators)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages sdl)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (nonguix licenses))

(define-public dgen
  (package
    (name "dgen")
    (version "1.33")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://sourceforge/dgen/dgen/"
             version "/dgen-sdl-" version ".tar.gz"))
       (sha256
        (base32
         "07pnvsw04jq9pdiwfhp0nr728rwqq2y6w648gz3p6a622xhc1qlr"))))
    (build-system gnu-build-system)
    (inputs
     `(("sdl" ,sdl)
       ("libarchive" ,libarchive)
       ("libgl" ,mesa)))
    (home-page "http://dgen.sourceforge.net")
    (synopsis "Emulator for Sega Genesis/Mega Drive systems")
    (description
     "DGen isn't the best Mega Drive/Genesis emulator out there, but it works
and it's probably the most portable.  It's one of the few that's x86_64
compatible.  It's also perfect for command line freaks.")
    (license (list license:bsd-3
                   ;; Many non-free licenses.
                   (nonfree "https://sourceforge.net/p/dgen/dgen/ci/master/tree/COPYING")))))

(define-public libretro-genesis-plus-gx
  ;; There is no release nor tags: use the latest commit.
  (let ((commit "a80e3b3b957d38961e274aa4da450245ddc63fe8")
        (revision "0"))
    (package
      (name "libretro-genesis-plus-gx")
      (version (git-version "0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/ekeeke/Genesis-Plus-GX")
                       (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0iqsbxw00mqc2hf0jw1v6qzccf5b40d6q1a7vv5bdi3i260py6i0"))))
      (build-system gnu-build-system)
      (arguments
       (list
        #:tests? #f                     ;no test suite
        #:make-flags
        #~(list "-f" "Makefile.libretro"
                (string-append "GIT_VERSION=" #$version)
                (string-append "CC=" #$(cc-for-target)))
        #:phases
        #~(modify-phases %standard-phases
            (delete 'configure)         ;no configure script
            (replace 'install
              (lambda _
                (install-file "genesis_plus_gx_libretro.so"
                              (string-append #$output "/lib/libretro"))
                (invoke "find" "-executable" "-name" "*.so"))))))
      (home-page "https://github.com/ekeeke/Genesis-Plus-GX")
      (synopsis "Accurate Sega 8/16 bit emulator")
      (description "This is the Libretro port of Genesis Plus GX, a SG-1000,
Mark-III, Master System (I & II), Game Gear, Genesis/Mega Drive and Sega/Mega
CD emulator.  It was originally based on Genesis Plus, from which it improves
emulation accuracy and adds support for new peripherals, cartridges and
features such as:
@itemize
@item
NTSC (60Hz) and PAL (50Hz) video hardware emulation
@item
accurate 68000, Z80 CPU emulation and synchronization
@item
accurate VDP emulation and timings
@item
sample-accurate YM2612,YM2413, SN76489 and RF5C164 PCM sound chips emulation
@item
cycle-accurate sound chips synchronization with 68000/Z80 CPU
@item
cycle-accurate 68000 and Z80 CPU synchronization
@item
optimized Main-CPU / Sub-CPU synchronization (Sega/Mega CD)
@item
accurate CDD, CDC and GFX chip emulation (Sega/Mega CD)
@item
accurate CD-DA fader emulation (Sega/Mega CD)
@item
Mode 1 cartridge support (Sega/Mega CD)
@item
Audio CD and CD+G support (Sega/Mega CD)
@item
high-quality audio resampling using Blip Buffer
@item
basic hardware latency emulation (VDP/68k, Z80/68k)
@item
full overscan area emulation (horizontal and vertical color borders)
@end itemize")
      ;; Redistributable but has a non-commercial clause, so does not meet the
      ;; GNU FSDG requirements.
      (license (nonfree "file://LICENSE.txt")))))
