;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2021 Korytov Pavel <thexcloud@gmail.com>
;;; Copyright © 2021 Jonathan Brielmaier <jonathan.brielmaier@web.de>

(define-module (nongnu packages fonts)
  #:use-module (ice-9 string-fun)
  #:use-module (gnu packages compression)
  #:use-module (guix deprecation)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system font)
  #:use-module (guix build-system copy)
  #:use-module (nonguix licenses))

(define mscorefont
  (lambda* (file #:key version font-name hash)
    (package
      ;; Downcase and replace " " with - to get "font-microsoft-times-new-roman"
      ;; from "Times New Roman"
      (name (string-append
             "font-microsoft-"
             (string-replace-substring (string-downcase font-name) " " "-")))
      (version version)
      (source
       (origin
         (method url-fetch)
         (uri (string-append "https://deac-ams.dl.sourceforge.net/project/corefonts/the%20fonts/final/" file ".exe"))
         (sha256 (base32 hash))))
      (build-system font-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           ;; Unpack EXE and replace .TTF with .ttf
           (replace 'unpack
             (lambda* (#:key inputs #:allow-other-keys)
               (use-modules (ice-9 string-fun))
               (let ((source (assoc-ref inputs "source")))
                 (system (format #f "7z e ~a -ofont" source))
                 (map (lambda (oldname)
                        (rename-file oldname
                          (string-replace-substring oldname "TTF" "ttf")))
                      (find-files "./font" "\\.TTF$"))))))))
      (native-inputs
       `(("p7zip" ,p7zip)))
      (home-page "http://corefonts.sourceforge.net/")
      (synopsis (string-append font-name " font"))
      (description (string-append "This package provides " font-name "
font from the pack of Microsoft core fonts for the web."))
      (license (nonfree "http://corefonts.sourceforge.net/eula.htm")))))

(define-public font-microsoft-andale-mono
  (mscorefont
   "andale32"
   #:version "2.00"
   #:font-name "Andale Mono"
   #:hash "0w7927hlwayqf3vvanf8f3qp2g1i404jzqvhp1z3mp0sjm1gw905"))

(define-public font-microsoft-arial
  (mscorefont
   "arial32"
   #:version "2.82"
   #:font-name "Arial"
   #:hash "1xkqyivbyb3z9dcalzidf8m4npzfpls2g0kldyn8g73f2i6plac5"))

(define-public font-microsoft-arial-black
  (mscorefont
   "arialb32"
   #:version "2.35"
   #:font-name "Arial Black"
   #:hash "1a60zqrg63kjnykh5hz7dbpzvx7lyivn3vbrp7jyv9d1nvzz09d4"))

(define-public font-microsoft-comic-sans-ms
  (mscorefont
   "comic32"
   #:version "2.10"
   #:font-name "Comic Sans MS"
   #:hash "0ki0rljjc1pxkbsxg515fwx15yc95bdyaksa3pjd89nyxzzg6vcw"))

(define-public font-microsoft-courier-new
  (mscorefont
   "courie32"
   #:version "2.82"
   #:font-name "Courier New"
   #:hash "111k3waxki9yyxpjwl2qrdkswvsd2dmvhbjmmrwyipam2s31sldv"))

(define-public font-microsoft-couirer-new
  (deprecated-package "font-microsoft-couirer-new" font-microsoft-courier-new))

(define-public font-microsoft-georgia
  (mscorefont
   "georgi32"
   #:version "2.05"
   #:font-name "Georgia"
   #:hash "0083jcpd837j2c06kp1q8glfjn9k7z6vg3wi137savk0lv6psb1c"))

(define-public font-microsoft-impact
  (mscorefont
   "impact32"
   #:version "2.35"
   #:font-name "Impact"
   #:hash "1yyc5z7zmm3s418hmrkmc8znc55afsrz5dgxblpn9n81fhxyyqb0"))

(define-public font-microsoft-trebuchet-ms
  (mscorefont
   "trebuc32"
   #:version "1.22"
   #:font-name "Trebuchet MS"
   #:hash "1jfsgz80pvyqvpfpaiz5pd8zwlcn67rg2jgynjwf22sip2dhssas"))

(define-public font-microsoft-times-new-roman
  (mscorefont
   "times32"
   #:version "2.82"
   #:font-name "Times New Roman"
   #:hash "1aq7z3l46vwgqljvq9zfgkii6aivy00z1529qbjkspggqrg5jmnv"))

(define-public font-microsoft-verdana
  (mscorefont
   "verdan32"
   #:version "2.35"
   #:font-name "Verdana"
   #:hash "15mdbbfqbyp25a6ynik3rck3m3mg44plwrj79rwncc9nbqjn3jy1"))

(define-public font-microsoft-webdings
  (mscorefont
   "webdin32"
   #:version "1.03"
   #:font-name "Webdings"
   #:hash "0nnp2znmnmx87ijq9zma0vl0hd46npx38p0cc6lgp00hpid5nnb4"))

(define-public font-microsoft-web-core-fonts
  (package
    (inherit font-microsoft-times-new-roman)
    (name "font-microsoft-web-core-fonts")
    (version "1.0.0")
    (synopsis "Collection of widely spread Microsoft TrueType fonts")
    (description "This package provides fonts from the collection of Microsoft
True Type Core Fonts for the Web.

Included fonts:
@itemize
@item Andale Mono
@item Arial Black
@item Arial (Bold, Italic, Bold Italic)
@item Comic Sans MS (Bold)
@item Courier New (Bold, Italic, Bold Italic)
@item Georgia (Bold, Italic, Bold Italic)
@item Impact
@item Times New Roman (Bold, Italic, Bold Italic)
@item Trebuchet (Bold, Italic, Bold Italic)
@item Verdana (Bold, Italic, Bold Italic)
@item Webdings
@end itemize")
    (propagated-inputs
     `(("font-microsoft-andale-mono" ,font-microsoft-andale-mono)
       ("font-microsoft-arial" ,font-microsoft-arial)
       ("font-microsoft-arial-black" ,font-microsoft-arial-black)
       ("font-microsoft-comic-sans-ms" ,font-microsoft-comic-sans-ms)
       ("font-microsoft-couirer-new" ,font-microsoft-couirer-new)
       ("font-microsoft-georgia" ,font-microsoft-georgia)
       ("font-microsoft-impact" ,font-microsoft-impact)
       ("font-microsoft-times-new-roman" ,font-microsoft-times-new-roman)
       ("font-microsoft-trebuchet-ms" ,font-microsoft-trebuchet-ms)
       ("font-microsoft-verdana" ,font-microsoft-verdana)
       ("font-microsoft-webdings" ,font-microsoft-webdings)))))
