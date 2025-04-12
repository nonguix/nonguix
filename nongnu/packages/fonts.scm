;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2021 Korytov Pavel <thexcloud@gmail.com>
;;; Copyright © 2021 Jonathan Brielmaier <jonathan.brielmaier@web.de>
;;; Copyright © 2023 Krzysztof Baranowski <pharcosyle@gmail.com>
;;; Copyright © 2025 Kurome <hunt31999@gmail.org>

(define-module (nongnu packages fonts)
  #:use-module (ice-9 string-fun)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages fonts)
  #:use-module (guix deprecation)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
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

(define* (apple-font #:key
                     font-name
                     archive-timestamp
                     version
                     file
                     hash
                     synopsis
                     description)
  (package
    (name (string-append "font-apple-"
                         (string-replace-substring
                          (string-downcase font-name)
                          " " "-")))
    (version version)
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             ;; Download link is unversioned, use a stable snapshot.
             "https://web.archive.org/web/" archive-timestamp "/"
             "https://devimages-cdn.apple.com/design/resources/download/"
             file ".dmg"))
       (sha256
        (base32 hash))))
    (build-system font-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (replace 'unpack
            (lambda* (#:key source inputs #:allow-other-keys)
              (let ((7z-exe (search-input-file inputs "/bin/7z")))
                (invoke 7z-exe "x" source)
                (invoke 7z-exe "x" (car (find-files "." "\\.pkg$")))
                (invoke 7z-exe "x" "Payload~")))))))
    (native-inputs (list p7zip))
    (home-page "https://developer.apple.com/fonts")
    (synopsis (string-append font-name " typeface by Apple."))
    (description description)
    (license
     (nonfree
      "https://www.apple.com"
      "Specific license information is embedded in the font files."))))

(define-public font-apple-sf-pro
  (apple-font
   #:font-name "SF Pro"
   #:archive-timestamp "20240527221252"
   #:file "SF-Pro"
   #:version "20.0d8e1"
   #:hash "153k3q8h5mpd358b1lsa8ha32qxv7vc2q661k20j4nnf3ndjkv9j"
   #:description "This neutral, flexible, sans-serif typeface is the system
font for iOS, iPad OS, macOS and tvOS.  SF Pro features nine weights, variable
optical sizes for optimal legibility, four widths, and includes a rounded
variant.  SF Pro supports over 150 languages across Latin, Greek, and Cyrillic
scripts."))

(define-public font-apple-sf-compact
  (apple-font
   #:font-name "SF Compact"
   #:archive-timestamp "20240527221317"
   #:file "SF-Compact"
   #:version "20.0d8e1"
   #:hash "01j7k95rnmg667hyhj88fxqrgsah2mnk2ws7fmclif92mqcgwirj"
   #:description "Sharing many features with SF Pro, SF Compact features an
efficient, compact design that is optimized for small sizes and narrow columns.
SF Compact is the system font for watchOS and includes a rounded variant."))

(define-public font-apple-sf-mono
  (apple-font
   #:font-name "SF Mono"
   #:archive-timestamp "20240527221346"
   #:file "SF-Mono"
   #:version "20.0d8e1"
   #:hash "0cxj1wk6nbz1ysi0cg1w79b7ja48n00dypxkrsc3mvrn1vmdb4dm"
   #:description "This monospaced variant of San Francisco enables alignment
between rows and columns of text, and is used in coding environments like Xcode.
SF Mono features six weights and supports Latin, Greek, and Cyrillic scripts."))

(define-public font-apple-sf-arabic
  (apple-font
   #:font-name "SF Arabic"
   #:archive-timestamp "20240527221359"
   #:file "SF-Arabic"
   #:version "20.0d8e1"
   #:hash "0sm7w5i19n3jdj6d04344nxizqacg264rladjiwhxhwa3ksfbxsn"
   #:description "A contemporary interpretation of the Naskh style with a
rational and flexible design, this extension of San Francisco is the Arabic
system font on Apple platforms.  Like San Francisco, SF Arabic features nine
weights, variable optical sizes that automatically adjust spacing and contrast
based on the point size, and includes a rounded variant."))

(define-public font-apple-new-york
  (apple-font
   #:font-name "New York"
   #:archive-timestamp "20240527221356"
   #:file "NY"
   #:version "17.0d5e1"
   #:hash "06d8pnfp4z5nb82mh7g7pnz5m4njph3zy17ww1m8x0m34hn50zdn"
   #:description "A companion to San Francisco, this serif typeface is based on
essential aspects of historical type styles.  New York features six weights,
supports Latin, Greek and Cyrillic scripts, and features variable optical sizes
allowing it to perform as a traditional reading face at small sizes and a
graphic display face at larger sizes."))

;; At the time of this writing, `sf-symbols' is just `sf-pro' and `sf-compact'
;; together plus one extra file, SFSymbolsFallback.otf. This package is
;; probably of limited use on non-macOS but we'll include it for completeness
;; and in case the situation changes in the future.
(define-public font-apple-sf-symbols
  (apple-font
   #:font-name "SF Symbols"
   #:archive-timestamp "20240701100406"
   #:file "SF-Symbols-6"
   #:version "6"
   #:hash "1hyghp03zpsiks2p6kmghnmg28kkjj3qqk2hgwva855kly6r4hv8"
   #:description "With over 5,000 symbols, SF Symbols is a library of
iconography designed to integrate seamlessly with San Francisco, the system
font for Apple platforms.  Symbols come in nine weights and three scales, and
automatically align with text labels."))

(define-public font-apple-color-emoji
  (package
    (name "font-apple-color-emoji")
    (version "18.0d4e1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/pharcosyle/apple-fonts"
             "/releases/download"
             "/Apple-Color-Emoji-" version "/Apple.Color.Emoji.ttc"))
       (sha256
        (base32 "06i1ady7b2g1i2hl3f8yxn64g2i82c2ni9vdw25gpdqdj28vyqw3"))))
    (build-system font-build-system)
    (home-page "https://www.apple.com")
    (synopsis "Apple Color Emoji typeface by Apple.")
    (description "Color and black-and-white Apple emoji fonts.")
    (license (nonfree "https://www.apple.com"))))

(define-public font-apple-symbols
  (package
    (name "font-apple-symbols")
    (version "17.0d1e2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/pharcosyle/apple-fonts"
             "/releases/download"
             "/Apple-Symbols-" version "/Apple.Symbols.ttf"))
       (sha256
        (base32 "14ivhsiszmy1ql69kpr1f33n07qgpaj7w43w2p5qnmlwcrvs3ccr"))))
    (build-system font-build-system)
    (home-page "https://www.apple.com")
    (synopsis "Apple Symbols typeface by Apple.")
    (description "An Apple system font intended to provide coverage for
characters defined as symbols in the Unicode Standard.")
    (license (nonfree "https://www.apple.com"))))

;; XXX The build scripts are not freely licensed <https://bugs.gnu.org/32916>.
(define-public font-awesome-nonfree
  (package
    (inherit font-awesome)
    (name "font-awesome-nonfree")
    (version "6.7.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/FortAwesome/Font-Awesome")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1gmr7f9vqh2g5bamcrpzd9wzywchx4q3kajw4njcp3qqm7vld8ii"))))
    (arguments '())))

(define-public font-ubuntu
  (package
    (name "font-ubuntu")
    (version "0.83")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://assets.ubuntu.com/v1/fad7939b-ubuntu-font-family-"
             version ".zip"))
       (sha256
        (base32 "0hjvq2x758dx0sfwqhzflns0ns035qm7h6ygskbx1svzg517sva5"))))
    (build-system font-build-system)
    (home-page "https://design.ubuntu.com/font/")
    (synopsis "Ubuntu font family")
    (description
     "The Ubuntu font family is a sans-serif typeface family available in 22
styles plus a variable font with adjustable weight and width axes.  Its
fixed-width companion, Ubuntu Mono, comes in 8 styles and a variable font with
an adjustable weight axis.")
    ;; Ubuntu Font License is used.  This license is considered non-free by Debian
    ;; and Fedora, so probably not compatible with GNU FSDG.
    ;;
    ;; https://bugs.launchpad.net/ubuntu-font-licence/+bug/769874
    ;; https://bugzilla.redhat.com/show_bug.cgi?id=961642#c4
    (license (nonfree "https://ubuntu.com/legal/font-licence"))))
