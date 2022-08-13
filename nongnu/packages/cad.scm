;;; Copyright © 2021-2022 Petr Hodina <phodina@protonmail.com>
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(define-module (nongnu packages cad)
  #:use-module (guix packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages xorg)
  #:use-module (guix download)
  #:use-module (nonguix build-system binary)
  #:use-module ((nonguix licenses) #:prefix license:))

(define-public lightburn
  (package
    (name "lightburn")
    (version "1.2.01")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/LightBurnSoftware/deployment/"
                    "releases/download/" version
                    "/LightBurn-Linux64-v" version ".7z"))
              (sha256
               (base32
                "1yqxkf0izcfz05wrxh9xpmm7qi5wd5f1w9d2kni2wbzs531nr22p"))))
    (build-system binary-build-system)
    (arguments
     `(#:strip-binaries? #f ;TODO: For some reason it fails validate-runpath
       ;; phase if enbaled
       #:install-plan
       `(("LightBurn" "bin/LightBurn") ("LightBurn.png" "Lightburn.png")
         ("qt.conf" "qt.conf")
         ("languages" "languages")
         ("plugins" "plugins")
         ("translations" "translations"))
       #:phases
       (modify-phases %standard-phases
         (replace 'unpack
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((source (assoc-ref inputs "source")))
               (invoke "7z" "x" source)
               (chdir "LightBurn")
               (delete-file-recursively "lib"))))
         (replace 'patchelf
           (lambda* (#:key inputs outputs patchelf-plan #:allow-other-keys)
             (let* ((lightburn "LightBurn")
			        (plugins (list
                                 "audio/libqtaudio_alsa.so"
                                 "audio/libqtmedia_pulse.so"
                                 "bearer/libqconnmanbearer.so"
                                 "bearer/libqgenericbearer.so"
                                 "bearer/libqnmbearer.so"
                                 "imageformats/libqgif.so"
                                 "imageformats/libqicns.so"
                                 "imageformats/libqico.so"
                                 "imageformats/libqjpeg.so"
                                 "imageformats/libqtga.so"
                                 "imageformats/libqtiff.so"
                                 "imageformats/libqwbmp.so"
                                 "imageformats/libqwebp.so"
                                 "mediaservice/libgstaudiodecoder.so"
                                 "mediaservice/libgstcamerabin.so"
                                 "mediaservice/libgstmediacapture.so"
                                 "mediaservice/libgstmediaplayer.so"
                                 (string-append
                                 "platforminputcontexts"
                                 "/libcomposeplatforminputcontextplugin.so")
                                 (string-append
                                 "platforminputcontexts"
                                 "/libibusplatforminputcontextplugin.so")
                                 "platforms/libqxcb.so"
                                 "printsupport/libcupsprintersupport.so"
                                 "xcbglintegrations/libqxcb-egl-integration.so"
                                                  "xcbglintegrations/libqxcb-glx-integration.so"))
                    (libc (assoc-ref inputs "libc"))
                    (nss (assoc-ref inputs "nss"))
                    (rpath (string-append (apply string-append
                                                 (map (lambda (pkg)
                                                        (string-append (assoc-ref
                                                                        inputs
                                                                        pkg)
                                                         "/lib:"))
                                                      '("alsa-lib"
                                                        "cups-minimal"
                                                        "fontconfig-minimal"
                                                        "freetype"
                                                        "gcc"
                                                        "glib"
                                                        "gst-plugins-base"
                                                        "gstreamer"
                                                        "libusb"
                                                        "libx11"
                                                        "libxcb"
                                                        "libxext"
                                                        "libxi"
                                                        "libxrender"
                                                        "mesa"
                                                        "nspr"
                                                        "openlibm"
                                                        "pulseaudio"
                                                        "qtbase"
                                                        "qtserialport"
                                                        "qtmultimedia"
                                                        "zlib"))) nss
                                          "/lib/nss:"))
                    (ld-so (string-append libc
                                          ,(glibc-dynamic-linker))))
               (invoke "patchelf" "--set-rpath" rpath lightburn)
               (invoke "patchelf" "--set-interpreter" ld-so lightburn)
               (map (lambda (x)
                      (invoke "patchelf" "--set-rpath" rpath
                              (string-append "plugins/" x))) plugins)))))))
    (supported-systems '("x86_64-linux"))
    (native-inputs (list p7zip
                         patchelf))
    (inputs (list alsa-lib
                  cups-minimal
                  fontconfig
                  `(,gcc "lib")
                  glib
                  gst-plugins-base
                  gstreamer
                  libusb
                  libx11
                  libxcb
                  libxext
                  libxi
                  libxrender
                  nspr
                  nss
                  mesa
                  openlibm
                  pulseaudio
                  qtbase-5
                  qtserialport
                  qtmultimedia-5
                  zlib))
    (synopsis "Layout, editing, and control software for your laser cutter")
    (description
     "This package provides layout, editing, and control software
for your laser cutter.  Following features are supported:
@enumerate
@item Import artwork from formats such as AI, PDF, SVG, DXF, PLT, PNG, JPG, GIF
, BMP
@item arrange, edit, and even create new vector shapes within the editor
@item apply production settings such as like power, speed, number of passes,
cut order, etc.
@item send the result directly to your laser cutter
@end enumerate")
    (home-page "https://lightburnsoftware.com/")
    (license (license:nonfree "https://lightburnsoftware.com/pages/how-the-lightburn-license-works"))))
