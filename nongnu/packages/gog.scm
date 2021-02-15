;;; Copyright © 2019, 2020 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2021 Timotej Lazar <timotej.lazar@araneo.si>
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

(define-module (nongnu packages gog)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages man)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module ((guix licenses) #:prefix license:))

(define-public lgogdownloader
  (package
    (name "lgogdownloader")
    (version "3.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Sude-/lgogdownloader.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "02zn4zc9hqym81vbs88x5ayk2xb808jlvfyvn96ksx1ai4b6a4fz"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags '("-DUSE_QT_GUI=ON")
       #:tests? #f))                    ; no tests
    (inputs
     `(("boost" ,boost)
       ("curl" ,curl)
       ("htmlcxx" ,htmlcxx)
       ("jsoncpp" ,jsoncpp)
       ("liboauth" ,liboauth)
       ("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)
       ("qtwebchannel" ,qtwebchannel)
       ("qtwebengine" ,qtwebengine)
       ("rhash" ,rhash)
       ("tinyxml2" ,tinyxml2)
       ("zlib" ,zlib)))
    (native-inputs
     `(("help2man" ,help2man)
       ("pkg-config" ,pkg-config)))
    (home-page "https://sites.google.com/site/gogdownloader/")
    (synopsis "Downloader for GOG.com files")
    (description "LGOGDownloader is a client for the GOG.com download API,
allowing simple downloads and updates of games and other files from GOG.com.")
    (license license:wtfpl2)))
