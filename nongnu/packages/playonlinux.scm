;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2019 Julien Lepiller <julien@lepiller.eu>

(define-module (nongnu packages playonlinux)
  #:use-module ((guix licenses) :prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix build-system pyproject)
  #:use-module (guix git-download)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages python)
  #:use-module (gnu packages wget)
  #:use-module (gnu packages web)
  #:use-module (gnu packages wine)
  #:use-module (gnu packages wxwidgets)
  #:use-module (gnu packages xorg))

(define-public playonlinux
  (package
    (name "playonlinux")
    (version "4.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/PlayOnLinux/POL-POM-4")
                     (commit version)))
              (sha256
               (base32
                "0jw43fmc298gb7ga2aldcdyrwlhki5k6hc198pl5x987x4gxfg2h"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:python python-2
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'hardcode-paths
            (lambda _
              ;; Do not search for python, call it from its store path
              (substitute* "bash/find_python"
                (("POL_PYTHON=\"\"")
                 (string-append "POL_PYTHON=\"" (which "python") "\"")))
              ;; Do the same for other tools
              (substitute* "python/lib/dpiFetcher.py"
                (("xrdb") (which "xrdb")))
              (substitute* "lib/setupwindow.lib"
                (("nc") (which "nc")))
              (substitute* "python/lib/Variables.py"
                (("curl") (which "curl"))
                (("wget ") (string-append (which "wget") " ")))
              (substitute* "lib/scripts.lib"
                (("\twget") (string-append "\t" (which "wget")))
                (("\tcabextract") (string-append "\t" (which "cabextract")))
                (("\tconvert") (string-append "\t" (which "convert"))))
              (substitute* "lib/playonlinux.lib"
                (("convert") (which "convert"))
                (("gpg ") (string-append (which "gpg") " ")))
              (substitute* "bash/polconfigurator"
                (("convert") (which "convert")))
              ;; Also substitute a non essential tool, that is still
              ;; needed to parse recipes
              (substitute* "lib/wine.lib"
                (("jq") (which "jq")))
              ;; Do not warn if we can't find the tools, since they are
              ;; called directly with their store path now.
              (substitute* "python/mainwindow.py"
                ((".*self.singleCheckFatal.*") "")
                ((".*self.singleCheck\\(\"jq\", package=\"jq\"\\).*") ""))))
          (replace 'build
            (lambda _
              (invoke "make" (string-append "PREFIX=" #$output))))
          (replace 'install
            (lambda _
              (invoke "make" "install" (string-append "PREFIX=" #$output))))
          (add-after 'install 'symlink-locales
            (lambda _
              (mkdir-p (string-append #$output "/share/playonlinux/lang"))
              (symlink (string-append #$output "/share/locale")
                       (string-append #$output "/share/playonlinux/lang/locale")))))))
    (inputs
     (list cabextract
           curl
           gnupg
           imagemagick
           jq
           libx11
           mesa
           netcat
           python-wxpython
           wget
           wine
           xrdb))
    (home-page "https://www.playonlinux.com/")
    (synopsis "Easy installer for Windows games")
    (description "PlayOnLinux is a piece of software which allows you to easily
install and use numerous games and apps designed to run with Microsoft
Windows.  Few games are compatible with GNU/Linux at the moment and it
certainly is a factor preventing the migration to this system.  PlayOnLinux
brings a cost-free, accessible and efficient solution to this problem.")
    (license (list license:gpl2+ license:gpl3+))))
