;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2024 Nicolas Graves <ngraves@ngraves.fr>

;;; This file is not part of GNU Guix but requires this naming scheme
;;; so that the %nvidia-updater is properly read when using
;;; `guix refresh -L$(pwd) nvidia-driver' in nonguix root.

(define-module (guix import nvidia)
  #:use-module (web client)
  #:use-module (sxml match)
  #:use-module (sxml simple)
  #:use-module (guix memoization)
  #:use-module (guix packages)
  #:use-module (guix upstream)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-71)
  #:export (%nvidia-updater))

(define nvidia-latest-url "https://www.nvidia.com/en-us/drivers/unix/")

(define (archive->guix-arch system)
  (match system
    ("https://www.nvidia.com/object/linux-amd64-display-archive.html"
     "x86_64-linux")
    ("https://www.nvidia.com/en-us/drivers/unix/linux-aarch64-archive/"
     "aarch64-linux")
    (_ #f)))

(define (archive? cand)
  (or (string= cand (string-append nvidia-latest-url "linux-aarch64-archive/"))
      (and (string-prefix? "https://www.nvidia.com/object/" cand)
           (string-suffix? "-archive.html" cand))))

(define nvidia-versions
  (memoize
   (lambda _
     (let* ((response content (http-get nvidia-latest-url))
            (match-str (string-match "<div id=\"rightContent\".*</div>"
                                     content))
            (greedy-right-content (match:substring match-str))
            (match-str (string-match "</div>" greedy-right-content))
            (right-content
             (string-append (match:prefix match-str) "</div>"))
            ;; xml->sxml is not flexible enough for html.
            ;; For instance, <br> tags don't have closing </br>.
            ;; This trick preprocesses html to extract all <a> tags in
            ;; a <body> wrapper, which sxml-match can handle well.
            (xml (xml->sxml
                  (string-append
                   "<body><"
                   (string-join
                    (filter (cute string-prefix? "a " <>)
                            (string-split right-content #\<))
                    "</a><")
                   "</a></body>")
                  #:trim-whitespace? #t))
            (link-alist
             (sxml-match
              xml
              ((*TOP*
                (body
                 (a (@ (href ,url)) ,version) ...))
               (fold acons
                     '()
                     (list (or (string= version "Archive")
                               (string-trim version))
                           ...)
                     (list (if (archive? url)
                               (archive->guix-arch url)
                               url)
                           ...)))))
            (system #f)
            (versions
             (fold
              (lambda (el rest)
                (match el
                  (`(#t . ,s)
                   (set! system s)
                   rest)
                  (`(,version . ,address)
                   ;; aarch64 seems to follow the same driver versions than x86_64
                   ;; KISS: use only an alist of versions
                   ;; go for an alist of alists insted if they diverge
                   (if (and (string? system) (string= system "x86_64-linux"))
                       (cons version rest)
                       rest))
                  (_ rest)))
              '()
              link-alist)))
       (fold acons '() (list "main" "latest" "beta") (take versions 3))))))

(define* (latest-release package #:key (version #f) partial-version?)
  "Return an <upstream-source> for the latest-release of PACKAGE."
  (let* ((name (package-name package))
         (kind (match name
                 ("nvidia-driver" "main")
                 ("nvidia-driver-beta" "beta")))
         (version (or version (assoc-ref (nvidia-versions) kind))))
    (upstream-source
     (package name)
     (version version)
     (urls (list (string-append
                  "https://us.download.nvidia.com/XFree86/Linux-x86_64/"
                  version "/NVIDIA-Linux-x86_64-" version ".run"))))))

(define (nvidia-package? package)
  "Return true if PACKAGE is Nvidia."
  (member (package-name package)
          (list "nvidia-driver" "nvidia-driver-beta")))

(define %nvidia-updater
  (upstream-updater
   (name 'nvidia)
   (description "Updater for Nvidia packages")
   (pred nvidia-package?)
   (import latest-release)))

;; nvidia.scm ends here.
