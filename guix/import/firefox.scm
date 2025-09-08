;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2025 Nicolas Graves <ngraves@ngraves.fr>

;;; This file is not part of GNU Guix but requires this naming scheme
;;; so that the %firefox-updater is properly read when using
;;; `guix refresh -L$(pwd) firefox' in nonguix root.

(define-module (guix import firefox)
  #:use-module (guix import json)
  #:use-module (guix memoization)
  #:use-module (guix packages)
  #:use-module (guix upstream)
  #:export (%firefox-updater))

(define firefox-json-url "https://product-details.mozilla.org/1.0/firefox_versions.json")

(define firefox-versions
  (memoize
   (lambda _
     (let ((alist (json-fetch firefox-json-url)))
       (list (cons "firefox" (assoc-ref alist "LATEST_FIREFOX_VERSION"))
             (cons "firefox-esr" (assoc-ref alist "FIREFOX_ESR")))))))

(define* (latest-release package #:key (version #f) partial-version?)
  "Return an <upstream-source> for the latest-release of PACKAGE."
  (let* ((name (package-name package))
         (version (or version (assoc-ref (firefox-versions) name))))
    (upstream-source
      (package name)
      (version version)
      (urls
       (list (string-append "https://archive.mozilla.org/pub/firefox/releases/"
                            version "/source/firefox-"
                            version ".source.tar.xz"))))))

(define (firefox-package? package)
  "Return true if PACKAGE is Firefox."
  (member (package-name package) (list "firefox" "firefox-esr")))

(define %firefox-updater
  (upstream-updater
   (name 'firefox)
   (description "Updater for Firefox packages")
   (pred firefox-package?)
   (import latest-release)))

;;; firefox.scm ends here.
