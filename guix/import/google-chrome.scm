;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Anderson Torres <anderson.torres.8519@gmail.com>
;;; Copyright © 2026 Hilton Chain <hako@ultrarare.space>
;;;
;;; This file is not part of GNU Guix but requires this naming scheme so that
;;; the %google-chrome-updater is properly read when used in Nonguix root:
;;;
;;; guix refresh -t google-chrome -L$(pwd) google-chrome-{stable,beta,unstable,canary}

(define-module (guix import google-chrome)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (guix http-client)
  #:use-module (guix memoization)
  #:use-module (guix packages)
  #:use-module (guix upstream)
  #:export (%google-chrome-updater))

(define http-fetch*
  ;; Like http-fetch, but memoized and returning the body as a string.
  (memoize (lambda args
             (call-with-port (apply http-fetch args) get-string-all))))

(define %google-chrome-control-file-url
  "https://dl.google.com/linux/chrome/deb/dists/stable/main/binary-amd64/Packages")

(define (collect-package-and-version port)
  (define (extract-package-or-version str)
    ;; "Package: google-chrome-beta" => "google-chrome-beta"
    (second (string-split str #\space)))

  (let loop ((line (get-line port))
             (result '()))
    (if (eof-object? line)
        result
        (let ((next-line (get-line port)))
          (loop next-line
                (if (string-prefix? "Package:" line)
                    (cons (cons (extract-package-or-version line)
                                (extract-package-or-version next-line))
                          result)
                    result))))))

(define* (latest-release package #:key (version #f) partial-version?)
  "Return an <upstream-source> for the latest-release of PACKAGE."
  (let* ((file (http-fetch* %google-chrome-control-file-url))
         (versions (call-with-input-string file collect-package-and-version))
         (name (package-name package))
         (version (or version (assoc-ref versions name))))
    (upstream-source
      (package name)
      (version version)
      (urls
       (list (string-append
              "https://dl.google.com/linux/chrome/deb/pool/main/g/" name "/"
              name "_" version "_amd64.deb"))))))

(define (google-chrome-package? package)
  "Return #t if PACKAGE is Google Chrome."
  (string-prefix? "google-chrome-" (package-name package)))

(define %google-chrome-updater
  (upstream-updater
    (name 'google-chrome)
    (description "Updater for Google Chrome packages")
    (pred google-chrome-package?)
    (import latest-release)))

;;; google-chrome.scm ends here.
