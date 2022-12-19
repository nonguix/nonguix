;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2021 Mathieu Othacehe <m.othacehe@gmail.com>

(define-module (nongnu ci)
  #:use-module (gnu ci)
  #:use-module (gnu system image)
  #:use-module (nongnu system install)
  #:use-module (srfi srfi-1)
  #:export (cuirass-jobs))

(define (cuirass-jobs store arguments)
  (define systems
    (arguments->systems arguments))

  (append-map
   (lambda (system)
     (list
      (image->job store
                  (image-with-os iso9660-image
                                 installation-os-nonfree)
                  #:name "nonfree-iso9660-image"
                  #:system system)))
   systems))
