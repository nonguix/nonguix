;;; Copyright © 2021 Mathieu Othacehe <m.othacehe@gmail.com>
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
