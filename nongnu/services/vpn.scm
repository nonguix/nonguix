;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Alexey Abramov <levenson@mmer.org>
;;;
;;; This file is not part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix. If not, see <http://www.gnu.org/licenses/>.

(define-module (nongnu services vpn)
  #:use-module (guix gexp)
  #:use-module (gnu packages)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (nongnu packages vpn)
  #:export (zerotier-one-service))

(define %zerotier-action-join
  (shepherd-action
   (name 'join)
   (documentation "Join a network")
   (procedure #~(lambda (running network)
                  (let ((zerotier-cli (string-append #$zerotier "/sbin/zerotier-cli")))
                    (invoke zerotier-cli "join" network))))))

(define %zerotier-action-leave
  (shepherd-action
   (name 'leave)
   (documentation "Leave a network")
   (procedure #~(lambda (running network)
                  (let ((zerotier-cli (string-append #$zerotier "/sbin/zerotier-cli")))
                    (invoke zerotier-cli "leave" network))))))

(define zerotier-one-shepherd-service
  (lambda (config)
    (list (shepherd-service
           (documentation "ZeroTier One daemon.")
           (provision '(zerotier-one))
           (requirement '(networking))
           (actions (list %zerotier-action-join
                          %zerotier-action-leave))
           (start #~(make-forkexec-constructor
                     (list (string-append #$zerotier "/sbin/zerotier-one"))))
           (stop #~(make-kill-destructor))))))

(define zerotier-one-service-type
  (service-type (name 'zerotier-one)
                (extensions
                 (list (service-extension shepherd-root-service-type
                                          zerotier-one-shepherd-service)))))

(define* (zerotier-one-service #:key (config (list)))
  (service zerotier-one-service-type config))
