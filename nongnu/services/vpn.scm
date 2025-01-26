;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2020 Alexey Abramov <levenson@mmer.org>
;;; Copyright © 2025 James Kalyan <mjkalyan@proton.me>

(define-module (nongnu services vpn)
  #:use-module (guix deprecation)
  #:use-module (guix gexp)
  #:use-module (gnu packages)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (nongnu packages vpn)
  #:export (zerotier-service-type
            zerotier-one-service))

(define %zerotier-action-join
  (shepherd-action
   (name 'join)
   (documentation "Join a network")
   (procedure #~(lambda (running network)
                  (let* ((zerotier-cli (string-append #$zerotier "/sbin/zerotier-cli"))
                         (cmd (string-join (list zerotier-cli "join" network)))
                         (port (open-input-pipe cmd))
                         (str (get-string-all port)))
                    (display str)
                    (status:exit-val (close-pipe port)))))))

(define %zerotier-action-leave
  (shepherd-action
   (name 'leave)
   (documentation "Leave a network")
   (procedure #~(lambda (running network)
                  (let* ((zerotier-cli (string-append #$zerotier "/sbin/zerotier-cli"))
                         (cmd (string-join (list zerotier-cli "leave" network)))
                         (port (open-input-pipe cmd))
                         (str (get-string-all port)))
                    (display str)
                    (status:exit-val (close-pipe port)))))))



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

(define zerotier-service-type
  (service-type (name 'zerotier)
                (description "ZeroTier One daemon.")
                (extensions
                 (list (service-extension shepherd-root-service-type
                                          zerotier-one-shepherd-service)))
                (default-value '())))

(define-deprecated/alias
  zerotier-one-service-type
  zerotier-service-type)

(define-deprecated (zerotier-one-service #:key (config '()))
  zerotier-service-type
  (service zerotier-one-service-type config))
