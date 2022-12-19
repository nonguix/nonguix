;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2022 Hilton Chain <hako@ultrarare.space>

(define-module (nongnu services nvidia)
  #:use-module (gnu packages)
  #:use-module (gnu packages linux)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services linux)
  #:use-module (gnu services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (nongnu packages nvidia)
  #:export (nvidia-configuration
            nvidia-configuration?
            nvidia-configuration-record?
            nvidia-service-type))

(define-record-type* <nvidia-configuration>
  nvidia-configuration make-nvidia-configuration
  nvidia-configuration?
  (nvidia-driver   nvidia-configuration-nvidia-driver
                   (default (list nvidia-driver)))         ; list of file-like
  (nvidia-firmware nvidia-configuration-nvidia-firmware
                   (default (list nvidia-firmware)))       ; list of file-like
  (nvidia-module   nvidia-configuration-nvidia-module
                   (default (list nvidia-module)))         ; list of file-like
  (modules         nvidia-configuration-modules
                   (default (list "nvidia-uvm"))))         ; list of string

(define (nvidia-shepherd-service config)
  (list (shepherd-service
         (documentation "Unload nvidia-uvm module on powering off.")
         (provision '(nvidia))
         (requirement '(user-processes))
         (start #~(const #t))
         (stop #~(lambda _
                   (let ((rmmod #$(file-append kmod "/bin/rmmod")))
                     (zero? (system* rmmod "nvidia-uvm"))))))))

(define nvidia-service-type
  (service-type
   (name 'nvidia)
   (extensions
    (list (service-extension shepherd-root-service-type
                             nvidia-shepherd-service)
          (service-extension udev-service-type
                             nvidia-configuration-nvidia-driver)
          (service-extension firmware-service-type
                             nvidia-configuration-nvidia-firmware)
          (service-extension linux-loadable-module-service-type
                             nvidia-configuration-nvidia-module)
          (service-extension kernel-module-loader-service-type
                             nvidia-configuration-modules)))
   (default-value (nvidia-configuration))
   (description "Load NVIDIA modules.")))
