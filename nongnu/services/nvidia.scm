;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2022, 2024 Hilton Chain <hako@ultrarare.space>

(define-module (nongnu services nvidia)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu packages)
  #:use-module (gnu packages linux)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services linux)
  #:use-module (gnu services shepherd)
  #:use-module (nongnu packages nvidia)
  #:export (nvidia-configuration
            nvidia-configuration?
            nvidia-configuration-record?
            nvidia-service-type))

(define-record-type* <nvidia-configuration>
  nvidia-configuration make-nvidia-configuration
  nvidia-configuration?
  (driver   nvidia-configuration-driver
            (default nvda))             ; file-like
  (firmware nvidia-configuration-firmware
            (default nvidia-firmware))  ; file-like
  (module   nvidia-configuration-module
            (default nvidia-module)))   ; file-like

(define (nvidia-shepherd-service config)
  (let ((nvidia-driver (nvidia-configuration-driver config))
        (nvidia-smi (file-append nvidia-driver "/bin/nvidia-smi"))
        (rmmod (file-append kmod "/bin/rmmod")))
    (list (shepherd-service
           (documentation "Prepare system environment for NVIDIA driver.")
           (provision '(nvidia))
           (requirement '(udev user-processes))
           (start
            #~(lambda _
                (when (file-exists? #$nvidia-smi)
                  (system* #$nvidia-smi))))
           (stop
            #~(lambda _
                (system* #$rmmod "nvidia_uvm")))))))

(define nvidia-service-type
  (service-type
   (name 'nvidia)
   (extensions
    (list (service-extension shepherd-root-service-type
                             nvidia-shepherd-service)
          (service-extension profile-service-type
                             (compose list nvidia-configuration-driver))
          (service-extension udev-service-type
                             (compose list nvidia-configuration-driver))
          (service-extension firmware-service-type
                             (compose list nvidia-configuration-firmware))
          (service-extension linux-loadable-module-service-type
                             (compose list nvidia-configuration-module))
          (service-extension kernel-module-loader-service-type
                             (const '("nvidia_uvm")))))
   (default-value (nvidia-configuration))
   (description "Prepare system environment for NVIDIA driver.")))
