;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2022, 2024, 2026 Hilton Chain <hako@ultrarare.space>

(define-module (nongnu services nvidia)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu system privilege)
  #:use-module (gnu packages)
  #:use-module (gnu packages linux)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services dbus)
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
            (default nvidia-firmware))  ; file-like or #f
  (module   nvidia-configuration-module
            (default nvidia-module))    ; file-like
  (modprobe nvidia-configuration-modprobe
            (default nvidia-modprobe))  ; file-like
  (settings nvidia-configuration-settings
            (default #f))               ; file-like or #f
  (powerd   nvidia-configuration-powerd
            (default #t)))              ; boolean

(define (%nvidia-firmware config)
  (match-record config <nvidia-configuration>
    (firmware)
    (filter identity (list firmware))))

(define (nvidia-profile config)
  (match-record config <nvidia-configuration>
    (driver settings)
    (filter identity
            (list driver
                  settings
                  nvidia-prime))))

(define (nvidia-privileged-program config)
  (match-record config <nvidia-configuration>
    (modprobe)
    (list (file-like->setuid-program
           (file-append modprobe "/bin/nvidia-modprobe")))))

;; Create paths hard-coded in NVIDIA libraries.
(define (nvidia-special-files config)
  `(("/usr/bin/nvidia-modprobe" "/run/privileged/bin/nvidia-modprobe")
    ("/usr/share/nvidia" "/run/booted-system/profile/share/nvidia")))

;; https://github.com/Frogging-Family/nvidia-all/blob/master/system/60-nvidia.rules
(define (nvidia-udev-rule config)
  (list (udev-rule "90-nvidia.rules" "\
# Device nodes are created by nvidia-modprobe, which is called by the nvidia DDX.
# In case the DDX is not started, the device nodes are never created, so call
# nvidia-modprobe in the udev rules to cover the Wayland/EGLStream and compute
# case without a started display. In the case where vfio-pci is used
# nvidia-modprobe should not be invoked.
ACTION==\"add|bind\", ATTR{vendor}==\"0x10de\", ATTR{class}==\"0x03[0-9]*\", \\
    DRIVER==\"nvidia\", TEST!=\"/dev/nvidia-uvm\", \\
    RUN+=\"/usr/bin/nvidia-modprobe\", \\
    RUN+=\"/usr/bin/nvidia-modprobe -c0 -u\"

# Enable runtime PM for NVIDIA VGA/3D controller devices
ACTION==\"bind\", SUBSYSTEM==\"pci\", ATTR{vendor}==\"0x10de\", ATTR{class}==\"0x03[0-9]*\", TEST==\"power/control\", ATTR{power/control}=\"auto\"
# Enable runtime PM for NVIDIA Audio devices
ACTION==\"bind\", SUBSYSTEM==\"pci\", ATTR{vendor}==\"0x10de\", ATTR{class}==\"0x040300\", TEST==\"power/control\", ATTR{power/control}=\"auto\"
# Enable runtime PM for NVIDIA USB xHCI Host Controller devices
ACTION==\"bind\", SUBSYSTEM==\"pci\", ATTR{vendor}==\"0x10de\", ATTR{class}==\"0x0c0330\", TEST==\"power/control\", ATTR{power/control}=\"auto\"
# Enable runtime PM for NVIDIA USB Type-C UCSI devices
ACTION==\"bind\", SUBSYSTEM==\"pci\", ATTR{vendor}==\"0x10de\", ATTR{class}==\"0x0c8000\", TEST==\"power/control\", ATTR{power/control}=\"auto\"

# Disable runtime PM for NVIDIA VGA/3D controller devices
ACTION==\"unbind\", SUBSYSTEM==\"pci\", ATTR{vendor}==\"0x10de\", ATTR{class}==\"0x03[0-9]*\", TEST==\"power/control\", ATTR{power/control}=\"on\"
# Disable runtime PM for NVIDIA Audio devices
ACTION==\"unbind\", SUBSYSTEM==\"pci\", ATTR{vendor}==\"0x10de\", ATTR{class}==\"0x040300\", TEST==\"power/control\", ATTR{power/control}=\"on\"
# Disable runtime PM for NVIDIA USB xHCI Host Controller devices
ACTION==\"unbind\", SUBSYSTEM==\"pci\", ATTR{vendor}==\"0x10de\", ATTR{class}==\"0x0c0330\", TEST==\"power/control\", ATTR{power/control}=\"on\"
# Disable runtime PM for NVIDIA USB Type-C UCSI devices
ACTION==\"unbind\", SUBSYSTEM==\"pci\", ATTR{vendor}==\"0x10de\", ATTR{class}==\"0x0c8000\", TEST==\"power/control\", ATTR{power/control}=\"on\"
")))

(define nvidia-shepherd-service
  (match-record-lambda <nvidia-configuration>
      (driver powerd)
    (if powerd
        (list (shepherd-service
                (documentation "NVIDIA Dynamic Boost support.")
                (provision '(nvidia-powerd))
                (requirement '(user-processes))
                (respawn? #f)
                (start
                 #~(make-forkexec-constructor
                    (list #$(file-append driver "/bin/nvidia-powerd"))))
                (stop #~(make-kill-destructor))))
        '())))

(define nvidia-service-type
  (service-type
   (name 'nvidia)
   (extensions
    (list (service-extension profile-service-type
                             nvidia-profile)
          (service-extension privileged-program-service-type
                             nvidia-privileged-program)
          (service-extension special-files-service-type
                             nvidia-special-files)
          (service-extension shepherd-root-service-type
                             nvidia-shepherd-service)
          (service-extension dbus-root-service-type
                             (compose list nvidia-configuration-driver))
          (service-extension udev-service-type
                             nvidia-udev-rule)
          (service-extension firmware-service-type
                             %nvidia-firmware)
          (service-extension linux-loadable-module-service-type
                             (compose list nvidia-configuration-module))))
   (default-value (nvidia-configuration))
   (description "Prepare system environment for NVIDIA driver.")))
