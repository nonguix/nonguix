;;; Copyright © 2019, 2020 Alex Griffin <a@ajgrf.com>
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

(define-module (nongnu services kernel-modules)
  #:use-module (gnu packages linux)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:use-module (nongnu packages linux)
  #:export (load-broadcom-sta-service))

;;;
;;; broadcom-sta
;;;

(define-record-type* <load-broadcom-sta-configuration>
  load-broadcom-sta-configuration make-load-broadcom-sta-configuration
  load-broadcom-sta-configuration?
  (package load-broadcom-sta-configuration-package
           (default broadcom-sta)))

(define load-broadcom-sta-shepherd-service
  (match-lambda
    (($ <load-broadcom-sta-configuration> package)
     (list
      (shepherd-service
       (documentation "Load nonfree Broadcom wireless driver.")
       (provision '(load-broadcom-sta))
       (respawn? #f)
       (start
        #~(lambda _
            (and
             (zero? (system* "modprobe" "cfg80211"))
             (zero? (system* "modprobe" "lib80211"))
             (zero? (system* "env" (string-append
                                    "LINUX_MODULE_DIRECTORY="
                                    #$(file-append package "/lib/modules"))
                                    "modprobe" "wl"))))))))))

(define load-broadcom-sta-service-type
  (service-type
   (name 'load-broadcom-sta)
   (extensions
    (list
     (service-extension profile-service-type
                        (compose list load-broadcom-sta-configuration-package))
     (service-extension shepherd-root-service-type
                        load-broadcom-sta-shepherd-service)))
   (description "Load the nonfree Broadcom wireless driver.")
   (default-value (load-broadcom-sta-configuration))))

(define* (load-broadcom-sta-service #:key (broadcom-sta broadcom-sta))
  "Return a service that loads the nonfree Broadcom wireless driver.
This function is here only for backwards compatibility.  You should prefer
the `kernel-loadable-modules' mechanism to this now.

Users should also blacklist conflicting modules by adding the following
to kernel-arguments:
modprobe.blacklist=b43,b43legacy,ssb,bcm43xx,brcm80211,brcmfmac,brcmsmac,bcma"
  (service load-broadcom-sta-service-type
           (load-broadcom-sta-configuration
            (package broadcom-sta))))
