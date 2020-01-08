;;; Copyright © 2020 Alex Griffin <a@ajgrf.com>
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

(define-module (nongnu system linux-initrd)
  #:use-module (gnu system linux-initrd)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix utils)
  #:use-module (nongnu packages linux)
  #:use-module (nonguix modules)
  #:export (microcode-initrd))

;; See https://www.kernel.org/doc/Documentation/x86/microcode.txt

(define* (microcode-initrd file-systems
                           #:key
                           (initrd base-initrd)
                           (microcode-packages (list amd-microcode
                                                     intel-microcode))
                           #:allow-other-keys
                           #:rest rest)
  "Build INITRD, extended to include x86 processor microcode from
MICROCODE-PACKAGES."
  (let ((args (strip-keyword-arguments '(#:initrd #:microcode-packages) rest)))
    (combined-initrd (microcode-initrd* microcode-packages)
                     (apply initrd file-systems
                            args))))

(define (microcode-initrd* microcode-packages)
  "Build an uncompressed initrd containing x86 processor microcode from
MICROCODE-PACKAGES, in the format expected by the kernel."
  (define builder
    (with-imported-modules (source-module-closure
                            '((gnu build linux-initrd)
                              (guix build utils)
                              (nonguix build utils))
                            #:select? nonguix-module-name?)
      #~(begin
          (use-modules (gnu build linux-initrd)
                       (guix build utils)
                       (nonguix build utils))

          (let* ((initrd (string-append #$output "/initrd.cpio"))
                 (dest-dir "kernel/x86/microcode")
                 (amd-bin   (string-append dest-dir "/AuthenticAMD.bin"))
                 (intel-bin (string-append dest-dir "/GenuineIntel.bin")))
            (mkdir-p dest-dir)
            (for-each
             (lambda (package)
               (let ((intel-ucode (string-append package
                                                 "/lib/firmware/intel-ucode"))
                     (amd-ucode (string-append package
                                               "/lib/firmware/amd-ucode")))
                 (when (directory-exists? intel-ucode)
                   (concatenate-files (find-files intel-ucode ".*")
                                      intel-bin))
                 (when (directory-exists? amd-ucode)
                   (concatenate-files (find-files amd-ucode
                                                  "^microcode_amd.*\\.bin$")
                                      amd-bin))))
             '#$microcode-packages)

            (mkdir-p #$output)
            (write-cpio-archive initrd "kernel" #:compress? #f)))))

  (file-append (computed-file "microcode-initrd" builder)
               "/initrd.cpio"))

(define (combined-initrd . initrds)
  "Return a combined initrd, the result of concatenating INITRDS."
  (define builder
    (with-imported-modules (source-module-closure
                            '((guix build utils)
                              (nonguix build utils))
                            #:select? nonguix-module-name?)
      #~(begin
          (use-modules (guix build utils)
                       (nonguix build utils))

          ;; Use .img suffix since the result is no longer easily inspected by
          ;; standard tools like cpio and gzip.
          (let ((initrd (string-append #$output "/initrd.img")))
            (mkdir-p #$output)
            (concatenate-files '#$initrds initrd)))))

  (file-append (computed-file "combined-initrd" builder)
               "/initrd.img"))
