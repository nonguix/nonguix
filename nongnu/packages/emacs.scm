;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2020 Zhu Zihao <all_but_last@163.com>
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
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (nongnu packages emacs)
  #:use-module (guix packages)
  #:use-module (guix build-system emacs)
  #:use-module (guix download)
  #:use-module (nonguix licenses))

(define-public clhs
  (package
    (name "clhs")
    (version "0.6.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://beta.quicklisp.org/archive/clhs/2015-04-07/clhs-"
                           version
                           ".tgz"))
       (sha256
        (base32
         "1cn5bfrcawrbc8s1wb07lpr6xv8758l1n5pgkyhamagmi1r0x128"))))
    (build-system emacs-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'replace-loader
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (delete-file "clhs-use-local.el")
             (let* ((out (assoc-ref outputs "out"))
                    (hyperspec-dir (string-append out "/share/HyperSpec-7-0/")))
               (with-output-to-file "clhs.el"
                 (lambda ()
                   (format #t ";;;###autoload~%~s~%~%~s"
                           `(defun clhs-setup ()
                              (setq common-lisp-hyperspec-root ,hyperspec-dir))
                           `(provide 'clhs)))))))
         (add-after 'install 'install-doc
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (hyperspec-dir (string-append out "/share/HyperSpec-7-0/")))
               (mkdir-p hyperspec-dir)
               (copy-recursively "HyperSpec-7-0/HyperSpec" hyperspec-dir)))))))
    (home-page "http://quickdocs.org/clhs/")
    (synopsis "Offline Common Lisp HyperSpec")
    (description
     "This package bundles the full Common Lisp HyperSpec ready for offline
browsing. An Emacs package is provided for easy access.  Load it with:

@lisp
(require 'clhs)
(clhs-setup)
@end lisp
")
    (license (nonfree "http://quickdocs.org/clhs/"))))
