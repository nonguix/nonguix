;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Pierre Neidhardt <mail@ambrevar.xyz>
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

(define-module (nongnu packages clojure)
  #:use-module (guix packages)
  #:use-module (guix build-system ant)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:))

(define-public leiningen
  (package
    (name "leiningen")
    (version "2.9.1")
    (source (origin
              (method url-fetch/tarbomb)
              (uri (string-append
                    "https://github.com/technomancy/leiningen/archive/"
                    version ".tar.gz"))
              (file-name (string-append name "-" version))
              (sha256
               (base32
                "0acbmgs9sq6rc24b0ly2345pvyfky03s3gzmzvi98vsp0ys3khm4"))))
    (build-system ant-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (delete 'check)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (install-file (string-append
                            "leiningen-" ,version "/bin/lein")
                           (string-append
                            (assoc-ref outputs "out") "/bin")))))))
    (home-page "https://leiningen.org")
    (synopsis "Automating Clojure projects without setting your hair on fire")
    (description "Leiningen is an easy way to use Clojure.  With a focus
on project automation and declarative configuration, it gets out of your way
and lets you focus on your code.")
    (license license:epl1.0)))
