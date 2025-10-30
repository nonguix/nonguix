;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2019 Julien Lepiller <julien@lepiller.eu>

(define-module (nonguix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix derivations)
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (guix packages)
  #:use-module (guix store)
  #:use-module (ice-9 match)
  #:export (go-mod-vendor
            unredistributable-url-fetch))

(define* (go-mod-vendor #:key go)
  (lambda* (src hash-algo hash #:optional name #:key (system (%current-system)))
    "Return a fixed-output derivation to run \"go mod vendor\" against SRC and
use the produced \"vendor\" directory as the result.  The derivation is expected
to have HASH of type HASH-ALGO (a symbol).  File name of the derivation defaults
to \"vendored-go-dependencies\" and can be specified by NAME."

    (define nss-certs
      (module-ref (resolve-interface '(gnu packages nss)) 'nss-certs))

    (gexp->derivation
     (or name "vendored-go-dependencies")
     (with-imported-modules %default-gnu-imported-modules
       #~(begin
           (use-modules (guix build gnu-build-system)
                        (guix build utils))
           ;; Support Unicode in file name.
           (setlocale LC_ALL "C.UTF-8")
           ;; Use our bundled Go toolchain.
           (setenv "GOTOOLCHAIN" "local")
           ;; HTTPS support.
           (setenv "SSL_CERT_DIR" #+(file-append nss-certs "/etc/ssl/certs"))

           ((assoc-ref %standard-phases 'unpack) #:source #+src)
           (invoke #+(file-append go "/bin/go") "mod" "vendor")
           (copy-recursively "vendor" #$output)))
     #:system system
     #:hash-algo hash-algo
     #:hash hash
     ;; Is a directory.
     #:recursive? #t
     #:env-vars
     '(("GOCACHE" . "/tmp/go-cache")
       ("GOPATH" . "/tmp/go"))
     ;; Honor the user's proxy and locale settings.
     #:leaked-env-vars
     '("GOPROXY"
       "http_proxy" "https_proxy"
       "LC_ALL" "LC_MESSAGES" "LANG"
       "COLUMNS"))))

(define* (unredistributable-url-fetch url hash-algo hash
                                      #:optional name
                                      #:key (system (%current-system))
                                      (guile (default-guile)))
  "Return a fixed-output derivation that fetches URL (a string) which is expected
to have HASH of type HASH-ALGO (a symbol).  By default, the file name is the base
name of URL; optionally, NAME can specify a different file name.

This is a simpler version of url-fetch from Guix, that doesn't support mirror://
or file:// uris.  It is specifically designed to prevent substitution of the
source, for the purpose of downloading copyrighted content you have access to,
but you don't have the right to redistribute.  By marking the derivation as non
substitutable, this fetch prevents you from giving others access to the source
if you run a substitute server on your machine."
  (define file-name
    (match url
      ((head _ ...)
       (basename head))
      (_
       (basename url))))

  (mlet %store-monad ()
    (raw-derivation (or name file-name) "builtin:download" '()
                #:system system
                #:hash-algo hash-algo
                #:hash hash

                ;; Honor the user's proxy and locale settings.
                #:leaked-env-vars '("http_proxy" "https_proxy"
                                    "LC_ALL" "LC_MESSAGES" "LANG"
                                    "COLUMNS")
                #:env-vars `(("url" . ,(object->string url)))

                ;; Do not offload because the remote daemon may not support
                ;; the 'download' builtin.
                #:local-build? #t

                ;; Do not substitute copyrighted material
                #:substitutable? #f)))
