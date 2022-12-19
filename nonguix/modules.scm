;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2020 Alex Griffin <a@ajgrf.com>

(define-module (nonguix modules)
  #:use-module (ice-9 match)
  #:export (import-nonguix-module?))

(define (nonguix-module-name? name)
  "Return true if NAME (a list of symbols) denotes a Guix or Nonguix module."
  (match name
    (('guix _ ...) #t)
    (('gnu _ ...) #t)
    (('nonguix _ ...) #t)
    (('nongnu _ ...) #t)
    (_ #f)))

;; Since we don't use deduplication support in 'populate-store', don't
;; import (guix store deduplication) and its dependencies, which
;; includes Guile-Gcrypt.
(define (import-nonguix-module? module)
  "Return true if MODULE is not (guix store deduplication)"
  (and (nonguix-module-name? module)
       (not (equal? module '(guix store deduplication)))))
