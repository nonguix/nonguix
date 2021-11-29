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
