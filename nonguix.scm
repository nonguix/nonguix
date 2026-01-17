;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Hilton Chain <hako@ultrarare.space>

(define-module (nonguix)
  #:use-module (srfi srfi-26))

;; Re-export commonly-used modules for system setup.

(eval-when (eval load compile)
  (begin
    (define %public-modules
      '((nonguix transformations)
        (nongnu packages linux)
        (nongnu system linux-initrd)))

    (for-each (let ((i (module-public-interface (current-module))))
                (lambda (m)
                  ;; Ignore non-existent modules, so that we can split the
                  ;; channel without breaking this module in the future.
                  (and=> (false-if-exception (resolve-interface m))
                         (cut module-use! i <>))))
              %public-modules)))
