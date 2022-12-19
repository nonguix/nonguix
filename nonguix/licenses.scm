;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2019 Alex Griffin <a@ajgrf.com>

(define-module (nonguix licenses)
  #:use-module (guix licenses)
  #:export (nonfree
            undistributable))

(define license (@@ (guix licenses) license))

(define* (nonfree uri #:optional (comment ""))
  "Return a nonfree license, whose full text can be found
at URI, which may be a file:// URI pointing the package's tree."
  (license "Nonfree"
           uri
           (string-append
            "This a nonfree license.  Check the URI for details.  "
            comment)))

(define* (undistributable uri #:optional (comment ""))
  "Return a nonfree license for packages which may not be redistributed, whose
full text can be found at URI, which may be a file:// URI pointing the
package's tree."
  (license "Nonfree Undistributable"
           uri
           (string-append
            "This a nonfree license.  This package may NOT be redistributed "
            "in prebuilt form.  Check the URI for details.  "
            comment)))
