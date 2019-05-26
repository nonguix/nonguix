;;; Copyright © 2019 Alex Griffin <a@ajgrf.com>
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
