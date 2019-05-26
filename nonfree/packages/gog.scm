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

(define-module (nonfree packages gog)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:))

(define-public htmlcxx
  (package
    (name "htmlcxx")
    (version "0.87")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://sourceforge/htmlcxx/v"
                       version "/htmlcxx-" version ".tar.gz"))
       (sha256
        (base32 "1j3mzjlczjrk4ahc43s6kzpvzypzjmqz4sillnca5yadrwwgjf2x"))))
    (build-system gnu-build-system)
    (home-page "http://htmlcxx.sourceforge.net/")
    (synopsis "Simple non-validating CSS1 and HTML parser for C++")
    (description "htmlcxx is a simple non-validating CSS1 and HTML parser for
C++.  Although there are several other HTML parsers available, htmlcxx has some
characteristics that make it unique:
@itemize
@item STL like navigation of DOM tree, using excelent's tree.hh library from
Kasper Peeters
@item It is possible to reproduce exactly, character by character, the original
document from the parse tree
@item Bundled CSS parser
@item Optional parsing of attributes
@item C++ code that looks like C++ (not so true anymore)
@item Offsets of tags/elements in the original document are stored in the nodes
of the DOM tree
@end itemize
The parsing politics of htmlcxx were created trying to mimic Mozilla Firefox
(https://www.mozilla.org) behavior.  So you should expect parse trees similar
to those create by Firefox.  However, differently from Firefox, htmlcxx does
not insert non-existent stuff in your html.  Therefore, serializing the DOM
tree gives exactly the same bytes contained in the original HTML document.")
    (license (list license:lgpl2.0
                   license:asl2.0))))
