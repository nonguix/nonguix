;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Pierre Neidhardt <mail@ambrevar.xyz>
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

(define-module (nongnu packages mono)
  #:use-module (srfi srfi-1)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages base)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages kerberos)
  #:use-module (gnu packages libunwind)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages mono)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system copy)
  #:use-module (nonguix build-system binary))

(define-public mono-5
  (package
    (name "mono")
    (version "5.20.1.27")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://download.mono-project.com/sources/mono/"
                    name "-" version ".tar.bz2"))
              (sha256
               (base32
                "15rpwxw642ad1na93k5nj7d2lb24f21kncr924gxr00178a9x0jy"))
              (patches
               (parameterize
                   ((%patch-path
                     (map (lambda (directory)
                            (string-append directory "/nongnu/packages/patches"))
                          %load-path)))
                 (search-patches "mono-pkgconfig-before-gac.patch"
                                 "mono-mdoc-timestamping.patch")))))
    (build-system gnu-build-system)
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("glib" ,glib)
       ("libxslt" ,libxslt)
       ("perl" ,perl)
       ("python" ,python-2)
       ("cmake" ,cmake)
       ("which" ,which)
       ("libgdiplus" ,libgdiplus)
       ("libx11" ,libx11)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'make-reproducible
           (lambda _
             (substitute* "mono/mini/Makefile.in"
               (("build_date = [^;]*;")
                "build_date = (void*) 0;"))
             #t))
         ;; TODO: Update Mono certs.  We need a certificate bundle, which nss-certs does not have.
         ;; (add-after 'install 'update-mono-key-store
         ;;   (lambda* (#:key outputs inputs #:allow-other-keys)
         ;;     (let* ((out (assoc-ref outputs "out"))
         ;;            (ca (assoc-ref inputs "nss-certs"))
         ;;            (cert-sync (string-append out "/bin/cert-sync"))))
         ;;     (invoke cert-sync (string-append ca "/etc/ssl/certs/ca-bundle.crt")
         (add-after 'install 'install-gmcs
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out")))
               (symlink (string-append out "/bin/mcs")
                        (string-append out "/bin/gmcs")))
             #t))
         (add-after 'unpack 'set-env
           (lambda _ ;;* (#:key inputs #:allow-other-keys)
             ;; all tests under mcs/class fail trying to access $HOME
             (setenv "HOME" "/tmp")
             ;; ZIP files have "DOS time" which starts in Jan 1980.
             (setenv "SOURCE_DATE_EPOCH" "315532800")
             #t)))
       #:configure-flags (list
                          (string-append "--x-includes="
                                         (assoc-ref %build-inputs "libx11")
                                         "/include")
                          (string-append "--x-libraries="
                                         (assoc-ref %build-inputs "libx11")
                                         "/lib")
                          (string-append "--with-libgdiplus="
                                         (assoc-ref %build-inputs "libgdiplus")
                                         "/lib/libgdiplus.so"))
       ;; TODO: Most tests pass but something fails.  See bug#39695 and
       ;; https://github.com/mono/mono/issues/18979.
       #:tests? #f))
    (synopsis "Compiler and libraries for the C# programming language")
    (description "Mono is a compiler, vm, debugger and set of libraries for
C#, a C-style programming language from Microsoft that is very similar to
Java.")
    (home-page "https://www.mono-project.com/")
    ;; TODO: Still x11?
    (license license:x11)))

;; TODO: This can probably be upstreamed since only the check phase doesn't
;; pass (even if most of the tests succeed).
(define-public mono-6
  (package
    (name "mono")
    (version "6.8.0.105")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://download.mono-project.com/sources/mono/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "0y11c7w6r96laqckfxnk1ya42hx2c1nfqvdgbpmsk1iw9k29k1sp"))
              (patches
               (parameterize
                   ((%patch-path
                     (map (lambda (directory)
                            (string-append directory "/nongnu/packages/patches"))
                          %load-path)))
                 (search-patches "mono-pkgconfig-before-gac.patch"
                                 "mono-mdoc-timestamping.patch")))))
    (build-system gnu-build-system)
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("glib" ,glib)
       ("libxslt" ,libxslt)
       ("perl" ,perl)
       ("python" ,python-2)
       ("cmake" ,cmake)
       ("which" ,which)
       ("libgdiplus" ,libgdiplus)
       ("libx11" ,libx11)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'make-reproducible
           (lambda _
             (substitute* "mono/mini/Makefile.in"
               (("build_date = [^;]*;")
                "build_date = (void*) 0;"))
             #t))
         ;; TODO: Update Mono certs.  We need a certificate bundle, which nss-certs does not have.
         ;; (add-after 'install 'update-mono-key-store
         ;;   (lambda* (#:key outputs inputs #:allow-other-keys)
         ;;     (let* ((out (assoc-ref outputs "out"))
         ;;            (ca (assoc-ref inputs "nss-certs"))
         ;;            (cert-sync (string-append out "/bin/cert-sync"))))
         ;;     (invoke cert-sync (string-append ca "/etc/ssl/certs/ca-bundle.crt")
         (add-after 'install 'install-gmcs
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out")))
               (symlink (string-append out "/bin/mcs")
                        (string-append out "/bin/gmcs")))
             #t))
         (add-after 'unpack 'set-env
           (lambda _ ;;* (#:key inputs #:allow-other-keys)
             ;; all tests under mcs/class fail trying to access $HOME
             (setenv "HOME" "/tmp")
             ;; ZIP files have "DOS time" which starts in Jan 1980.
             (setenv "SOURCE_DATE_EPOCH" "315532800")
             #t)))
       #:configure-flags (list
                          (string-append "--x-includes="
                                         (assoc-ref %build-inputs "libx11")
                                         "/include")
                          (string-append "--x-libraries="
                                         (assoc-ref %build-inputs "libx11")
                                         "/lib")
                          (string-append "--with-libgdiplus="
                                         (assoc-ref %build-inputs "libgdiplus")
                                         "/lib/libgdiplus.so"))
       ;; TODO: Most tests pass but something fails.  See bug#39695 and
       ;; https://github.com/mono/mono/issues/18979.
       #:tests? #f))
    (synopsis "Compiler and libraries for the C# programming language")
    (description "Mono is a compiler, vm, debugger and set of libraries for
C#, a C-style programming language from Microsoft that is very similar to
Java.")
    (home-page "https://www.mono-project.com/")
    ;; TODO: Still x11?
    (license license:x11)))

;; TODO: Needs to build from source in order to upstream to Guix.
;; Nix does not do it.
;; See https://www.archlinux.org/packages/community/x86_64/dotnet-sdk/.
(define-public dotnet-sdk
  (package
    (name "dotnet-sdk")
    (version "3.1.101")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://dotnetcli.azureedge.net/dotnet/Sdk/"
             version
             "/dotnet-sdk-" version "-linux-x64.tar.gz"))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "079156dzsi1337didvv5dk1qp0ypjrrm8yw7darz8rr2928hh1m1"))))
    (build-system binary-build-system)
    (inputs
     `(("curl" ,curl)
       ("gcc" ,gcc "lib")
       ("icu" ,icu4c)
       ("krb5" ,mit-krb5)
       ("libunwind" ,libunwind)
       ("lttng-ust" ,lttng-ust)
       ("openssl" ,openssl)
       ("zlib" ,zlib)))
    (arguments
     `(#:patchelf-plan
       `(("dotnet"
          ("gcc"))
         ("sdk/3.1.101/AppHostTemplate/apphost"
          ("gcc"))
         ("packs/Microsoft.NETCore.App.Host.linux-x64/3.1.1/runtimes/linux-x64/native/apphost"
          ("gcc"))
         ,@(map (lambda (lib)
                  (list lib '("$ORIGIN"
                              "curl" "gcc" "icu" "krb5" "lttng-ust" "openssl" "zlib")))
                '("shared/Microsoft.NETCore.App/3.1.1/System.Net.Http.Native.so"
                  "shared/Microsoft.NETCore.App/3.1.1/libclrjit.so"
                  "shared/Microsoft.NETCore.App/3.1.1/System.Globalization.Native.so"
                  "shared/Microsoft.NETCore.App/3.1.1/libcoreclrtraceptprovider.so"
                  "shared/Microsoft.NETCore.App/3.1.1/System.Security.Cryptography.Native.OpenSsl.so"
                  "shared/Microsoft.NETCore.App/3.1.1/System.Net.Security.Native.so"
                  "shared/Microsoft.NETCore.App/3.1.1/libmscordaccore.so"
                  "shared/Microsoft.NETCore.App/3.1.1/System.IO.Compression.Native.so"
                  "shared/Microsoft.NETCore.App/3.1.1/libcoreclr.so"
                  "shared/Microsoft.NETCore.App/3.1.1/libmscordbi.so"
                  "shared/Microsoft.NETCore.App/3.1.1/System.Native.so"
                  "shared/Microsoft.NETCore.App/3.1.1/libdbgshim.so"
                  "shared/Microsoft.NETCore.App/3.1.1/libhostpolicy.so"
                  "packs/Microsoft.NETCore.App.Host.linux-x64/3.1.1/runtimes/linux-x64/native/libnethost.so"
                  "host/fxr/3.1.1/libhostfxr.so")))
       #:validate-runpath? #f           ; TODO: Fails because of the symlink?
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'cd-root
           (lambda _
             (chdir "..")
             #t))
         (add-after 'install 'symlink
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (mkdir-p (string-append out "/bin"))
               (symlink (string-append out "/dotnet")
                        (string-append out "/bin/dotnet"))
               #t))))))
    (supported-systems '("x86_64-linux"))
    (home-page "https://www.microsoft.com/net/core")
    (synopsis "Generic driver for the .NET Core command line interface")
    (description "This package provides a generic driver for the .NET Core
command line interface.")
    (license license:expat)))

;; TODO: Make a mono-binary-build-system.  See Nix' build-dotnet-package/default.nix.
(define-public nuget
  (let ((commit "7871fa26914593fdb2f2500df1196df7b8aecb1c")
        (basename "Nuget"))
    (package
      (name "nuget")
      (version "4.9.1.5694")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/mono/nuget-binary/")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "07r63xam6icm17pf6amh1qkmna13nxa3ncdan7a3ql307i5isriz"))))
      (build-system copy-build-system)
      (inputs
       `(("mono" ,mono-6)))
      (arguments
       `(#:strip-binaries? #f
         #:install-plan
         '(("." ,(string-append "lib/dotnet/" basename "/")))
         #:phases
         (modify-phases %standard-phases
           (add-after 'install 'make-wrapper
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (wrapper (string-append out "/bin/nuget"))
                      (real (string-append out "/lib/dotnet/" ,basename "/nuget.exe"))
                      (mono (string-append (assoc-ref inputs "mono") "/bin/mono")))
                 (mkdir-p (dirname wrapper))
                 (with-output-to-file wrapper
                   (lambda ()
                     (format #t "#!~a~%~a ~a \"$@\""
                             (which "bash")
                             mono real)))
                 (chmod wrapper #o755)
                 #t))))))
      (supported-systems '("x86_64-linux"))
      (home-page "")
      (synopsis "")
      (description "")
      ;; TODO: License?
      (license license:expat))))

;; TODO: Make fetcher.
(define* (nuget-fetch #:key name version url sha256)
  (if name
      (origin
        (method url-fetch)
        (uri url)
        (file-name (string-append name "-" version))
        (sha256
         (base32 sha256)))
      (origin
        (method url-fetch)
        (uri url)
        (sha256
         (base32 sha256)))))

(define-public msbuild
  (let ((date "2019.07.26.14.57")
        ;; (bootstrap-version "0.08")
        (mono-inputs
         '(("microsoft.build"
            #:version "14.3.0"
            #:url "https://www.nuget.org/api/v2/package/microsoft.build/14.3.0"
            #:sha256 "1zamn3p8xxi0wsjlpln0y71ncb977f3fp08mvaz4wmbmi76nr0rz")
           ("system.io"
            #:version "4.1.0"
            #:url "https://www.nuget.org/api/v2/package/system.io/4.1.0"
            #:sha256 "1g0yb8p11vfd0kbkyzlfsbsp5z44lwsvyc0h3dpw6vqnbi035ajp")
           ("system.io"
            #:version "4.3.0"
            #:url "https://www.nuget.org/api/v2/package/system.io/4.3.0"
            #:sha256 "05l9qdrzhm4s5dixmx68kxwif4l99ll5gqmh7rqgw554fx0agv5f")
           ("system.xml.xpath"
            #:version "4.3.0"
            #:url "https://www.nuget.org/api/v2/package/system.xml.xpath/4.3.0"
            #:sha256 "1cv2m0p70774a0sd1zxc8fm8jk3i5zk2bla3riqvi8gsm0r4kpci")
           ("microsoft.net.compilers.toolset"
            #:version "3.3.0-beta2-19367-02"
            #:url "https://dotnetfeed.blob.core.windows.net/dotnet-core/flatcontainer/microsoft.net.compilers.toolset/3.3.0-beta2-19367-02/microsoft.net.compilers.toolset.3.3.0-beta2-19367-02.nupkg"
            #:sha256 "1v9lz2fmfprhql0klqa8iipiiz3wcflvlgr3a86pcjjk7x0y84sl")
           ("system.io.filesystem"
            #:version "4.0.1"
            #:url "https://www.nuget.org/api/v2/package/system.io.filesystem/4.0.1"
            #:sha256 "0kgfpw6w4djqra3w5crrg8xivbanh1w9dh3qapb28q060wb9flp1")
           ("system.io.filesystem"
            #:version "4.3.0"
            #:url "https://www.nuget.org/api/v2/package/system.io.filesystem/4.3.0"
            #:sha256 "0z2dfrbra9i6y16mm9v1v6k47f0fm617vlb7s5iybjjsz6g1ilmw")
           ("largeaddressaware"
            #:version "1.0.3"
            #:url "https://www.nuget.org/api/v2/package/largeaddressaware/1.0.3"
            #:sha256 "1ppss9bgj0hf5s8307bnm2a4qm10mrymp0v12m28a5q81zjz0fr5")
           ("nuget.protocol"
            #:version "5.2.0-rtm.6067"
            #:url "https://dotnet.myget.org/F/nuget-build/api/v2/package/nuget.protocol/5.2.0-rtm.6067"
            #:sha256 "0fm3qgcdsy6dy6fih0n9a4w39mzdha4cz51gr9pp9g4nag34za2a")
           ("runtime.native.system.security.cryptography.openssl"
            #:version "4.3.0"
            #:url "https://www.nuget.org/api/v2/package/runtime.native.system.security.cryptography.openssl/4.3.0"
            #:sha256 "18pzfdlwsg2nb1jjjjzyb5qlgy6xjxzmhnfaijq5s2jw3cm3ab97")
           ("runtime.ubuntu.14.04-x64.runtime.native.system.security.cryptography.openssl"
            #:version "4.3.0"
            #:url "https://www.nuget.org/api/v2/package/runtime.ubuntu.14.04-x64.runtime.native.system.security.cryptography.openssl/4.3.0"
            #:sha256 "160p68l2c7cqmyqjwxydcvgw7lvl1cr0znkw8fp24d1by9mqc8p3")
           ("system.buffers"
            #:version "4.3.0"
            #:url "https://www.nuget.org/api/v2/package/system.buffers/4.3.0"
            #:sha256 "0fgns20ispwrfqll4q1zc1waqcmylb3zc50ys9x8zlwxh9pmd9jy")
           ("system.buffers"
            #:version "4.4.0"
            #:url "https://www.nuget.org/api/v2/package/system.buffers/4.4.0"
            #:sha256 "183f8063w8zqn99pv0ni0nnwh7fgx46qzxamwnans55hhs2l0g19")
           ("xunit.core"
            #:version "2.4.1"
            #:url "https://www.nuget.org/api/v2/package/xunit.core/2.4.1"
            #:sha256 "1nnb3j4kzmycaw1g76ii4rfqkvg6l8gqh18falwp8g28h802019a")
           ("system.io.filesystem.primitives"
            #:version "4.3.0"
            #:url "https://www.nuget.org/api/v2/package/system.io.filesystem.primitives/4.3.0"
            #:sha256 "0j6ndgglcf4brg2lz4wzsh1av1gh8xrzdsn9f0yznskhqn1xzj9c")
           ("system.io.filesystem.primitives"
            #:version "4.0.1"
            #:url "https://www.nuget.org/api/v2/package/system.io.filesystem.primitives/4.0.1"
            #:sha256 "1s0mniajj3lvbyf7vfb5shp4ink5yibsx945k6lvxa96r8la1612")
           ("system.xml.xmldocument"
            #:version "4.0.1"
            #:url "https://www.nuget.org/api/v2/package/system.xml.xmldocument/4.0.1"
            #:sha256 "0ihsnkvyc76r4dcky7v3ansnbyqjzkbyyia0ir5zvqirzan0bnl1")
           ("system.xml.xmldocument"
            #:version "4.3.0"
            #:url "https://www.nuget.org/api/v2/package/system.xml.xmldocument/4.3.0"
            #:sha256 "0bmz1l06dihx52jxjr22dyv5mxv6pj4852lx68grjm7bivhrbfwi")
           ("microsoft.build.framework"
            #:version "15.5.180"
            #:url "https://www.nuget.org/api/v2/package/microsoft.build.framework/15.5.180"
            #:sha256 "064y3a711ikx9pm9d2wyms4i3k4f9hfvn3vymhwygg7yv7gcj92z")
           ("microsoft.build.framework"
            #:version "14.3.0"
            #:url "https://www.nuget.org/api/v2/package/microsoft.build.framework/14.3.0"
            #:sha256 "0r7y1i7dbr3pb53fdrh268hyi627w85nzv2iblwyg8dzkfxraafd")
           ("system.globalization"
            #:version "4.3.0"
            #:url "https://www.nuget.org/api/v2/package/system.globalization/4.3.0"
            #:sha256 "1cp68vv683n6ic2zqh2s1fn4c2sd87g5hpp6l4d4nj4536jz98ki")
           ("system.globalization"
            #:version "4.0.11"
            #:url "https://www.nuget.org/api/v2/package/system.globalization/4.0.11"
            #:sha256 "070c5jbas2v7smm660zaf1gh0489xanjqymkvafcs4f8cdrs1d5d")
           ("microsoft.dotnet.signtool"
            #:version "1.0.0-beta.19372.10"
            #:url "https://dotnetfeed.blob.core.windows.net/dotnet-core/flatcontainer/microsoft.dotnet.signtool/1.0.0-beta.19372.10/microsoft.dotnet.signtool.1.0.0-beta.19372.10.nupkg"
            #:sha256 "1f2im2lilw10zslfclxh49knr542jy7q09p009flxsgn68riy0j6")
           ("system.runtime.handles"
            #:version "4.3.0"
            #:url "https://www.nuget.org/api/v2/package/system.runtime.handles/4.3.0"
            #:sha256 "0sw2gfj2xr7sw9qjn0j3l9yw07x73lcs97p8xfc9w1x9h5g5m7i8")
           ("system.runtime.handles"
            #:version "4.0.1"
            #:url "https://www.nuget.org/api/v2/package/system.runtime.handles/4.0.1"
            #:sha256 "1g0zrdi5508v49pfm3iii2hn6nm00bgvfpjq1zxknfjrxxa20r4g")
           ("microsoft.codeanalysis.common"
            #:version "3.0.0-beta1-61516-01"
            #:url "https://dotnet.myget.org/F/roslyn/api/v2/package/microsoft.codeanalysis.common/3.0.0-beta1-61516-01"
            #:sha256 "1qfm61yrsmihhir7n3hb5ccn1r50i39rv1g74880ma7ihjl1hz54")
           ("microsoft.netcore.platforms"
            #:version "1.0.1"
            #:url "https://www.nuget.org/api/v2/package/microsoft.netcore.platforms/1.0.1"
            #:sha256 "01al6cfxp68dscl15z7rxfw9zvhm64dncsw09a1vmdkacsa2v6lr")
           ("microsoft.netcore.platforms"
            #:version "1.1.0"
            #:url "https://www.nuget.org/api/v2/package/microsoft.netcore.platforms/1.1.0"
            #:sha256 "08vh1r12g6ykjygq5d3vq09zylgb84l63k49jc4v8faw9g93iqqm")
           ("system.reflection.primitives"
            #:version "4.3.0"
            #:url "https://www.nuget.org/api/v2/package/system.reflection.primitives/4.3.0"
            #:sha256 "04xqa33bld78yv5r93a8n76shvc8wwcdgr1qvvjh959g3rc31276")
           ("system.reflection.primitives"
            #:version "4.0.1"
            #:url "https://www.nuget.org/api/v2/package/system.reflection.primitives/4.0.1"
            #:sha256 "1bangaabhsl4k9fg8khn83wm6yial8ik1sza7401621jc6jrym28")
           ("microbuild.core"
            #:version "0.2.0"
            #:url "https://www.nuget.org/api/v2/package/microbuild.core/0.2.0"
            #:sha256 "0q4s45jskbyxfx4ay6znnvv94zma2wd85b8rwmwszd2nb0xl3194")
           ("system.diagnostics.tracesource"
            #:version "4.0.0"
            #:url "https://www.nuget.org/api/v2/package/system.diagnostics.tracesource/4.0.0"
            #:sha256 "1mc7r72xznczzf6mz62dm8xhdi14if1h8qgx353xvhz89qyxsa3h")
           ("system.runtime.numerics"
            #:version "4.3.0"
            #:url "https://www.nuget.org/api/v2/package/system.runtime.numerics/4.3.0"
            #:sha256 "19rav39sr5dky7afygh309qamqqmi9kcwvz3i0c5700v0c5cg61z")
           ("system.threading.tasks.parallel"
            #:version "4.3.0"
            #:url "https://www.nuget.org/api/v2/package/system.threading.tasks.parallel/4.3.0"
            #:sha256 "1rr3qa4hxwyj531s4nb3bwrxnxxwz617i0n9gh6x7nr7dd3ayzgh")
           ("system.threading.tasks.parallel"
            #:version "4.0.1"
            #:url "https://www.nuget.org/api/v2/package/system.threading.tasks.parallel/4.0.1"
            #:sha256 "114wdg32hr46dfsnns3pgs67kcha5jn47p5gg0mhxfn5vrkr2p75")
           ("nuget.credentials"
            #:version "5.2.0-rtm.6067"
            #:url "https://dotnet.myget.org/F/nuget-build/api/v2/package/nuget.credentials/5.2.0-rtm.6067"
            #:sha256 "07g2na590sph9li5igww74i3gqyrj5cb6gsgjh54f1f4bs4x1c4k")
           ("system.objectmodel"
            #:version "4.0.12"
            #:url "https://www.nuget.org/api/v2/package/system.objectmodel/4.0.12"
            #:sha256 "1sybkfi60a4588xn34nd9a58png36i0xr4y4v4kqpg8wlvy5krrj")
           ("system.objectmodel"
            #:version "4.3.0"
            #:url "https://www.nuget.org/api/v2/package/system.objectmodel/4.3.0"
            #:sha256 "191p63zy5rpqx7dnrb3h7prvgixmk168fhvvkkvhlazncf8r3nc2")
           ("system.xml.xmlserializer"
            #:version "4.0.11"
            #:url "https://www.nuget.org/api/v2/package/system.xml.xmlserializer/4.0.11"
            #:sha256 "01nzc3gdslw90qfykq4qzr2mdnqxjl4sj0wp3fixiwdmlmvpib5z")
           ("microsoft.codeanalysis.build.tasks"
            #:version "3.0.0-beta1-61516-01"
            #:url "https://dotnet.myget.org/F/roslyn/api/v2/package/microsoft.codeanalysis.build.tasks/3.0.0-beta1-61516-01"
            #:sha256 "1cjpqbd4i0gxhh86nvamlpkisd1krcrya6riwjhghvpjph6115vp")
           ("system.private.datacontractserialization"
            #:version "4.1.1"
            #:url "https://www.nuget.org/api/v2/package/system.private.datacontractserialization/4.1.1"
            #:sha256 "1xk9wvgzipssp1393nsg4n16zbr5481k03nkdlj954hzq5jkx89r")
           ("system.numerics.vectors"
            #:version "4.4.0"
            #:url "https://www.nuget.org/api/v2/package/system.numerics.vectors/4.4.0"
            #:sha256 "0rdvma399070b0i46c4qq1h2yvjj3k013sqzkilz4bz5cwmx1rba")
           ("microsoft.build.centralpackageversions"
            #:version "2.0.1"
            #:url "https://www.nuget.org/api/v2/package/microsoft.build.centralpackageversions/2.0.1"
            #:sha256 "17cjiaj2b98q8s89168g42jb8rhwm6062jcbv57rbkdiiwdsn55k")
           ("system.text.encoding.extensions"
            #:version "4.0.11"
            #:url "https://www.nuget.org/api/v2/package/system.text.encoding.extensions/4.0.11"
            #:sha256 "08nsfrpiwsg9x5ml4xyl3zyvjfdi4mvbqf93kjdh11j4fwkznizs")
           ("system.text.encoding.extensions"
            #:version "4.3.0"
            #:url "https://www.nuget.org/api/v2/package/system.text.encoding.extensions/4.3.0"
            #:sha256 "11q1y8hh5hrp5a3kw25cb6l00v5l5dvirkz8jr3sq00h1xgcgrxy")
           ("microsoft.visualstudio.sdk.embedinteroptypes"
            #:version "15.0.15"
            #:url "https://www.nuget.org/api/v2/package/microsoft.visualstudio.sdk.embedinteroptypes/15.0.15"
            #:sha256 "0chr3slzzcanwcyd9isx4gichqzmfh4zd3h83piw0r4xsww1wmpd")
           ("runtime.ubuntu.16.04-x64.runtime.native.system.security.cryptography.openssl"
            #:version "4.3.0"
            #:url "https://www.nuget.org/api/v2/package/runtime.ubuntu.16.04-x64.runtime.native.system.security.cryptography.openssl/4.3.0"
            #:sha256 "15zrc8fgd8zx28hdghcj5f5i34wf3l6bq5177075m2bc2j34jrqy")
           ("system.runtime.extensions"
            #:version "4.1.0"
            #:url "https://www.nuget.org/api/v2/package/system.runtime.extensions/4.1.0"
            #:sha256 "0rw4rm4vsm3h3szxp9iijc3ksyviwsv6f63dng3vhqyg4vjdkc2z")
           ("system.runtime.extensions"
            #:version "4.3.0"
            #:url "https://www.nuget.org/api/v2/package/system.runtime.extensions/4.3.0"
            #:sha256 "1ykp3dnhwvm48nap8q23893hagf665k0kn3cbgsqpwzbijdcgc60")
           ("system.resources.extensions"
            #:version "4.6.0-preview8.19364.1"
            #:url "https://dotnetfeed.blob.core.windows.net/dotnet-core/flatcontainer/system.resources.extensions/4.6.0-preview8.19364.1/system.resources.extensions.4.6.0-preview8.19364.1.nupkg"
            #:sha256 "0jh9ilbicmsngv77a4ayzs0n7s440ycdf726nbljw029gq4rzvqf")
           ("nuget.frameworks"
            #:version "5.2.0-rtm.6067"
            #:url "https://dotnet.myget.org/F/nuget-build/api/v2/package/nuget.frameworks/5.2.0-rtm.6067"
            #:sha256 "1g1kcfqhxr1bhl3ksbdmz3rb9nq1qmkac1sijf9ng4gmr9fmprdm")
           ("system.diagnostics.diagnosticsource"
            #:version "4.3.0"
            #:url "https://www.nuget.org/api/v2/package/system.diagnostics.diagnosticsource/4.3.0"
            #:sha256 "0z6m3pbiy0qw6rn3n209rrzf9x1k4002zh90vwcrsym09ipm2liq")
           ("system.security.claims"
            #:version "4.3.0"
            #:url "https://www.nuget.org/api/v2/package/system.security.claims/4.3.0"
            #:sha256 "0jvfn7j22l3mm28qjy3rcw287y9h65ha4m940waaxah07jnbzrhn")
           ("system.linq.expressions"
            #:version "4.3.0"
            #:url "https://www.nuget.org/api/v2/package/system.linq.expressions/4.3.0"
            #:sha256 "0ky2nrcvh70rqq88m9a5yqabsl4fyd17bpr63iy2mbivjs2nyypv")
           ("system.diagnostics.stacktrace"
            #:version "4.3.0"
            #:url "https://www.nuget.org/api/v2/package/system.diagnostics.stacktrace/4.3.0"
            #:sha256 "0ash4h9k0m7xsm0yl79r0ixrdz369h7y922wipp5gladmlbvpyjd")
           ("runtime.osx.10.10-x64.runtime.native.system.security.cryptography.openssl"
            #:version "4.3.0"
            #:url "https://www.nuget.org/api/v2/package/runtime.osx.10.10-x64.runtime.native.system.security.cryptography.openssl/4.3.0"
            #:sha256 "0zcxjv5pckplvkg0r6mw3asggm7aqzbdjimhvsasb0cgm59x09l3")
           ("system.diagnostics.tracing"
            #:version "4.3.0"
            #:url "https://www.nuget.org/api/v2/package/system.diagnostics.tracing/4.3.0"
            #:sha256 "1m3bx6c2s958qligl67q7grkwfz3w53hpy7nc97mh6f7j5k168c4")
           ("system.diagnostics.tracing"
            #:version "4.1.0"
            #:url "https://www.nuget.org/api/v2/package/system.diagnostics.tracing/4.1.0"
            #:sha256 "1d2r76v1x610x61ahfpigda89gd13qydz6vbwzhpqlyvq8jj6394")
           ("xunit.analyzers"
            #:version "0.10.0"
            #:url "https://www.nuget.org/api/v2/package/xunit.analyzers/0.10.0"
            #:sha256 "15n02q3akyqbvkp8nq75a8rd66d4ax0rx8fhdcn8j78pi235jm7j")
           ("xunit.assert"
            #:version "2.4.1"
            #:url "https://www.nuget.org/api/v2/package/xunit.assert/2.4.1"
            #:sha256 "1imynzh80wxq2rp9sc4gxs4x1nriil88f72ilhj5q0m44qqmqpc6")
           ("system.appcontext"
            #:version "4.1.0"
            #:url "https://www.nuget.org/api/v2/package/system.appcontext/4.1.0"
            #:sha256 "0fv3cma1jp4vgj7a8hqc9n7hr1f1kjp541s6z0q1r6nazb4iz9mz")
           ("system.appcontext"
            #:version "4.3.0"
            #:url "https://www.nuget.org/api/v2/package/system.appcontext/4.3.0"
            #:sha256 "1649qvy3dar900z3g817h17nl8jp4ka5vcfmsr05kh0fshn7j3ya")
           ("system.text.encoding.codepages"
            #:version "4.3.0"
            #:url "https://www.nuget.org/api/v2/package/system.text.encoding.codepages/4.3.0"
            #:sha256 "0lgxg1gn7pg7j0f942pfdc9q7wamzxsgq3ng248ikdasxz0iadkv")
           ("system.text.encoding.codepages"
            #:version "4.0.1"
            #:url "https://www.nuget.org/api/v2/package/system.text.encoding.codepages/4.0.1"
            #:sha256 "00wpm3b9y0k996rm9whxprngm8l500ajmzgy2ip9pgwk0icp06y3")
           ("runtime.fedora.24-x64.runtime.native.system.security.cryptography.openssl"
            #:version "4.3.0"
            #:url "https://www.nuget.org/api/v2/package/runtime.fedora.24-x64.runtime.native.system.security.cryptography.openssl/4.3.0"
            #:sha256 "0c2p354hjx58xhhz7wv6div8xpi90sc6ibdm40qin21bvi7ymcaa")
           ("microsoft.codeanalysis.csharp"
            #:version "3.0.0-beta1-61516-01"
            #:url "https://dotnet.myget.org/F/roslyn/api/v2/package/microsoft.codeanalysis.csharp/3.0.0-beta1-61516-01"
            #:sha256 "0a7npkdw6s5jczw1lkm63x2bpz1z3ccid20h5nm6k78cv7sihm4h")
           ("system.console"
            #:version "4.0.0"
            #:url "https://www.nuget.org/api/v2/package/system.console/4.0.0"
            #:sha256 "0ynxqbc3z1nwbrc11hkkpw9skw116z4y9wjzn7id49p9yi7mzmlf")
           ("system.console"
            #:version "4.3.0"
            #:url "https://www.nuget.org/api/v2/package/system.console/4.3.0"
            #:sha256 "1flr7a9x920mr5cjsqmsy9wgnv3lvd0h1g521pdr1lkb2qycy7ay")
           ("system.reflection.typeextensions"
            #:version "4.1.0"
            #:url "https://www.nuget.org/api/v2/package/system.reflection.typeextensions/4.1.0"
            #:sha256 "1bjli8a7sc7jlxqgcagl9nh8axzfl11f4ld3rjqsyxc516iijij7")
           ("system.runtime.compilerservices.unsafe"
            #:version "4.5.2"
            #:url "https://www.nuget.org/api/v2/package/system.runtime.compilerservices.unsafe/4.5.2"
            #:sha256 "1vz4275fjij8inf31np78hw50al8nqkngk04p3xv5n4fcmf1grgi")
           ("system.threading.tasks"
            #:version "4.3.0"
            #:url "https://www.nuget.org/api/v2/package/system.threading.tasks/4.3.0"
            #:sha256 "134z3v9abw3a6jsw17xl3f6hqjpak5l682k2vz39spj4kmydg6k7")
           ("system.threading.tasks"
            #:version "4.0.11"
            #:url "https://www.nuget.org/api/v2/package/system.threading.tasks/4.0.11"
            #:sha256 "0nr1r41rak82qfa5m0lhk9mp0k93bvfd7bbd9sdzwx9mb36g28p5")
           ("xunit.abstractions"
            #:version "2.0.3"
            #:url "https://www.nuget.org/api/v2/package/xunit.abstractions/2.0.3"
            #:sha256 "00wl8qksgkxld76fgir3ycc5rjqv1sqds6x8yx40927q5py74gfh")
           ("microsoft.build.utilities.core"
            #:version "15.5.180"
            #:url "https://www.nuget.org/api/v2/package/microsoft.build.utilities.core/15.5.180"
            #:sha256 "0c4bjhaqgc98bchln8p5d2p1vyn8qrha2b8gpn2l7bnznbcrd630")
           ("microsoft.build.utilities.core"
            #:version "14.3.0"
            #:url "https://www.nuget.org/api/v2/package/microsoft.build.utilities.core/14.3.0"
            #:sha256 "0351nsnx12nzkss6vaqwwh7d7car7hrgyh0vyd4bl83c4x3ls1kb")
           ("system.reflection.emit"
            #:version "4.0.1"
            #:url "https://www.nuget.org/api/v2/package/system.reflection.emit/4.0.1"
            #:sha256 "0ydqcsvh6smi41gyaakglnv252625hf29f7kywy2c70nhii2ylqp")
           ("microsoft.visualstudio.setup.configuration.interop"
            #:version "1.16.30"
            #:url "https://www.nuget.org/api/v2/package/microsoft.visualstudio.setup.configuration.interop/1.16.30"
            #:sha256 "14022lx03vdcqlvbbdmbsxg5pqfx1rfq2jywxlyaz9v68cvsb0g4")
           ("system.net.sockets"
            #:version "4.3.0"
            #:url "https://www.nuget.org/api/v2/package/system.net.sockets/4.3.0"
            #:sha256 "1ssa65k6chcgi6mfmzrznvqaxk8jp0gvl77xhf1hbzakjnpxspla")
           ("microsoft.dotnet.arcade.sdk"
            #:version "1.0.0-beta.19372.10"
            #:url "https://dotnetfeed.blob.core.windows.net/dotnet-core/flatcontainer/microsoft.dotnet.arcade.sdk/1.0.0-beta.19372.10/microsoft.dotnet.arcade.sdk.1.0.0-beta.19372.10.nupkg"
            #:sha256 "1lii0yg4fbsma80mmvw2zwplc26abb46q6gkxwbsbkyszkw128hv")
           ("runtime.fedora.23-x64.runtime.native.system.security.cryptography.openssl"
            #:version "4.3.0"
            #:url "https://www.nuget.org/api/v2/package/runtime.fedora.23-x64.runtime.native.system.security.cryptography.openssl/4.3.0"
            #:sha256 "0hkg03sgm2wyq8nqk6dbm9jh5vcq57ry42lkqdmfklrw89lsmr59")
           ("runtime.native.system.io.compression"
            #:version "4.3.0"
            #:url "https://www.nuget.org/api/v2/package/runtime.native.system.io.compression/4.3.0"
            #:sha256 "1vvivbqsk6y4hzcid27pqpm5bsi6sc50hvqwbcx8aap5ifrxfs8d")
           ("system.diagnostics.debug"
            #:version "4.3.0"
            #:url "https://www.nuget.org/api/v2/package/system.diagnostics.debug/4.3.0"
            #:sha256 "00yjlf19wjydyr6cfviaph3vsjzg3d5nvnya26i2fvfg53sknh3y")
           ("system.diagnostics.debug"
            #:version "4.0.11"
            #:url "https://www.nuget.org/api/v2/package/system.diagnostics.debug/4.0.11"
            #:sha256 "0gmjghrqmlgzxivd2xl50ncbglb7ljzb66rlx8ws6dv8jm0d5siz")
           ("system.xml.readerwriter"
            #:version "4.3.0"
            #:url "https://www.nuget.org/api/v2/package/system.xml.readerwriter/4.3.0"
            #:sha256 "0c47yllxifzmh8gq6rq6l36zzvw4kjvlszkqa9wq3fr59n0hl3s1")
           ("system.xml.readerwriter"
            #:version "4.0.11"
            #:url "https://www.nuget.org/api/v2/package/system.xml.readerwriter/4.0.11"
            #:sha256 "0c6ky1jk5ada9m94wcadih98l6k1fvf6vi7vhn1msjixaha419l5")
           ("system.threading.timer"
            #:version "4.3.0"
            #:url "https://www.nuget.org/api/v2/package/system.threading.timer/4.3.0"
            #:sha256 "1nx773nsx6z5whv8kaa1wjh037id2f1cxhb69pvgv12hd2b6qs56")
           ("system.threading.timer"
            #:version "4.0.1"
            #:url "https://www.nuget.org/api/v2/package/system.threading.timer/4.0.1"
            #:sha256 "15n54f1f8nn3mjcjrlzdg6q3520571y012mx7v991x2fvp73lmg6")
           ("system.reflection.metadata"
            #:version "1.4.2"
            #:url "https://www.nuget.org/api/v2/package/system.reflection.metadata/1.4.2"
            #:sha256 "08b7b43vczlliv8k7q43jinjfrxwpljsglw7sxmc6sd7d54pd1vi")
           ("system.reflection.metadata"
            #:version "1.6.0"
            #:url "https://www.nuget.org/api/v2/package/system.reflection.metadata/1.6.0"
            #:sha256 "1wdbavrrkajy7qbdblpbpbalbdl48q3h34cchz24gvdgyrlf15r4")
           ("system.xml.xdocument"
            #:version "4.3.0"
            #:url "https://www.nuget.org/api/v2/package/system.xml.xdocument/4.3.0"
            #:sha256 "08h8fm4l77n0nd4i4fk2386y809bfbwqb7ih9d7564ifcxr5ssxd")
           ("system.linq"
            #:version "4.3.0"
            #:url "https://www.nuget.org/api/v2/package/system.linq/4.3.0"
            #:sha256 "1w0gmba695rbr80l1k2h4mrwzbzsyfl2z4klmpbsvsg5pm4a56s7")
           ("system.linq"
            #:version "4.1.0"
            #:url "https://www.nuget.org/api/v2/package/system.linq/4.1.0"
            #:sha256 "1ppg83svb39hj4hpp5k7kcryzrf3sfnm08vxd5sm2drrijsla2k5")
           ("nuget.librarymodel"
            #:version "5.2.0-rtm.6067"
            #:url "https://dotnet.myget.org/F/nuget-build/api/v2/package/nuget.librarymodel/5.2.0-rtm.6067"
            #:sha256 "0dxvnspgkc1lcmilb67kkipg39ih34cmifs6jwk9kbrwf96z51q9")
           ("xlifftasks"
            #:version "1.0.0-beta.19252.1"
            #:url "https://dotnetfeed.blob.core.windows.net/dotnet-core/flatcontainer/xlifftasks/1.0.0-beta.19252.1/xlifftasks.1.0.0-beta.19252.1.nupkg"
            #:sha256 "0249sfb30y9dgsfryaj8644qw3yc1xp2xzc08lsrwvmm8vjcvkri")
           ("system.text.regularexpressions"
            #:version "4.3.0"
            #:url "https://www.nuget.org/api/v2/package/system.text.regularexpressions/4.3.0"
            #:sha256 "1bgq51k7fwld0njylfn7qc5fmwrk2137gdq7djqdsw347paa9c2l")
           ("system.text.regularexpressions"
            #:version "4.1.0"
            #:url "https://www.nuget.org/api/v2/package/system.text.regularexpressions/4.1.0"
            #:sha256 "1mw7vfkkyd04yn2fbhm38msk7dz2xwvib14ygjsb8dq2lcvr18y7")
           ("system.security.accesscontrol"
            #:version "4.3.0"
            #:url "https://www.nuget.org/api/v2/package/system.security.accesscontrol/4.3.0"
            #:sha256 "1gakrskmlmwhzmjc1c2mrwk0fml615rsk31dw0kbjnn9yqnnrjbi")
           ("xunit.runner.visualstudio"
            #:version "2.4.1"
            #:url "https://www.nuget.org/api/v2/package/xunit.runner.visualstudio/2.4.1"
            #:sha256 "0fln5pk18z98gp0zfshy1p9h6r9wc55nyqhap34k89yran646vhn")
           ("system.resources.resourcemanager"
            #:version "4.0.1"
            #:url "https://www.nuget.org/api/v2/package/system.resources.resourcemanager/4.0.1"
            #:sha256 "0b4i7mncaf8cnai85jv3wnw6hps140cxz8vylv2bik6wyzgvz7bi")
           ("system.resources.resourcemanager"
            #:version "4.3.0"
            #:url "https://www.nuget.org/api/v2/package/system.resources.resourcemanager/4.3.0"
            #:sha256 "0sjqlzsryb0mg4y4xzf35xi523s4is4hz9q4qgdvlvgivl7qxn49")
           ("nuget.projectmodel"
            #:version "5.2.0-rtm.6067"
            #:url "https://dotnet.myget.org/F/nuget-build/api/v2/package/nuget.projectmodel/5.2.0-rtm.6067"
            #:sha256 "1s5950nbcsnfrpbaxdnl6cv1xbsa57fln04lhyrki536476a6wcn")
           ("nuget.versioning"
            #:version "5.2.0-rtm.6067"
            #:url "https://dotnet.myget.org/F/nuget-build/api/v2/package/nuget.versioning/5.2.0-rtm.6067"
            #:sha256 "04rr31ms95h7ymqxlalpv3xs48j8ng4ljfz5lmrfw7547rhcrj2h")
           ("system.memory"
            #:version "4.5.3"
            #:url "https://www.nuget.org/api/v2/package/system.memory/4.5.3"
            #:sha256 "0naqahm3wljxb5a911d37mwjqjdxv9l0b49p5dmfyijvni2ppy8a")
           ("system.resources.reader"
            #:version "4.0.0"
            #:url "https://www.nuget.org/api/v2/package/system.resources.reader/4.0.0"
            #:sha256 "1jafi73dcf1lalrir46manq3iy6xnxk2z7gpdpwg4wqql7dv3ril")
           ("nuget.common"
            #:version "5.2.0-rtm.6067"
            #:url "https://dotnet.myget.org/F/nuget-build/api/v2/package/nuget.common/5.2.0-rtm.6067"
            #:sha256 "1ff5dhkv8v04n2kr5gyjjvki4mqsp1w4dwsgj7cvdcfcm8alba0m")
           ("runtime.native.system"
            #:version "4.0.0"
            #:url "https://www.nuget.org/api/v2/package/runtime.native.system/4.0.0"
            #:sha256 "1ppk69xk59ggacj9n7g6fyxvzmk1g5p4fkijm0d7xqfkig98qrkf")
           ("runtime.native.system"
            #:version "4.3.0"
            #:url "https://www.nuget.org/api/v2/package/runtime.native.system/4.3.0"
            #:sha256 "15hgf6zaq9b8br2wi1i3x0zvmk410nlmsmva9p0bbg73v6hml5k4")
           ("system.runtime.interopservices"
            #:version "4.1.0"
            #:url "https://www.nuget.org/api/v2/package/system.runtime.interopservices/4.1.0"
            #:sha256 "01kxqppx3dr3b6b286xafqilv4s2n0gqvfgzfd4z943ga9i81is1")
           ("system.runtime.interopservices"
            #:version "4.3.0"
            #:url "https://www.nuget.org/api/v2/package/system.runtime.interopservices/4.3.0"
            #:sha256 "00hywrn4g7hva1b2qri2s6rabzwgxnbpw9zfxmz28z09cpwwgh7j")
           ("microbuild.core.sentinel"
            #:version "1.0.0"
            #:url "https://dotnetfeed.blob.core.windows.net/dotnet-core/flatcontainer/microbuild.core.sentinel/1.0.0/microbuild.core.sentinel.1.0.0.nupkg"
            #:sha256 "035kqx5fkapql108n222lz8psvxk04mv3dy1qg3h08i4b8j3dy8i")
           ("sn"
            #:version "1.0.0"
            #:url "https://dotnetfeed.blob.core.windows.net/dotnet-core/flatcontainer/sn/1.0.0/sn.1.0.0.nupkg"
            #:sha256 "1012fcdc6vq2355v86h434s6p2nnqgpdapb7p25l4h39g5q8p1qs")
           ("system.text.encoding"
            #:version "4.0.11"
            #:url "https://www.nuget.org/api/v2/package/system.text.encoding/4.0.11"
            #:sha256 "1dyqv0hijg265dwxg6l7aiv74102d6xjiwplh2ar1ly6xfaa4iiw")
           ("system.text.encoding"
            #:version "4.3.0"
            #:url "https://www.nuget.org/api/v2/package/system.text.encoding/4.3.0"
            #:sha256 "1f04lkir4iladpp51sdgmis9dj4y8v08cka0mbmsy0frc9a4gjqr")
           ("runtime.ubuntu.16.10-x64.runtime.native.system.security.cryptography.openssl"
            #:version "4.3.0"
            #:url "https://www.nuget.org/api/v2/package/runtime.ubuntu.16.10-x64.runtime.native.system.security.cryptography.openssl/4.3.0"
            #:sha256 "1p4dgxax6p7rlgj4q73k73rslcnz4wdcv8q2flg1s8ygwcm58ld5")
           ("system.reflection.emit.lightweight"
            #:version "4.0.1"
            #:url "https://www.nuget.org/api/v2/package/system.reflection.emit.lightweight/4.0.1"
            #:sha256 "1s4b043zdbx9k39lfhvsk68msv1nxbidhkq6nbm27q7sf8xcsnxr")
           ("microsoft.net.test.sdk"
            #:version "15.9.0"
            #:url "https://www.nuget.org/api/v2/package/microsoft.net.test.sdk/15.9.0"
            #:sha256 "0g7wjgiigs4v8qa32g9ysqgx8bx55dzmbxfkc4ic95mpd1vkjqxw")
           ("system.io.compression"
            #:version "4.3.0"
            #:url "https://www.nuget.org/api/v2/package/system.io.compression/4.3.0"
            #:sha256 "084zc82yi6yllgda0zkgl2ys48sypiswbiwrv7irb3r0ai1fp4vz")
           ("system.runtime.serialization.primitives"
            #:version "4.1.1"
            #:url "https://www.nuget.org/api/v2/package/system.runtime.serialization.primitives/4.1.1"
            #:sha256 "042rfjixknlr6r10vx2pgf56yming8lkjikamg3g4v29ikk78h7k")
           ("system.diagnostics.fileversioninfo"
            #:version "4.3.0"
            #:url "https://www.nuget.org/api/v2/package/system.diagnostics.fileversioninfo/4.3.0"
            #:sha256 "094hx249lb3vb336q7dg3v257hbxvz2jnalj695l7cg5kxzqwai7")
           ("system.xml.xpath.xdocument"
            #:version "4.3.0"
            #:url "https://www.nuget.org/api/v2/package/system.xml.xpath.xdocument/4.3.0"
            #:sha256 "1wxckyb7n1pi433xzz0qcwcbl1swpra64065mbwwi8dhdc4kiabn")
           ("system.security.principal.windows"
            #:version "4.3.0"
            #:url "https://www.nuget.org/api/v2/package/system.security.principal.windows/4.3.0"
            #:sha256 "00a0a7c40i3v4cb20s2cmh9csb5jv2l0frvnlzyfxh848xalpdwr")
           ("vswhere"
            #:version "2.6.7"
            #:url "https://www.nuget.org/api/v2/package/vswhere/2.6.7"
            #:sha256 "0h4k5i96p7633zzf4xsv7615f9x72rr5qr7b9934ri2y6gshfcwk")
           ("runtime.opensuse.13.2-x64.runtime.native.system.security.cryptography.openssl"
            #:version "4.3.0"
            #:url "https://www.nuget.org/api/v2/package/runtime.opensuse.13.2-x64.runtime.native.system.security.cryptography.openssl/4.3.0"
            #:sha256 "0qyynf9nz5i7pc26cwhgi8j62ps27sqmf78ijcfgzab50z9g8ay3")
           ("xunit.runner.console"
            #:version "2.4.1"
            #:url "https://www.nuget.org/api/v2/package/xunit.runner.console/2.4.1"
            #:sha256 "13ykz9anhz72xc4q6byvdfwrp54hlcbl6zsfapwfhnzyvfgb9w13")
           ("system.threading"
            #:version "4.0.11"
            #:url "https://www.nuget.org/api/v2/package/system.threading/4.0.11"
            #:sha256 "19x946h926bzvbsgj28csn46gak2crv2skpwsx80hbgazmkgb1ls")
           ("system.threading"
            #:version "4.3.0"
            #:url "https://www.nuget.org/api/v2/package/system.threading/4.3.0"
            #:sha256 "0rw9wfamvhayp5zh3j7p1yfmx9b5khbf4q50d8k5rk993rskfd34")
           ("system.threading.tasks.dataflow"
            #:version "4.5.24"
            #:url "https://www.nuget.org/api/v2/package/system.threading.tasks.dataflow/4.5.24"
            #:sha256 "0wahbfdb0jxx3hi04xggfms8wgf68wmvv68m2vfp8v2kiqr5mr2r")
           ("microsoft.codeanalysis.analyzers"
            #:version "1.1.0"
            #:url "https://www.nuget.org/api/v2/package/microsoft.codeanalysis.analyzers/1.1.0"
            #:sha256 "08r667hj2259wbim1p3al5qxkshydykmb7nd9ygbjlg4mmydkapc")
           ("system.dynamic.runtime"
            #:version "4.3.0"
            #:url "https://www.nuget.org/api/v2/package/system.dynamic.runtime/4.3.0"
            #:sha256 "1d951hrvrpndk7insiag80qxjbf2y0y39y8h5hnq9612ws661glk")
           ("system.io.pipes"
            #:version "4.3.0"
            #:url "https://www.nuget.org/api/v2/package/system.io.pipes/4.3.0"
            #:sha256 "1ygv16gzpi9cnlzcqwijpv7055qc50ynwg3vw29vj1q3iha3h06r")
           ("system.net.primitives"
            #:version "4.3.0"
            #:url "https://www.nuget.org/api/v2/package/system.net.primitives/4.3.0"
            #:sha256 "0c87k50rmdgmxx7df2khd9qj7q35j9rzdmm2572cc55dygmdk3ii")
           ("system.runtime.serialization.xml"
            #:version "4.1.1"
            #:url "https://www.nuget.org/api/v2/package/system.runtime.serialization.xml/4.1.1"
            #:sha256 "11747an5gbz821pwahaim3v82gghshnj9b5c4cw539xg5a3gq7rk")
           ("system.security.cryptography.encoding"
            #:version "4.3.0"
            #:url "https://www.nuget.org/api/v2/package/system.security.cryptography.encoding/4.3.0"
            #:sha256 "1jr6w70igqn07k5zs1ph6xja97hxnb3mqbspdrff6cvssgrixs32")
           ("system.collections.nongeneric"
            #:version "4.0.1"
            #:url "https://www.nuget.org/api/v2/package/system.collections.nongeneric/4.0.1"
            #:sha256 "19994r5y5bpdhj7di6w047apvil8lh06lh2c2yv9zc4fc5g9bl4d")
           ("system.diagnostics.tools"
            #:version "4.3.0"
            #:url "https://www.nuget.org/api/v2/package/system.diagnostics.tools/4.3.0"
            #:sha256 "0in3pic3s2ddyibi8cvgl102zmvp9r9mchh82ns9f0ms4basylw1")
           ("microsoft.netframework.referenceassemblies"
            #:version "1.0.0-alpha-004"
            #:url "https://dotnet.myget.org/F/roslyn-tools/api/v2/package/microsoft.netframework.referenceassemblies/1.0.0-alpha-004"
            #:sha256 "1qrpxhcx11v92lqwvrih88mlyfw2rkrsjqh7gl8c1h71vyppr3bp")
           ("system.reflection.emit.ilgeneration"
            #:version "4.0.1"
            #:url "https://www.nuget.org/api/v2/package/system.reflection.emit.ilgeneration/4.0.1"
            #:sha256 "1pcd2ig6bg144y10w7yxgc9d22r7c7ww7qn1frdfwgxr24j9wvv0")
           ("xunit.extensibility.execution"
            #:version "2.4.1"
            #:url "https://www.nuget.org/api/v2/package/xunit.extensibility.execution/2.4.1"
            #:sha256 "1pbilxh1gp2ywm5idfl0klhl4gb16j86ib4x83p8raql1dv88qia")
           ("microsoft.codecoverage"
            #:version "15.9.0"
            #:url "https://www.nuget.org/api/v2/package/microsoft.codecoverage/15.9.0"
            #:sha256 "10v5xrdilnm362g9545qxvlrbwc9vn65jhpb1i0jlhyqsj6bfwzg")
           ("xunit.extensibility.core"
            #:version "2.4.1"
            #:url "https://www.nuget.org/api/v2/package/xunit.extensibility.core/2.4.1"
            #:sha256 "103qsijmnip2pnbhciqyk2jyhdm6snindg5z2s57kqf5pcx9a050")
           ("system.collections.concurrent"
            #:version "4.0.12"
            #:url "https://www.nuget.org/api/v2/package/system.collections.concurrent/4.0.12"
            #:sha256 "07y08kvrzpak873pmyxs129g1ch8l27zmg51pcyj2jvq03n0r0fc")
           ("system.collections.concurrent"
            #:version "4.3.0"
            #:url "https://www.nuget.org/api/v2/package/system.collections.concurrent/4.3.0"
            #:sha256 "0wi10md9aq33jrkh2c24wr2n9hrpyamsdhsxdcnf43b7y86kkii8")
           ("system.collections"
            #:version "4.0.11"
            #:url "https://www.nuget.org/api/v2/package/system.collections/4.0.11"
            #:sha256 "1ga40f5lrwldiyw6vy67d0sg7jd7ww6kgwbksm19wrvq9hr0bsm6")
           ("system.collections"
            #:version "4.3.0"
            #:url "https://www.nuget.org/api/v2/package/system.collections/4.3.0"
            #:sha256 "19r4y64dqyrq6k4706dnyhhw7fs24kpp3awak7whzss39dakpxk9")
           ("runtime.opensuse.42.1-x64.runtime.native.system.security.cryptography.openssl"
            #:version "4.3.0"
            #:url "https://www.nuget.org/api/v2/package/runtime.opensuse.42.1-x64.runtime.native.system.security.cryptography.openssl/4.3.0"
            #:sha256 "1klrs545awhayryma6l7g2pvnp9xy4z0r1i40r80zb45q3i9nbyf")
           ("microsoft.build.nugetsdkresolver"
            #:version "5.2.0-rtm.6067"
            #:url "https://dotnet.myget.org/F/nuget-build/api/v2/package/microsoft.build.nugetsdkresolver/5.2.0-rtm.6067"
            #:sha256 "1rz2i4md7b8rlybb9s7416l0pr357f3ar149s6ipfq0xijn3xgmh")
           ("system.reflection"
            #:version "4.1.0"
            #:url "https://www.nuget.org/api/v2/package/system.reflection/4.1.0"
            #:sha256 "1js89429pfw79mxvbzp8p3q93il6rdff332hddhzi5wqglc4gml9")
           ("system.reflection"
            #:version "4.3.0"
            #:url "https://www.nuget.org/api/v2/package/system.reflection/4.3.0"
            #:sha256 "0xl55k0mw8cd8ra6dxzh974nxif58s3k1rjv1vbd7gjbjr39j11m")
           ("nuget.configuration"
            #:version "5.2.0-rtm.6067"
            #:url "https://dotnet.myget.org/F/nuget-build/api/v2/package/nuget.configuration/5.2.0-rtm.6067"
            #:sha256 "075mypb32i0d0x73rcr0di6pb0bhlp0izv3633ky64kddriajma1")
           ("system.net.http"
            #:version "4.3.0"
            #:url "https://www.nuget.org/api/v2/package/system.net.http/4.3.0"
            #:sha256 "1i4gc757xqrzflbk7kc5ksn20kwwfjhw9w7pgdkn19y3cgnl302j")
           ("runtime.debian.8-x64.runtime.native.system.security.cryptography.openssl"
            #:version "4.3.0"
            #:url "https://www.nuget.org/api/v2/package/runtime.debian.8-x64.runtime.native.system.security.cryptography.openssl/4.3.0"
            #:sha256 "16rnxzpk5dpbbl1x354yrlsbvwylrq456xzpsha1n9y3glnhyx9d")
           ("system.security.cryptography.x509certificates"
            #:version "4.3.0"
            #:url "https://www.nuget.org/api/v2/package/system.security.cryptography.x509certificates/4.3.0"
            #:sha256 "0valjcz5wksbvijylxijjxb1mp38mdhv03r533vnx1q3ikzdav9h")
           ("nuget.packaging"
            #:version "5.2.0-rtm.6067"
            #:url "https://dotnet.myget.org/F/nuget-build/api/v2/package/nuget.packaging/5.2.0-rtm.6067"
            #:sha256 "16p5glvvpp5rw10ycbpyg39k4prir450l12r5frpm8qz0rdp3xig")
           ("nuget.commands"
            #:version "5.2.0-rtm.6067"
            #:url "https://dotnet.myget.org/F/nuget-build/api/v2/package/nuget.commands/5.2.0-rtm.6067"
            #:sha256 "06vnphsmwnvcigwj37hy5abipjzwhnq61zw66cclwd6jjibb1kh9")
           ("system.runtime"
            #:version "4.1.0"
            #:url "https://www.nuget.org/api/v2/package/system.runtime/4.1.0"
            #:sha256 "02hdkgk13rvsd6r9yafbwzss8kr55wnj8d5c7xjnp8gqrwc8sn0m")
           ("system.runtime"
            #:version "4.3.0"
            #:url "https://www.nuget.org/api/v2/package/system.runtime/4.3.0"
            #:sha256 "066ixvgbf2c929kgknshcxqj6539ax7b9m570cp8n179cpfkapz7")
           ("microsoft.win32.primitives"
            #:version "4.3.0"
            #:url "https://www.nuget.org/api/v2/package/microsoft.win32.primitives/4.3.0"
            #:sha256 "0j0c1wj4ndj21zsgivsc24whiya605603kxrbiw6wkfdync464wq")
           ("microsoft.win32.primitives"
            #:version "4.0.1"
            #:url "https://www.nuget.org/api/v2/package/microsoft.win32.primitives/4.0.1"
            #:sha256 "1n8ap0cmljbqskxpf8fjzn7kh1vvlndsa75k01qig26mbw97k2q7")
           ("system.collections.immutable"
            #:version "1.2.0"
            #:url "https://www.nuget.org/api/v2/package/system.collections.immutable/1.2.0"
            #:sha256 "1jm4pc666yiy7af1mcf7766v710gp0h40p228ghj6bavx7xfa38m")
           ("system.collections.immutable"
            #:version "1.3.1"
            #:url "https://www.nuget.org/api/v2/package/system.collections.immutable/1.3.1"
            #:sha256 "17615br2x5riyx8ivf1dcqwj6q3ipq1bi5hqhw54yfyxmx38ddva")
           ("system.collections.immutable"
            #:version "1.5.0"
            #:url "https://www.nuget.org/api/v2/package/system.collections.immutable/1.5.0"
            #:sha256 "1d5gjn5afnrf461jlxzawcvihz195gayqpcfbv6dd7pxa9ialn06")
           ("nuget.dependencyresolver.core"
            #:version "5.2.0-rtm.6067"
            #:url "https://dotnet.myget.org/F/nuget-build/api/v2/package/nuget.dependencyresolver.core/5.2.0-rtm.6067"
            #:sha256 "0iw1z2lascjjmdkk9nf2wqm5sj5nqjv4611xx29vlmp6cyhnpq4i")
           ("netstandard.library"
            #:version "1.6.1"
            #:url "https://www.nuget.org/api/v2/package/netstandard.library/1.6.1"
            #:sha256 "1z70wvsx2d847a2cjfii7b83pjfs34q05gb037fdjikv5kbagml8")
           ("shouldly"
            #:version "3.0.0"
            #:url "https://www.nuget.org/api/v2/package/shouldly/3.0.0"
            #:sha256 "1hg28w898kl84rx57sclb2z9b76v5hxlwxig1xnb6fr81aahzlw3")
           ("microsoft.diasymreader.pdb2pdb"
            #:version "1.1.0-beta1-62506-02"
            #:url "https://dotnetfeed.blob.core.windows.net/dotnet-core/flatcontainer/microsoft.diasymreader.pdb2pdb/1.1.0-beta1-62506-02/microsoft.diasymreader.pdb2pdb.1.1.0-beta1-62506-02.nupkg"
            #:sha256 "1dkhpmq5aw34nndvb4xc370866vf33x70zrjhgvnpwwspb6vb0zh")
           ("system.globalization.calendars"
            #:version "4.3.0"
            #:url "https://www.nuget.org/api/v2/package/system.globalization.calendars/4.3.0"
            #:sha256 "1xwl230bkakzzkrggy1l1lxmm3xlhk4bq2pkv790j5lm8g887lxq")
           ("system.io.compression.zipfile"
            #:version "4.3.0"
            #:url "https://www.nuget.org/api/v2/package/system.io.compression.zipfile/4.3.0"
            #:sha256 "1yxy5pq4dnsm9hlkg9ysh5f6bf3fahqqb6p8668ndy5c0lk7w2ar")
           ("system.runtime.interopservices.runtimeinformation"
            #:version "4.0.0"
            #:url "https://www.nuget.org/api/v2/package/system.runtime.interopservices.runtimeinformation/4.0.0"
            #:sha256 "0glmvarf3jz5xh22iy3w9v3wyragcm4hfdr17v90vs7vcrm7fgp6")
           ("system.runtime.interopservices.runtimeinformation"
            #:version "4.3.0"
            #:url "https://www.nuget.org/api/v2/package/system.runtime.interopservices.runtimeinformation/4.3.0"
            #:sha256 "0q18r1sh4vn7bvqgd6dmqlw5v28flbpj349mkdish2vjyvmnb2ii")
           ("system.io.filesystem.driveinfo"
            #:version "4.3.0"
            #:url "https://www.nuget.org/api/v2/package/system.io.filesystem.driveinfo/4.3.0"
            #:sha256 "0j67khc75lwdf7d5i3z41cks7zhac4zdccgvk2xmq6wm1l08xnlh")
           ("system.threading.tasks.extensions"
            #:version "4.3.0"
            #:url "https://www.nuget.org/api/v2/package/system.threading.tasks.extensions/4.3.0"
            #:sha256 "1xxcx2xh8jin360yjwm4x4cf5y3a2bwpn2ygkfkwkicz7zk50s2z")
           ("system.threading.tasks.extensions"
            #:version "4.0.0"
            #:url "https://www.nuget.org/api/v2/package/system.threading.tasks.extensions/4.0.0"
            #:sha256 "1cb51z062mvc2i8blpzmpn9d9mm4y307xrwi65di8ri18cz5r1zr")
           ("microsoft.netcore.targets"
            #:version "1.0.1"
            #:url "https://www.nuget.org/api/v2/package/microsoft.netcore.targets/1.0.1"
            #:sha256 "0ppdkwy6s9p7x9jix3v4402wb171cdiibq7js7i13nxpdky7074p")
           ("microsoft.netcore.targets"
            #:version "1.1.0"
            #:url "https://www.nuget.org/api/v2/package/microsoft.netcore.targets/1.1.0"
            #:sha256 "193xwf33fbm0ni3idxzbr5fdq3i2dlfgihsac9jj7whj0gd902nh")
           ("system.reflection.extensions"
            #:version "4.0.1"
            #:url "https://www.nuget.org/api/v2/package/system.reflection.extensions/4.0.1"
            #:sha256 "0m7wqwq0zqq9gbpiqvgk3sr92cbrw7cp3xn53xvw7zj6rz6fdirn")
           ("system.reflection.extensions"
            #:version "4.3.0"
            #:url "https://www.nuget.org/api/v2/package/system.reflection.extensions/4.3.0"
            #:sha256 "02bly8bdc98gs22lqsfx9xicblszr2yan7v2mmw3g7hy6miq5hwq")
           ("system.diagnostics.process"
            #:version "4.1.0"
            #:url "https://www.nuget.org/api/v2/package/system.diagnostics.process/4.1.0"
            #:sha256 "061lrcs7xribrmq7kab908lww6kn2xn1w3rdc41q189y0jibl19s")
           ("system.diagnostics.process"
            #:version "4.3.0"
            #:url "https://www.nuget.org/api/v2/package/system.diagnostics.process/4.3.0"
            #:sha256 "0g4prsbkygq8m21naqmcp70f24a1ksyix3dihb1r1f71lpi3cfj7")
           ("system.security.cryptography.primitives"
            #:version "4.3.0"
            #:url "https://www.nuget.org/api/v2/package/system.security.cryptography.primitives/4.3.0"
            #:sha256 "0pyzncsv48zwly3lw4f2dayqswcfvdwq2nz0dgwmi7fj3pn64wby")
           ("system.threading.thread"
            #:version "4.0.0"
            #:url "https://www.nuget.org/api/v2/package/system.threading.thread/4.0.0"
            #:sha256 "1gxxm5fl36pjjpnx1k688dcw8m9l7nmf802nxis6swdaw8k54jzc")
           ("system.threading.thread"
            #:version "4.3.0"
            #:url "https://www.nuget.org/api/v2/package/system.threading.thread/4.3.0"
            #:sha256 "0y2xiwdfcph7znm2ysxanrhbqqss6a3shi1z3c779pj2s523mjx4")
           ("newtonsoft.json"
            #:version "9.0.1"
            #:url "https://www.nuget.org/api/v2/package/newtonsoft.json/9.0.1"
            #:sha256 "0mcy0i7pnfpqm4pcaiyzzji4g0c8i3a5gjz28rrr28110np8304r")
           ("runtime.rhel.7-x64.runtime.native.system.security.cryptography.openssl"
            #:version "4.3.0"
            #:url "https://www.nuget.org/api/v2/package/runtime.rhel.7-x64.runtime.native.system.security.cryptography.openssl/4.3.0"
            #:sha256 "0vhynn79ih7hw7cwjazn87rm9z9fj0rvxgzlab36jybgcpcgphsn")
           ("xunit"
            #:version "2.4.1"
            #:url "https://www.nuget.org/api/v2/package/xunit/2.4.1"
            #:sha256 "0xf3kaywpg15flqaqfgywqyychzk15kz0kz34j21rcv78q9ywq20")
           ("system.valuetuple"
            #:version "4.3.0"
            #:url "https://www.nuget.org/api/v2/package/system.valuetuple/4.3.0"
            #:sha256 "1227k7fxbxapq7dms4lvwwjdf3pr1jcsmhy2nzzhj6g6hs530hxn")
           ("microsoft.netframework.referenceassemblies.net472"
            #:version "1.0.0-alpha-004"
            #:url "https://dotnet.myget.org/F/roslyn-tools/api/v2/package/microsoft.netframework.referenceassemblies.net472/1.0.0-alpha-004"
            #:sha256 "08wa54dm7yskayzxivnwbm8sg1pf6ai8ccr64ixf9lyz3yw6y0nc")
           ("system.security.cryptography.algorithms"
            #:version "4.3.0"
            #:url "https://www.nuget.org/api/v2/package/system.security.cryptography.algorithms/4.3.0"
            #:sha256 "03sq183pfl5kp7gkvq77myv7kbpdnq3y0xj7vi4q1kaw54sny0ml"))))
    (package
      (name "msbuild")
      (version "16.3")
      (source
       (origin
         (method url-fetch)
         (uri (string-append
               "https://download.mono-project.com/sources/msbuild/msbuild-"
               version "+xamarinxplat." date ".tar.xz")) ;
         ;; (file-name (git-file-name name version))
         (sha256
          (base32
           "1zcdfx4xsh62wj3g1jc2an0lppsfs691lz4dv05xbgi01aq1hk6a"))))
      (build-system gnu-build-system)
      (inputs
       `(("mono" ,mono-6)))
      (native-inputs
       `(("dotnet-sdk" ,dotnet-sdk)
         ("nuget" ,nuget)
         ("unzip" ,unzip)
         ("xplat" ,(origin
                     (method url-fetch)
                     ;; TODO: Update?  Better: build from source.
                     (uri (string-append "https://github.com/mono/msbuild/releases/download/0.07/mono_msbuild_xplat-master-8f608e49.zip"))
                     (sha256
                      (base32
                       "1jxq3fk9a6q2a8i9zacxaz3fkvc22i9qvzlpa7wbb95h42g0ffhq"))))
         ,@(map (lambda (input)
                  (list (car input)
                        (apply nuget-fetch (cdr input))))
                mono-inputs)))
      (arguments
       `(#:modules ((srfi srfi-1)
                    ,@%gnu-build-system-modules)
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (replace 'build
             (lambda* (#:key inputs #:allow-other-keys)
               (let* ((unzip (string-append (assoc-ref inputs "unzip") "/bin/unzip"))
                      (xplat (assoc-ref inputs "xplat"))
                      (nuget (string-append (assoc-ref inputs "nuget") "/bin/nuget"))
                      (dotnet-sdk (assoc-ref inputs "dotnet-sdk"))
                      (libhostfxr (string-append dotnet-sdk "/host/fxr/3.1.1/libhostfxr.so"))
                      (microsoft.dotnet.arcade.sdk (assoc-ref inputs "microsoft.dotnet.arcade.sdk")))
                 ;; Set up nuget packages.
                 (setenv "HOME" (string-append (getcwd) "/fake-home"))
                 (pk 'HERE)
                 (for-each
                  (lambda (mono-dep)
                    (invoke nuget "add" (assoc-ref inputs (car mono-dep)) "-Source" "guix"))
                  ',mono-inputs)
                 (invoke nuget "sources" "Disable" "-Name" "nuget.org")
                 (invoke nuget "sources" "Add" "-Name" "guix" "-Source" (string-append (getcwd) "/guix"))
                 ;; Extract bootstrap.
                 (mkdir-p "artifacts")
                 (invoke unzip xplat "-d" "artifacts")
                 (rename-file ;; (string-append "artifacts/msbuild-" ,bootstrap-version)
                  "artifacts/msbuild" "artifacts/mono-msbuild")
                 (chmod "artifacts/mono-msbuild/MSBuild.dll" 493)
                 (symlink libhostfxr
                          (string-append
                           "artifacts/mono-msbuild/SdkResolvers/Microsoft.DotNet.MSBuildSdkResolver/"
                           (basename libhostfxr)))
                 ;; Prevent installer from running.
                 (with-output-to-file "eng/common/dotnet-install.sh"
                   (lambda _
                     (format #t "#!~a~%" (which "bash"))))
                 ;; msbuild response files to use only our source
                 (with-output-to-file "artifacts/mono-msbuild/MSBuild.rsp"
                   (lambda _
                     (display "/p:RestoreSources=guix\n")))
                 (with-output-to-file "src/MSBuild/MSBuild.rsp"
                   (lambda _
                     (display "/p:RestoreSources=guix\n")))

                 (substitute* "./eng/cibuild_bootstrapped_msbuild.sh"
                   (("\t/bin/bash") (string-append "\t" (which "bash"))))
                 (pk 'BUILDING)
                 (invoke "./eng/cibuild_bootstrapped_msbuild.sh"
                         "--host_type" "mono"
                         "--configuration" "Release"
                         ;; TODO there are some (many?) failing tests
                         "--skip_tests" "/p:DisableNerdbankVersioning=true")))))))
      (supported-systems '("x86_64-linux"))
      (home-page "https://github.com/mono/msbuild")
      (synopsis "Implementation of the Microsoft build system for Mono")
      (description "This is the Mono version of Microsoft Build Engine, the
build platform for .NET, and Visual Studio.")
      (license license:expat))))

;; (define-public msbuild
;;   (let ((date "2020.01.10.05.36"))
;;     (package
;;       (name "msbuild")
;;       (version "16.5")
;;       (source
;;        (origin
;;          (method git-fetch)
;;          (uri (git-reference
;;                (url "https://github.com/mono/linux-packaging-msbuild")
;;                (commit (string-append "upstream/" version "+xamarinxplat." date))))
;;          (file-name (git-file-name name version))
;;          (sha256
;;           (base32
;;            "1knircr25qxvmm5v3mar3xfvcycrbn4l03kyhfx8jmyzjxf5slcg"))))
;;       (build-system gnu-build-system)
;;       (inputs
;;        `(("mono" ,mono)))
;;       (native-inputs
;;        `(("dotnet-sdk" ,dotnet-sdk)
;;          ("unzip" ,unzip)))
;;       (arguments
;;        `(#:phases
;;          (modify-phases %standard-phases
;;            (delete 'configure)
;;            (replace 'build
;;              (lambda _
;;                ;; TODO there are some (many?) failing tests
;;                (invoke "./eng/cibuild_bootstrapped_msbuild.sh"
;;                        "--host_type" "mono"
;;                        "--configuration" "Release"
;;                        "--skip_tests" "/p:DisableNerdbankVersioning=true"))))))
;;       (supported-systems '("x86_64-linux"))
;;       (home-page "https://github.com/mono/msbuild;")
;;       (synopsis "Implementation of the Microsoft build system for Mono")
;;       (description "This is the Mono version of Microsoft Build Engine, the
;; build platform for .NET, and Visual Studio.")
;;       (license license:expat))))

;; (define-public msbuild
;;   (let ((date "2020.01.10.05.36")
;;         (bootstrap-version "0.08"))
;;     (package
;;       (name "msbuild")
;;       (version "16.5")
;;       (source
;;        (origin
;;          (method url-fetch)
;;          (uri (string-append
;;                "https://download.mono-project.com/sources/msbuild/msbuild-"
;;                version "+xamarinxplat." date ".tar.xz")) ;
;;          ;; (file-name (git-file-name name version))
;;          (sha256
;;           (base32
;;            "19ki9ch4wxkh0l9jnsjr5fc7zx6lmpkmdklk3wxwsgmdakn3jws8"
;;            ;; "1knircr25qxvmm5v3mar3xfvcycrbn4l03kyhfx8jmyzjxf5slcg"
;;            ))))
;;       (build-system gnu-build-system)
;;       (inputs
;;        `(("mono" ,mono)))
;;       (native-inputs
;;        `(("dotnet-sdk" ,dotnet-sdk)
;;          ("unzip" ,unzip)
;;          ("xplat" ,(origin
;;                      (method url-fetch)
;;                      ;; (uri "https://github.com/mono/msbuild/releases/download/0.07/mono_msbuild_xplat-master-8f608e49.zip")
;;                      (uri (string-append "https://github.com/mono/msbuild/releases/download/"
;;                                          bootstrap-version "/mono_msbuild_6.4.0.208.zip"))
;;                      ;; (file-name "mono_msbuild_6.4.0.208.zip")
;;                      (sha256
;;                       (base32
;;                        "0w9x1sj8nrsfghf7qm7dbcbc4yxhnzyhsrpl5slblfiyrwddm3xk"
;;                        ;; "1jxq3fk9a6q2a8i9zacxaz3fkvc22i9qvzlpa7wbb95h42g0ffhq"
;;                        ))))))
;;       (arguments
;;        `(#:phases
;;          (modify-phases %standard-phases
;;            (delete 'configure)
;;            (replace 'build
;;              (lambda* (#:key inputs #:allow-other-keys)
;;                (let* ((unzip (string-append (assoc-ref inputs "unzip") "/bin/unzip"))
;;                       (xplat (assoc-ref inputs "xplat"))
;;                       (dotnet-sdk (assoc-ref inputs "dotnet-sdk"))
;;                       (libhostfxr (string-append dotnet-sdk "/host/fxr/3.1.1/libhostfxr.so")))
;;                  (mkdir-p "artifacts")
;;                  (invoke unzip xplat "-d" "artifacts")
;;                  (rename-file ;; (string-append "artifacts/msbuild-" ,bootstrap-version)
;;                   "artifacts/msbuild" "artifacts/mono-msbuild")
;;                  (chmod "artifacts/mono-msbuild/MSBuild.dll" #o755)
;;                  (symlink libhostfxr
;;                           (string-append
;;                            "artifacts/mono-msbuild/SdkResolvers/Microsoft.DotNet.MSBuildSdkResolver/"
;;                            (basename libhostfxr)))
;;                  ;; Prevent installer from running.
;;                  (with-output-to-file "eng/common/dotnet-install.sh"
;;                    (lambda _
;;                      (format #t "#!~a/bin/bash~%" (which "bash"))))
;;                  ;; msbuild response files to use only our source
;;                  (with-output-to-file "artifacts/mono-msbuild/MSBuild.rsp"
;;                    (lambda _
;;                      (display "/p:RestoreSources=guix\n")))
;;                  (with-output-to-file "src/MSBuild/MSBuild.rsp"
;;                    (lambda _
;;                      (display "/p:RestoreSources=guix\n")))

;;                  (substitute* "./eng/cibuild_bootstrapped_msbuild.sh"
;;                    (("\t/bin/bash") (string-append "\t" (which "bash"))))
;;                  (pk 'BUILDING)
;;                  (invoke "./eng/cibuild_bootstrapped_msbuild.sh"
;;                          "--host_type" "mono"
;;                          "--configuration" "Release"
;;                          ;; TODO there are some (many?) failing tests
;;                          "--skip_tests" "/p:DisableNerdbankVersioning=true")))))))
;;       (supported-systems '("x86_64-linux"))
;;       (home-page "https://github.com/mono/msbuild;")
;;       (synopsis "Implementation of the Microsoft build system for Mono")
;;       (description "This is the Mono version of Microsoft Build Engine, the
;; build platform for .NET, and Visual Studio.")
;;       (license license:expat))))
