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
        )
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
                     ;; (uri "https://github.com/mono/msbuild/releases/download/0.07/mono_msbuild_xplat-master-8f608e49.zip")
                     (uri (string-append "https://github.com/mono/msbuild/releases/download/0.07/mono_msbuild_xplat-master-8f608e49.zip"))
                     ;; (file-name "mono_msbuild_6.4.0.208.zip")
                     (sha256
                      (base32
                       "1jxq3fk9a6q2a8i9zacxaz3fkvc22i9qvzlpa7wbb95h42g0ffhq"))))
         ;; ("microsoft.build"
         ;;  ,(nuget-fetch #:version "14.3.0"
         ;;                #:url "https://www.nuget.org/api/v2/package/microsoft.build/14.3.0"
         ;;                #:sha256 "1zamn3p8xxi0wsjlpln0y71ncb977f3fp08mvaz4wmbmi76nr0rz"))
         ("microsoft.dotnet.arcade.sdk"
          ,(nuget-fetch #:version "1.0.0-beta.19372.10"
                        #:url (string-append "https://dotnetfeed.blob.core.windows.net/dotnet-core/"
                                             "flatcontainer/microsoft.dotnet.arcade.sdk/1.0.0-beta.19372.10/"
                                             "microsoft.dotnet.arcade.sdk.1.0.0-beta.19372.10.nupkg")
                        #:sha256 "1lii0yg4fbsma80mmvw2zwplc26abb46q6gkxwbsbkyszkw128hv"))
         ("sn"
          ,(nuget-fetch #:version "1.0.0"
                        #:url (string-append "https://dotnetfeed.blob.core.windows.net/dotnet-core/"
                                             "flatcontainer/sn/1.0.0/sn.1.0.0.nupkg")
                        #:sha256 "1012fcdc6vq2355v86h434s6p2nnqgpdapb7p25l4h39g5q8p1qs"))
         ("microbuild.core"
          ,(nuget-fetch #:name "microbuild.core"
                        #:version "0.2.0"
                        #:url "https://www.nuget.org/api/v2/package/microbuild.core/0.2.0"
                        #:sha256 "0q4s45jskbyxfx4ay6znnvv94zma2wd85b8rwmwszd2nb0xl3194"))
         ("microbuild.core.sentinel"
          ,(nuget-fetch #:version "1.0.0"
                        #:url (string-append
                               "https://dotnetfeed.blob.core.windows.net/"
                               "dotnet-core/flatcontainer/microbuild.core.sentinel/"
                               "1.0.0/microbuild.core.sentinel.1.0.0.nupkg")
                        #:sha256 "035kqx5fkapql108n222lz8psvxk04mv3dy1qg3h08i4b8j3dy8i"))
         ("vswhere"
          ,(nuget-fetch #:name "vswhere"
                        #:version "2.6.7"
                        #:url "https://www.nuget.org/api/v2/package/vswhere/2.6.7"
                        #:sha256 "0h4k5i96p7633zzf4xsv7615f9x72rr5qr7b9934ri2y6gshfcwk"))
         ("microsoft.diasymreader.pdb2pdb"
          ,(nuget-fetch #:version "1.1.0-beta1-62506-02"
                        #:url (string-append
                               "https://dotnetfeed.blob.core.windows.net/dotnet-core/"
                               "flatcontainer/microsoft.diasymreader.pdb2pdb/"
                               "1.1.0-beta1-62506-02/"
                               "microsoft.diasymreader.pdb2pdb.1.1.0-beta1-62506-02.nupkg")
                        #:sha256 "1dkhpmq5aw34nndvb4xc370866vf33x70zrjhgvnpwwspb6vb0zh"))
         ("microsoft.dotnet.signtool"
          ,(nuget-fetch #:version "1.0.0-beta.19372.10"
                        #:url (string-append
                               "https://dotnetfeed.blob.core.windows.net/dotnet-core/"
                               "flatcontainer/microsoft.dotnet.signtool/"
                               "1.0.0-beta.19372.10/"
                               "microsoft.dotnet.signtool.1.0.0-beta.19372.10.nupkg")
                        #:sha256 "1f2im2lilw10zslfclxh49knr542jy7q09p009flxsgn68riy0j6"))))
      (arguments
       `(#:phases
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
                 (for-each
                  (lambda (mono-dep)
                    (invoke nuget "add" (assoc-ref inputs mono-dep)  "-Source" "guix"))
                  '("microsoft.dotnet.arcade.sdk"
                    "sn"
                    "microbuild.core"
                    "microbuild.core.sentinel"
                    "vswhere"
                    "microsoft.diasymreader.pdb2pdb"
                    "microsoft.dotnet.signtool"))
                 (invoke nuget "sources" "Disable" "-Name" "nuget.org")
                 (invoke nuget "sources" "Add" "-Name" "guix" "-Source" (string-append (getcwd) "/guix"))
                 ;; Extract bootstrap.
                 (mkdir-p "artifacts")
                 (invoke unzip xplat "-d" "artifacts")
                 (rename-file ;; (string-append "artifacts/msbuild-" ,bootstrap-version)
                  "artifacts/msbuild" "artifacts/mono-msbuild")
                 (chmod "artifacts/mono-msbuild/MSBuild.dll" #o755)
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
