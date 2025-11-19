;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2022-2023 Pierre Langlois <pierre.langlois@gmx.com>

(define-module (nongnu packages wasm)
  #:use-module (guix base32)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages python))

(define-public wasi-libc
  (package
    (name "wasi-libc")
    (version "sdk-19")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/WebAssembly/wasi-libc")
                    (commit (string-append "wasi-" version))
                    (recursive? #t)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0bnpz8wk9wiic938296gxp4vz820bvpi1w41jksjzz5552hql169"))))
    (build-system gnu-build-system)
    (native-inputs (list clang-15))
    (arguments
     (list #:tests? #f ;No test suite
           ;; Firefox uses wasm2c to compile WebAssembly to C code, and it
           ;; does not support the memory.copy opcode.
           ;; See https://bugzilla.mozilla.org/show_bug.cgi?id=1773200#c4
           #:make-flags ''("BULK_MEMORY_SOURCES=")
           #:phases
           #~(modify-phases %standard-phases
               (delete 'configure)
               (add-before 'build 'set-sysroot-include
                 (lambda _
                   (setenv "C_INCLUDE_PATH"
                           (string-append (getcwd) "/sysroot/include"))))
               (add-before 'install 'set-install-dir
                 (lambda _
                   (setenv "INSTALL_DIR"
                           (string-append #$output "/wasm32-wasi")))))))
    (home-page "https://wasi.dev")
    (synopsis "WASI libc implementation for WebAssembly")
    (description
     "WASI Libc is a libc for WebAssembly programs built on top of WASI
system calls.  It provides a wide array of POSIX-compatible C APIs, including
support for standard I/O, file I/O, filesystem manipulation, memory
management, time, string, environment variables, program startup, and many
other APIs.")
    (license (list
              ;; For wasi-libc, with LLVM exceptions
              license:asl2.0
              ;; For malloc.c.
              license:cc0
              ;; For cloudlibc.
              license:bsd-2
              ;; For wasi-libc and musl-libc.
              license:expat))))

(define-public wasm32-wasi-clang-runtime
  (package (inherit clang-runtime-17)
    (native-inputs
     (list clang-17
           wasi-libc))
    (inputs (list llvm-17))
    (arguments
     (list
      #:build-type "Release"
      #:tests? #f
      ;; Stripping binaries breaks wasm linking, resulting in the following
      ;; error: "archive has no index; run ranlib to add one".
      #:strip-binaries? #f
      #:configure-flags
      #~(list "-DCMAKE_C_COMPILER=clang"
              "-DCMAKE_C_COMPILER_TARGET=wasm32-wasi"
              (string-append
               "-DCMAKE_SYSROOT=" #$wasi-libc "/wasm32-wasi")
              (string-append
               "-DCMAKE_C_FLAGS=-I " #$wasi-libc "/wasm32-wasi/include")

              ;; https://github.com/llvm/llvm-project/issues/63799
              "-DCMAKE_SYSTEM_NAME=Generic"

              "-DCOMPILER_RT_OS_DIR=wasi"

              "-DCOMPILER_RT_BAREMETAL_BUILD=On"
              "-DCOMPILER_RT_DEFAULT_TARGET_ONLY=On"

              ;; WASM only needs libclang_rt.builtins-wasm32.a from
              ;; compiler-rt.
              "../source/compiler-rt/lib/builtins")))))

;; FIXME: Ideally we wouldn't need to build a separate compiler because clang
;; can support multiple targets at runtime.  However Guix patches the default
;; clang with a specific clang-runtime package.  It would be good to improve
;; upstream Guix's support for cross-compiling with clang.

(define clang-from-llvm (@@ (gnu packages llvm) clang-from-llvm))
(define llvm-monorepo (@@ (gnu packages llvm) llvm-monorepo))

(define-public wasm32-wasi-clang
  (let ((base
         (clang-from-llvm llvm-17 wasm32-wasi-clang-runtime
                          #:patches '("clang-17.0-fix-build-with-gcc-14-on-arm.patch"))))
    (package (inherit base)
      (name "wasm32-wasi-clang")
      (inputs
       (modify-inputs (package-inputs base)
         (prepend wasi-libc)))
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ((#:configure-flags flags)
          #~(list "-DCLANG_INCLUDE_TESTS=True"
                  ;; Use a sane default include directory.
                  (string-append "-DC_INCLUDE_DIRS="
                                 #$wasi-libc
                                 "/wasm32-wasi/include")))
         ((#:phases phases)
          `(modify-phases ,phases
             (delete 'symlink-cfi_ignorelist))))))))

(define-public wasm32-wasi-libcxx
  (package
    (name "wasm32-wasi-libcxx")
    (version (package-version llvm-17))
    (source (llvm-monorepo version))
    (build-system cmake-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list (string-append "-S ../source/runtimes")

              "-DLLVM_ENABLE_RUNTIMES=libcxx;libcxxabi"

              (string-append
               "-DCMAKE_SYSROOT=" #$wasi-libc "/wasm32-wasi")

              (string-append
               "-DCMAKE_INCLUDE_PATH=" #$wasi-libc "/wasm32-wasi/include")

              (string-append
               "-DCMAKE_STAGING_PREFIX=" #$output "/wasm32-wasi")

              "-DCMAKE_C_COMPILER=clang"
              "-DCMAKE_C_COMPILER_WORKS=ON"
              "-DCMAKE_CXX_COMPILER=clang++"
              "-DCMAKE_CXX_COMPILER_WORKS=ON"
              "-DCMAKE_C_COMPILER_TARGET=wasm32-wasi"
              "-DCMAKE_CXX_COMPILER_TARGET=wasm32-wasi"

              "-DLIBCXX_LIBDIR_SUFFIX=/wasm32-wasi"

              ;; https://reviews.llvm.org/D151740
              "-DLIBCXX_HAS_MUSL_LIBC:BOOL=ON"

              "-DLIBCXX_ENABLE_EXCEPTIONS=OFF"
              "-DLIBCXX_ENABLE_SHARED=OFF"
              "-DLIBCXX_ENABLE_THREADS=OFF"
              "-DLIBCXX_ENABLE_FILESYSTEM=OFF"

              "-DLIBCXXABI_LIBDIR_SUFFIX=/wasm32-wasi"

              "-DLIBCXXABI_ENABLE_EXCEPTIONS=OFF"
              "-DLIBCXXABI_ENABLE_SHARED=OFF"
              "-DLIBCXXABI_ENABLE_THREADS=OFF"
              "-DLIBCXXABI_ENABLE_FILESYSTEM=OFF")
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'set-paths 'adjust-CPLUS_INCLUDE_PATH
            (lambda _
              (setenv "CPLUS_INCLUDE_PATH"
                      (string-append #$wasi-libc "/wasm32-wasi/include:"
                                     (getenv "CPLUS_INCLUDE_PATH"))))))))
    (native-inputs
     (list lld
           python
           wasm32-wasi-clang))
    (inputs
     (list wasi-libc))
    (home-page "https://libcxx.llvm.org")
    (synopsis "C++ standard library for WebAssembly")
    (description
     "This package provides an implementation of the C++ standard library for
use with Clang, targeting C++11, C++14 and above.  This package targets
WebAssembly with WASI.")
    (license license:expat)))

(define-public wasm32-wasi-clang-toolchain
  (package
    (name "wasm32-wasi-clang-toolchain")
    (version (package-version wasm32-wasi-clang))
    (source #f)
    (build-system trivial-build-system)
    (arguments
     (list
      #:builder
      (with-imported-modules '((guix build union)
                               (guix build utils))
        #~(begin
            (use-modules (guix build union)
                         (guix build utils))
            (union-build #$output
                         (list #$wasm32-wasi-clang-runtime
                               #$wasi-libc
                               #$wasm32-wasi-libcxx))
            (mkdir-p (string-append #$output "/bin"))

            ;; We provide clang and clang++ via a wrapped program that sets
            ;; include paths correctly so that it does not include paths from
            ;; the host.

            ;; FIXME: Review how we can provide better support for
            ;; cross-compiling with clang in Guix, maybe adding support for
            ;; the CROSS_C_INCLUDE_PATH and CROSS_CPLUS_INCLUDE_PATH
            ;; environment variables like GCC.

            (for-each
             (lambda (bin)
               (symlink (string-append #$wasm32-wasi-clang bin)
                        (string-append #$output bin))
               (wrap-program (string-append #$output bin)
                 #:sh (string-append #$bash-minimal "/bin/bash")
                 `("C_INCLUDE_PATH" ":" =
                   (,(string-append #$output "/wasm32-wasi/include")))
                 `("CPLUS_INCLUDE_PATH" ":" =
                   ;; Make sure inclure/c++/v1 comes first for #include_next
                   ;; to work.
                   (,(string-append #$output "/wasm32-wasi/include/c++/v1")
                    ,(string-append #$output "/wasm32-wasi/include")))))
             '("/bin/clang" "/bin/clang++"))

            (symlink (string-append #$lld "/bin/wasm-ld")
                     (string-append #$output "/bin/wasm-ld"))))))
    (inputs
     (list bash-minimal
           lld
           wasi-libc
           wasm32-wasi-clang
           wasm32-wasi-clang-runtime
           wasm32-wasi-libcxx))
    (license (cons
              (package-license wasm32-wasi-clang)
              (package-license wasi-libc)))
    (home-page "https://clang.llvm.org")
    (synopsis "Complete Clang toolchain for C/C++ development, for WebAssembly.")
    (description "This package provides a complete Clang toolchain for C/C++
development targeting WebAssembly with WASI.  This includes Clang, as well as
libc, libc++ and wasm-ld.")))
