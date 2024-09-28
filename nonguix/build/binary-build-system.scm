;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2019 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2022 Attila Lendvai <attila@lendvai.name>
;;; Copyright © 2023 Giacomo Leidi <goodoldpaul@autistici.org>
;;; Copyright © 2024 Ashish SHUKLA <ashish.is@lostca.se>

(define-module (nonguix build binary-build-system)
  #:use-module ((guix build gnu-build-system) #:prefix gnu:)
  #:use-module (nonguix build utils)
  #:use-module (guix build utils)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (%standard-phases
            binary-build))

;; Commentary:
;;
;; Builder-side code of the standard binary build procedure.
;;
;; Code:

(define (new-install)
  "Return the copy-build-system `install' procedure."
  (@@ (guix build copy-build-system) install))

(define* (old-install #:key install-plan outputs #:allow-other-keys)
  "Copy files from the \"source\" build input to the \"out\" output according to INSTALL-PLAN.

An INSTALL-PLAN is made of three elements:

- A source path which is a file or directory from the \"source\" build input.
- Patterns of the files to copy (only useful if the source path is a directory).
- The target destination.

If the target ends with a slash, it represents the target directory.  If not, it
represent the target full path, which only makes sense for single files."
  (define (install-file file target)
    (let ((target (string-append (assoc-ref outputs "out")
                                 "/" target
                                 (if (string-suffix? "/" target)
                                     (string-append "/" file)
                                     ""))))
      (mkdir-p (dirname target))
      (copy-file file target)))

  (define (install-file-pattern pattern target)
    (for-each
      (lambda (file)
        (install-file file target))
      (find-files "." pattern)))

  (define (install plan)
    (match plan
      ((file-or-directory files target)
       (if (file-is-directory? file-or-directory)
           (with-directory-excursion file-or-directory
             (for-each
              (lambda (pattern)
                (install-file-pattern pattern target))
              files))
           (install-file file-or-directory target)))))

  (for-each install install-plan)
  #t)

(define* (install #:key install-plan outputs #:allow-other-keys)
  (define (install-old-format)
     (warn "Install-plan format deprecated.
Please update to the format of the copy-build-system.")
     (old-install #:install-plan install-plan #:outputs outputs))
  (match (car install-plan)
    ((source (. matches) target)
     (install-old-format))
    ((source #f target)
     (install-old-format))
    (_ ((new-install) #:install-plan install-plan #:outputs outputs))))

(define* (patchelf #:key inputs outputs patchelf-plan #:allow-other-keys)
  "Set the interpreter and the RPATH of files as per the PATCHELF-PLAN.

The PATCHELF-PLAN elements are lists of:

- The file to patch.
- The inputs (as strings) to include in the rpath, e.g. \"mesa\".

Both executables and dynamic libraries are accepted.
The inputs are optional when the file is an executable."
  (define (binary-patch binary interpreter runpath)

    (define* (maybe-make-rpath entries name #:optional (extra-path "/lib"))
      (let ((entry (assoc-ref entries name)))
        (if entry
            (string-append entry extra-path)
            #f)))

    (define* (make-rpath name #:optional (extra-path "/lib"))
      (or (maybe-make-rpath outputs name extra-path)
          (maybe-make-rpath inputs  name extra-path)
          (error (format #f "`~a' not found among the inputs nor the outputs."
                         name))))

    (unless (string-contains binary ".so")
      ;; Use `system*' and not `invoke' since this may raise an error if
      ;; library does not end with .so.
      (system* "patchelf" "--set-interpreter" interpreter binary))
    (when runpath
      (let ((rpath (string-join
                    (map
                     (match-lambda
                       ((name extra-path)
                        (make-rpath name extra-path))
                       (name
                        (make-rpath name)))
                     runpath)
                    ":")))
        (invoke "patchelf" "--set-rpath" rpath binary)))
    #t)

  (display "Using patchelf version: ")
  (force-output)
  (invoke "patchelf" "--version")

  (when (and patchelf-plan
             (not (null? patchelf-plan)))
    (let ((interpreter (car (find-files (assoc-ref inputs "libc") "ld-linux.*\\.so")))
          (interpreter32 (car (find-files (assoc-ref inputs "libc32") "ld-linux.*\\.so"))))
      (for-each
       (lambda (plan)
         (match plan
           ((binary runpath)
            (binary-patch binary (if (64-bit? binary)
                                     interpreter
                                     interpreter32)
                          runpath))
           ((binary)
            (binary-patch binary (if (64-bit? binary)
                                     interpreter
                                     interpreter32)
                          #f))))
       patchelf-plan)))
  #t)

(define (deb-file? binary-file)
  (string-suffix? ".deb" binary-file))

(define (unpack-deb deb-file)
  (invoke "ar" "x" deb-file)
  (let ((data-file (find file-exists?
                         (list "data.tar.xz" "data.tar.gz"
                               "data.tar.bz2"))))
    (invoke "tar" "xvf" data-file)
    (invoke "rm" "-rfv" "control.tar.gz"
            data-file
            deb-file
            "debian-binary")))

(define* (binary-unpack #:key source #:allow-other-keys)
  (let* ((files (filter (lambda (f)
                          (not (string=? (basename f) "environment-variables")))
                        (find-files (getcwd))))
         (binary-file (car files)))
    (when (= 1 (length files))
      (mkdir "binary")
      (chdir "binary")
      (match binary-file
        ((? deb-file?) (unpack-deb binary-file))
        (_
         (begin
           (format #t "Unknown file type: ~a~%" (basename binary-file))
           ;; Cleanup after ourselves
           (chdir "..")
           (rmdir "binary")))))))

(define %standard-phases
  ;; Everything is as with the GNU Build System except for the `binary-unpack',
  ;; `configure', `build', `check' and `install' phases.
  (modify-phases gnu:%standard-phases
    (add-after 'unpack 'binary-unpack binary-unpack)
    (delete 'bootstrap)
    (delete 'configure)
    (delete 'build)
    (delete 'check)
    (add-before 'install 'patchelf patchelf)
    (replace 'install install)))

(define* (binary-build #:key inputs (phases %standard-phases)
                       #:allow-other-keys #:rest args)
  "Build the given package, applying all of PHASES in order."
  (apply gnu:gnu-build #:inputs inputs #:phases phases args))

;;; binary-build-system.scm ends here
