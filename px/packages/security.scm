;;; Package Repository for GNU Guix
;;; Copyright © 2026 Franz Geffke <mail@gofranz.com>

(define-module (px packages security)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages file)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages admin)
  #:use-module (px packages go))

(define-public chkrootkit
  (package
    (name "chkrootkit")
    (version "0.59")
    (source
     (origin
       (method url-fetch)
       (uri "ftp://ftp.chkrootkit.org/pub/seg/pac/chkrootkit.tar.gz")
       (sha256
        (base32 "1vn5r35iai62v9vp7x8x23fdgh7pinw6pbf7v1nsm8j3ypbz2f5x"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f
      #:make-flags
      #~(list (string-append "CC=" #$(cc-for-target))
              (string-append "CFLAGS=-DHAVE_LASTLOG_H"))
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (add-after 'unpack 'patch-paths
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let ((libexec (string-append (assoc-ref outputs "out")
                                            "/libexec/chkrootkit")))
                ;; Patch shebang
                (substitute* "chkrootkit"
                  (("#!/bin/sh")
                   (string-append "#!" (search-input-file inputs "bin/sh"))))
                ;; Replace ./helper-binaries with absolute paths
                (substitute* "chkrootkit"
                  (("\\./strings-static")
                   (string-append libexec "/strings-static"))
                  (("\\./ifpromisc")
                   (string-append libexec "/ifpromisc"))
                  (("\\./chkutmp")
                   (string-append libexec "/chkutmp"))
                  (("\\./chklastlog")
                   (string-append libexec "/chklastlog"))
                  (("\\./chkwtmp")
                   (string-append libexec "/chkwtmp"))
                  (("\\./chkproc")
                   (string-append libexec "/chkproc"))
                  (("\\./chkdirs")
                   (string-append libexec "/chkdirs")))
                ;; Remove hardcoded FHS paths that don't exist on Guix
                (substitute* "chkrootkit"
                  (("pth=\"\\$pth /sbin /usr/sbin /lib /usr/lib /usr/libexec \\.\"")
                   "pth=\"$pth\""))
                ;; Fix grep warning about unnecessary backslash before /
                (substitute* "chkrootkit"
                  (("\\^\\\\/" ) "^/")))
              ;; Fix Makefile check for linux/if.h (path differs in Guix)
              (substitute* "Makefile"
                (("if \\[ -f \"/usr/include/linux/if.h\" \\]")
                 "if true"))))
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (bin (string-append out "/bin"))
                     (libexec (string-append out "/libexec/chkrootkit"))
                     (doc (string-append out "/share/doc/chkrootkit")))
                ;; Install main script
                (mkdir-p bin)
                (install-file "chkrootkit" bin)
                ;; Install helper binaries
                (mkdir-p libexec)
                (for-each (lambda (f)
                            (when (file-exists? f)
                              (install-file f libexec)))
                          '("chklastlog" "chkwtmp" "ifpromisc" "chkproc"
                            "chkdirs" "check_wtmpx" "strings-static" "chkutmp"))
                ;; Install documentation
                (mkdir-p doc)
                (for-each (lambda (f)
                            (install-file f doc))
                          '("README" "README.chklastlog" "README.chkwtmp"
                            "COPYRIGHT" "ACKNOWLEDGMENTS")))))
          (add-after 'install 'wrap-program
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (libexec (string-append out "/libexec/chkrootkit"))
                     (bin (string-append out "/bin"))
                     (wrapper (string-append bin "/chkrootkit")))
                ;; First wrap with PATH
                (wrap-program wrapper
                  `("PATH" ":" prefix
                    (,libexec
                     ,(dirname (search-input-file inputs "bin/grep"))
                     ,(dirname (search-input-file inputs "bin/find"))
                     ,(dirname (search-input-file inputs "bin/ps"))
                     ,(dirname (search-input-file inputs "bin/netstat"))
                     ,(dirname (search-input-file inputs "bin/awk"))
                     ,(dirname (search-input-file inputs "bin/file"))
                     ,(dirname (search-input-file inputs "bin/sed"))
                     ;; coreutils: cut, echo, head, id, ls, uname
                     ,(dirname (search-input-file inputs "bin/ls"))
                     ,(dirname (search-input-file inputs "bin/strings")))))
                ;; Create outer wrapper to filter "No such file or directory" from stderr
                ;; These come from find searching FHS paths that don't exist on Guix
                (let ((wrapped (string-append wrapper "-wrapped")))
                  (rename-file wrapper wrapped)
                  (call-with-output-file wrapper
                    (lambda (port)
                      (format port "#!~a
exec ~a \"$@\" 2> >(grep -v 'No such file or directory' >&2)
"
                              (search-input-file inputs "bin/bash")
                              wrapped)))
                  (chmod wrapper #o755))))))))
    (inputs
     (list bash-minimal
           binutils
           coreutils
           file
           findutils
           gawk
           grep
           net-tools
           procps
           sed))
    (home-page "https://www.chkrootkit.org/")
    (synopsis "Locally check for signs of a rootkit")
    (description
     "chkrootkit is a tool to locally check for signs of a rootkit installed
on a Unix system.  It contains a shell script that checks system binaries for
rootkit modification, and several programs that perform various security
checks including examining network interfaces for promiscuous mode, checking
for deleted files still being accessed, and scanning for known rootkit
signatures in log files and system binaries.")
    (license license:bsd-2)))

(define-public osv-scanner
  (package
    (name "osv-scanner")
    (version "2.3.5")
    (source (origin
              (method go-fetch-vendored)
              (uri (go-git-reference
                    (url "https://github.com/google/osv-scanner")
                    (commit (string-append "v" version))
                    (sha (base32
                          "1ph7cwaca7yblqwxs7dir917mli46w1ghmhlz69gxxvnrdq26hxl"))))
              (sha256
               (base32
                "1ndxdvp6jsihnaf68n63fww4l8zqbi3d0qmj31zbq4h52d636y9c"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/google/osv-scanner/v2/cmd/osv-scanner"
      #:unpack-path "github.com/google/osv-scanner/v2"
      #:install-source? #f
      #:go go-1.26
      ;; segmentio/asm's amd64 assembly fails to link in our environment;
      ;; fall back to the pure-Go implementations via the 'purego' tag.
      #:build-flags #~(list "-tags=purego")
      #:phases
      #~(modify-phases %standard-phases
          (delete 'check))))
    (home-page "https://github.com/google/osv-scanner")
    (synopsis "Vulnerability scanner for project dependencies")
    (description
     "OSV-Scanner finds existing vulnerabilities affecting a project's
dependencies.  It is the official frontend to the OSV database and supports a
wide range of languages and package managers including npm, pip, cargo, go
modules, maven, composer, and many more.  It can also scan container images
and OS packages on Linux systems.")
    (license license:asl2.0)))
