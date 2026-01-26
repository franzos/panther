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
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages file)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages admin))

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
            (lambda* (#:key inputs #:allow-other-keys)
              ;; Patch shebang and ensure script uses tools from PATH
              (substitute* "chkrootkit"
                (("#!/bin/sh")
                 (string-append "#!" (search-input-file inputs "bin/sh"))))
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
                     (libexec (string-append out "/libexec/chkrootkit")))
                (wrap-program (string-append out "/bin/chkrootkit")
                  `("PATH" ":" prefix
                    (,libexec
                     ,(dirname (search-input-file inputs "bin/grep"))
                     ,(dirname (search-input-file inputs "bin/find"))
                     ,(dirname (search-input-file inputs "bin/ps"))
                     ,(dirname (search-input-file inputs "bin/netstat"))
                     ,(dirname (search-input-file inputs "bin/awk"))
                     ,(dirname (search-input-file inputs "bin/file"))
                     ,(dirname (search-input-file inputs "bin/head")))))))))))
    (inputs
     (list bash-minimal
           coreutils
           file
           findutils
           gawk
           grep
           net-tools
           procps))
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
