;;; Package Repository for GNU Guix
;;; Copyright © 2026 Franz Geffke <mail@gofranz.com>

(define-module (px packages security)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages file)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages java)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages security-token)
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

(define-public autofirma
  (package
    (name "autofirma")
    (version "1.9")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://firmaelectronica.gob.es/content/dam/"
                           "firmaelectronica/descargas-software/autofirma19/"
                           "Autofirma_Linux_Debian.zip"))
       (file-name (string-append name "-" version ".zip"))
       (sha256
        (base32 "1637f6pcwrghgx0v0slqclsshdnlvdvjcn7rhzy0vw795qgjb762"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:install-plan
      #~'(("usr/lib/Autofirma/autofirma.jar" "lib/Autofirma/")
          ("usr/lib/Autofirma/autofirmaConfigurador.jar" "lib/Autofirma/")
          ("usr/lib/Autofirma/Autofirma.png"
           "share/icons/hicolor/128x128/apps/autofirma.png")
          ("usr/share/Autofirma/Autofirma.svg"
           "share/icons/hicolor/scalable/apps/autofirma.svg")
          ("usr/share/metainfo/es.gob.afirma.metainfo.xml" "share/metainfo/")
          ("usr/share/common-licenses" "share/doc/autofirma"))
      #:phases
      #~(modify-phases %standard-phases
          (replace 'unpack
            (lambda* (#:key source #:allow-other-keys)
              (invoke "unzip" "-q" source)
              (invoke "ar" "x" (car (find-files "." "\\.deb$")))
              (invoke "tar" "-xzf" "data.tar.gz")))
          (add-after 'install 'install-launchers
            (lambda* (#:key inputs #:allow-other-keys)
              (let* ((bin (string-append #$output "/bin"))
                     (lib (string-append #$output "/lib/Autofirma"))
                     (sh (search-input-file inputs "/bin/sh"))
                     (java (search-input-file inputs "/bin/java"))
                     ;; The configurator and the in-app "restore installation"
                     ;; both shell out to a bare "certutil" to register the
                     ;; generated CA with the browser NSS stores.  nss:bin
                     ;; keeps its tools at the top level, not under bin/.
                     (certutil (dirname (search-input-file inputs "certutil")))
                     (common-flags
                      (list "-Djdk.tls.maxHandshakeMessageSize=65536"
                            ;; Autofirma renames the AWT window by reflection
                            ;; so that it matches StartupWMClass; without this
                            ;; the module system refuses and the window ends up
                            ;; unmatched by the desktop entry.
                            "--add-opens=java.desktop/sun.awt.X11=ALL-UNNAMED"
                            ;; javax.smartcardio only looks for libpcsclite
                            ;; under /usr and /usr/local, so without this the
                            ;; DNIe and every other reader stays invisible.
                            (string-append
                             "-Dsun.security.smartcardio.library="
                             (search-input-file inputs
                                                "/lib/libpcsclite.so.1")))))
                (define (launcher name jvm-flags)
                  (let ((file (string-append bin "/" name)))
                    (call-with-output-file file
                      (lambda (port)
                        (format port "\
#!~a
export PATH=\"~a${PATH:+:}$PATH\"
exec ~a ~a-jar ~a \"$@\"
"
                                sh certutil java
                                (string-join jvm-flags " " 'suffix)
                                (string-append lib "/autofirma.jar"))))
                    (chmod file #o555)))

                (mkdir-p bin)
                (launcher "autofirma" common-flags)
                (launcher "autofirmacl"
                          (append common-flags '("-Dafirma_debug_level=OFF")))
                ;; "-jnlp" makes the configurator treat ~/.afirma/Autofirma as
                ;; the application directory; without it it would try to write
                ;; the generated keystore next to the jar, in the store.  The
                ;; configurator only writes script.sh, leaving it to the .deb's
                ;; postinst to run it, so run it here as well.
                (let ((file (string-append bin "/autofirma-configurador")))
                  (call-with-output-file file
                    (lambda (port)
                      (format port "\
#!~a
export PATH=\"~a${PATH:+:}$PATH\"
~a -jar ~a -jnlp \"$@\" || exit
script=\"$HOME/.afirma/Autofirma/script.sh\"
if [ -f \"$script\" ]; then
    sh \"$script\" && rm -f \"$script\"
fi
"
                              sh certutil java
                              (string-append lib
                                             "/autofirmaConfigurador.jar"))))
                  (chmod file #o555)))))
          (add-after 'install-launchers 'install-desktop-file
            (lambda _
              (let ((apps (string-append #$output "/share/applications")))
                (mkdir-p apps)
                (copy-file "usr/share/applications/afirma.desktop"
                           (string-append apps "/afirma.desktop"))
                (substitute* (string-append apps "/afirma.desktop")
                  (("Exec=/usr/bin/autofirma")
                   (string-append "Exec=" #$output "/bin/autofirma"))
                  (("Icon=/usr/lib/Autofirma/Autofirma\\.png")
                   "Icon=autofirma"))))))))
    (native-inputs (list unzip))
    (inputs (list bash-minimal openjdk21 pcsc-lite `(,nss "bin")))
    (home-page "https://firmaelectronica.gob.es/descargas")
    (synopsis "Spanish government electronic signature client")
    (description
     "Autofirma signs documents with certificates held in a PKCS#12 file or on
a smart card such as the Spanish DNIe, producing CAdES, XAdES, PAdES and OOXML
signatures.  It registers itself as the handler for @code{afirma://} links so
that public administration websites can drive it from the browser.

Browser integration talks to Autofirma over a local TLS socket, which needs a
certificate generated on the machine.  Run @command{autofirma-configurador}
once to create it under @file{~/.afirma/Autofirma} and add its CA to the
Firefox and Chromium certificate stores; the same can be done from the
application under Herramientas, Restaurar instalación.

Reading certificates out of the Mozilla Firefox and Chromium key stores needs
the NSS libraries at one of the FHS locations Autofirma looks in.  On Guix
System, @code{nss-fhs-service-type} from @code{(px services nss)} puts them
there; without it only PKCS#12 files and smart cards are available.")
    (license (list license:gpl2 license:eupl1.1))))

(define-public osv-scanner
  (package
    (name "osv-scanner")
    (version "2.5.1")
    (source (origin
              (method go-fetch-vendored)
              (uri (go-git-reference
                    (url "https://github.com/google/osv-scanner")
                    (commit (string-append "v" version))
                    (sha (base32
                          "0nyzi5r30752c0hf19q615ib9il9xqib9gw3ggyl067y7jahc44h"))))
              (sha256
               (base32
                "1dxwdpfvxw13n9dlcym1scmb4zwx5bh4pnsnhgxdk8npqck4bi8l"))))
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

(define-public scorecard
  (package
    (name "scorecard")
    (version "5.5.0")
    (source (origin
              (method go-fetch-vendored)
              (uri (go-git-reference
                    (url "https://github.com/ossf/scorecard")
                    (commit (string-append "v" version))
                    (sha (base32
                          "0k93zgh32jcrhag98dy5v43akfdi3fccaxmpghjgliib0qc08vvn"))))
              (sha256
               (base32
                "0hn439587mdwjnlndw2ay5k95y6cy9lgql42jh5fqvqxv3dn3y9a"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/ossf/scorecard/v5"
      #:install-source? #f
      #:go go-1.26
      ;; GO111MODULE=off builds in GOPATH mode, so the version package is
      ;; reached through the module's vendor/ prefix.
      #:build-flags
      #~(let ((v "github.com/ossf/scorecard/v5/vendor/sigs.k8s.io/release-utils/version"))
          (list (string-append "-ldflags=-X " v ".gitVersion=v" #$version
                               " -X " v ".gitTreeState=clean")))
      #:phases
      #~(modify-phases %standard-phases
          (delete 'check)
          (add-after 'install 'rename-binary
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((bin (string-append (assoc-ref outputs "out") "/bin")))
                (rename-file (string-append bin "/v5")
                             (string-append bin "/scorecard"))))))))
    (home-page "https://github.com/ossf/scorecard")
    (synopsis "Security health metrics for open source projects")
    (description
     "OpenSSF Scorecard assesses open source projects for security risks
through a series of automated checks.  It evaluates practices such as branch
protection, dependency update tooling, code review, CI tests, fuzzing, signed
releases, and known vulnerabilities, producing a score that helps maintainers
and consumers understand a project's security posture.")
    (license license:asl2.0)))
