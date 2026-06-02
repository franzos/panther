;;; Package Repository for GNU Guix
;;; Copyright © 2021-2025 Franz Geffke <mail@gofranz.com>

(define-module (px packages tpm)
  #:use-module (gnu packages)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system python)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages check)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages dns)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages gtk)
  #:use-module ((gnu packages hardware) #:hide (tpm2-tss))
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages man)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages cyrus-sasl)
  #:use-module (gnu packages popt)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages security-token)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml))

(define-public tpm2-tss-3-openssl-1.1
  (package
    (name "tpm2-tss")
    (version "3.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/tpm2-software/tpm2-tss"
                           "/releases/download/"
                           version
                           "/tpm2-tss-"
                           version
                           ".tar.gz"))
       (sha256
        (base32 "05xynpwq851fp8f5fy7ac0blvz8mr5m5cbqj3gslgbwv63kjnfbq"))))
    (build-system gnu-build-system)
    (native-inputs (list pkg-config))
    (inputs (list curl json-c openssl-1.1))
    (home-page "https://tpm2-software.github.io/")
    (synopsis "OSS Implementation of the TCG TPM2 Software Stack (TSS2)")
    (description
     "This package provides the @acronym{TCG, Trusted Computing Group}
@acronym{TSS2, TPM2 Software Stack}.  The stack contains libtss2-fapi,
libtss2-esys, libtss2-sys, libtss2-mu, libtss2-tcti-device, libtss2-tcti-swtpm
and libtss2-tcti-mssim.")
    (license license:bsd-2)))

(define-public tpm2-tss
  (package
    (name "tpm2-tss")
    (version "4.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/tpm2-software/tpm2-tss"
                           "/releases/download/"
                           version
                           "/tpm2-tss-"
                           version
                           ".tar.gz"))
       (sha256
        (base32 "1s1v1nk3f9rkpxcwanz8rf9hrvma869dhwn83xfk0y5b0015iw9p"))))
    (build-system gnu-build-system)
    ;; shadow provides groupadd, which configure requires for the tss user.
    (native-inputs (list pkg-config shadow))
    (inputs `(("curl" ,curl)
              ("json-c" ,json-c)
              ("openssl" ,openssl)
              ("libuuid" ,util-linux "lib")))
    (home-page "https://tpm2-software.github.io/")
    (synopsis "OSS Implementation of the TCG TPM2 Software Stack (TSS2)")
    (description
     "This package provides the @acronym{TCG, Trusted Computing Group}
@acronym{TSS2, TPM2 Software Stack}.  The stack contains libtss2-fapi,
libtss2-esys, libtss2-sys, libtss2-mu, libtss2-tcti-device, libtss2-tcti-swtpm
and libtss2-tcti-mssim.")
    (license license:bsd-2)))

(define-public tpm2-tss-engine
  (package
    (name "tpm2-tss-engine")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/tpm2-software/tpm2-tss-engine/archive/refs/tags/"
             version ".tar.gz"))
       (sha256
        (base32 "1cjfj0gl6d9kmc18h54kfs065lx3qyfjm8cj9hzjmkwin6m726rb"))
       (modules '((guix build utils)))
       (snippet '(begin
                   (substitute* "bootstrap"
                     (("git describe --tags --always --dirty > VERSION")
                      "echo" version "> VERSION"))))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ;only manual test scripts
       #:configure-flags (list (string-append "--with-enginesdir="
                                              (assoc-ref %outputs "out")
                                              "/lib/engines-1.1/"))
       #:phases (modify-phases %standard-phases
                  (add-after 'install 'setting-env-vars-install-openssl-conf
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out"))
                            (tpm2-tss (assoc-ref %build-inputs "tpm2-tss"))
                            (engine-path (string-append (assoc-ref %outputs
                                                                   "out")
                                                        "/lib/engines-1.1/"))
                            (opensslconf-file "openssl.conf.sample")
                            (opensslconf-path (string-append (assoc-ref
                                                              %outputs "out")
                                                             "/etc/")))
                        (wrap-program (string-append out "/bin/tpm2tss-genkey")
                          `("OPENSSL_ENGINES" ":" prefix
                            (,engine-path)))
                        ;; (wrap-program (string-append out "/bin/tpm2tss-genkey")
                        ;; `("TPM2TSSENGINE_TCTI" ":" prefix (,(string-append tpm2-tss "/lib/libtss2-tcti-device.so:/dev/tpm0"))))
                        ;; (wrap-program (string-append out "/bin/tpm2tss-genkey")
                        ;; `("TPM2TOOLS_TCTI" ":" prefix (,(string-append tpm2-tss "/lib/libtss2-tcti-device.so:/dev/tpm0"))))
                        (mkdir-p opensslconf-path)
                        (substitute* opensslconf-file
                          (("/usr")
                           out))
                        (substitute* opensslconf-file
                          (("default_algorithms")
                           "#default_algorithms"))
                        (copy-file opensslconf-file
                                   (string-append opensslconf-path
                                                  "openssl-tss2.conf"))
                        #t))))))
    (native-inputs `(("autoconf" ,autoconf)
                     ("autoconf-archive" ,autoconf-archive)
                     ("automake" ,automake)
                     ("bash" ,bash)
                     ("curl" ,curl)
                     ("doxygen" ,doxygen)
                     ("json-c" ,json-c)
                     ("libgcrypt" ,libgcrypt)
                     ("libtool" ,libtool)
                     ("m4" ,m4)
                     ("pkg-config" ,pkg-config)
                     ("openssl" ,openssl-1.1)
                     ("patchelf" ,patchelf)))
    (inputs `(("tpm2-tss" ,tpm2-tss-3-openssl-1.1)
              ("bash-minimal" ,bash-minimal)))
    (home-page "https://github.com/tpm2-software/tpm2-tss-engine")
    (synopsis "OpenSSL Engine for TPM2 devices")
    (description
     "The tpm2-tss-engine project implements a cryptographic engine for OpenSSL
for Trusted Platform Module (TPM 2.0) using the tpm2-tss software stack that
follows the Trusted Computing Groups (TCG) TPM Software Stack (TSS 2.0).")
    (license license:bsd-2)))

(define-public tpm2-abrmd
  (package
    (name "tpm2-abrmd")
    (version "3.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/tpm2-software/tpm2-abrmd/releases/download/"
             version "/tpm2-abrmd-" version ".tar.gz"))
       (sha256
        (base32 "0rglv5nj2xng7x57akqh6fj1hsdnyq3ndf2ma6q0b9sa2qsgz6nm"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f))                    ;tests need a swtpm and dbus session
    (native-inputs `(("glib" ,glib "bin")
                     ("pkg-config" ,pkg-config)
                     ("which" ,which)))
    (inputs `(("glib" ,glib)
              ("tpm2-tss" ,tpm2-tss)))
    (home-page "https://github.com/tpm2-software/tpm2-abrmd")
    (synopsis "TPM2 Access Broker & Resource Manager")
    (description
     "This is a system daemon implementing the TPM2 access broker (TAB) 
& Resource Manager (RM) spec from the TCG. The daemon (tpm2-abrmd) is 
implemented using Glib and the GObject system. In this documentation and in the code 
we use tpm2-abrmd and tabrmd interchangeably.")
    (license license:bsd-2)))

(define-public tpm2-pkcs11
  (package
    (name "tpm2-pkcs11")
    (version "1.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/tpm2-software/tpm2-pkcs11/archive/refs/tags/"
             version ".tar.gz"))
       (sha256
        (base32 "0kkzzdxiz1389jl4rabh739m99x1jh42xagq4sycn5s8kvik1sa5"))

       (modules '((guix build utils)))
       (snippet '(begin
                   (substitute* "bootstrap"
                     (("git describe --tags --always --dirty > VERSION")
                      "echo" version " > VERSION"))))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ;only manual test scripts
       ;; #:configure-flags (list "--enable-tctienvvar")
       ))
    (native-inputs `(("autoconf" ,autoconf)
                     ("autoconf-archive" ,autoconf-archive)
                     ("automake" ,automake)
                     ("libtool" ,libtool)
                     ("m4" ,m4)
                     ("pkg-config" ,pkg-config)))
    (inputs `(("libyaml" ,libyaml)
              ("sqlite" ,sqlite)
              ("openssl" ,openssl-1.1)
              ("tpm2-abrmd" ,tpm2-abrmd)
              ("tpm2-tools" ,tpm2-tools)
              ("tpm2-tss" ,tpm2-tss-3-openssl-1.1)))
    (home-page "https://github.com/tpm2-software/tpm2-pkcs11")
    (synopsis "A PKCS#11 interface for TPM2 hardware")
    (description
     "PKCS #11 is a Public-Key Cryptography Standard that defines a standard method 
to access cryptographic services from tokens/ devices such as hardware security modules (HSM), 
smart cards, etc. In this project we intend to use a TPM2 device as the cryptographic token.")
    (license license:bsd-2)))
