;;; Package Repository for GNU Guix
;;; Copyright © 2021-2025 Franz Geffke <mail@gofranz.com>

(define-module (px packages security-token)
  #:use-module (gnu packages)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages base)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages compiler-tools)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages security-token)
  #:use-module (px packages python-xyz))

(define-public acsccid
  (package
    (name "acsccid")
    (version "1.1.13")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/acshk/acsccid/archive/v"
                           version ".tar.gz"))
       (sha256
        (base32 "1f4ddw2fglv3fayizibxb8rwzbmawp4byppxjm6rlhdbvvii3xcw"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags (list (string-append "--enable-usbdropdir=" %output
                                              "/pcsc/drivers"))
       #:phases (modify-phases %standard-phases
                  (add-before 'bootstrap 'patch-bootstrap-gettext
                    (lambda* (#:key inputs #:allow-other-keys)
                      ;; bootstrap hard-codes /usr/share/gettext for config.rpath.
                      (substitute* "bootstrap"
                        (("/usr/share/gettext")
                         (string-append (assoc-ref inputs "gettext")
                                        "/share/gettext")))
                      ;; Tarball lacks the AC_CONFIG_MACRO_DIR dir aclocal needs.
                      (mkdir-p "m4") #t))
                  (add-after 'configure 'patch-Makefile
                    (lambda _
                      (substitute* "src/Makefile.in"
                        (("/bin/echo")
                         (which "echo"))) #t)))))
    (native-inputs `(("autoconf" ,autoconf)
                     ("autoconf-archive" ,autoconf-archive)
                     ("automake" ,automake)
                     ("flex" ,flex)
                     ("gettext" ,gettext-minimal)
                     ("libtool" ,libtool)
                     ("pcsc-lite" ,pcsc-lite) ;only required for headers
                     ("perl" ,perl)
                     ("pkg-config" ,pkg-config)))
    (inputs `(("libusb" ,libusb)))
    (home-page "https://github.com/acshk/acsccid")
    (synopsis "ACS CCID PC/SC Driver for Linux/Mac OS X")
    (description
     "acsccid is a PC/SC driver for Linux/Mac OS X and it supports ACS CCID smart card
readers. This library provides a PC/SC IFD handler implementation and
communicates with the readers through the PC/SC Lite resource manager (pcscd).")
    (license license:lgpl2.1+)))

(define-public id-card-reader
  (package
    (name "id-card-reader")
    (version "0.1.27")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://source.pantherx.org/" name "_v" version
                           ".tgz"))
       (sha256
        (base32 "0xkly0qhgr96y6qk4h24a5llm6yl650ad26i2nbilzx1csyxryf2"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))
    (native-inputs (list python-setuptools))
    (inputs `(("python-pillow" ,python-pillow)
              ("python-pyscard" ,python-pyscard)))
    (home-page "https://www.pantherx.org/")
    (synopsis " ")
    (description " ")
    (license license:expat)))

