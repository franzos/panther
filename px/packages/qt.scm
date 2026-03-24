;;; Package Repository for GNU Guix
;;; Copyright © 2021-2025 Franz Geffke <mail@gofranz.com>

(define-module (px packages qt)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (gnu packages documentation)
  #:use-module (guix packages)
  #:use-module (guix build-system cmake)
  #:use-module (guix utils)
  #:use-module (guix build-system qt)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages build-tools)
  #:use-module (srfi srfi-1)
  #:use-module (px packages desktop-tools))

(define (qt5-urls component version)
  "Return a list of URLs for VERSION of the Qt5 COMPONENT."
  ;; We can't use a mirror:// scheme because these URLs are not exact copies:
  ;; the layout differs between them.
  (list (string-append "https://download.qt.io/official_releases/qt/"
                       (version-major+minor version) "/" version
                       "/submodules/" component "-everywhere-src-"
                       version ".tar.xz")
        (string-append "https://download.qt.io/archive/qt/"
                       (version-major+minor version) "/" version
                       "/submodules/" component "-everywhere-src-"
                       version ".tar.xz")
        (let ((directory (string-append "qt5" (string-drop component 2))))
          (string-append "http://sources.buildroot.net/" directory "/"
                         component "-everywhere-src-" version ".tar.xz"))
        (string-append "https://distfiles.macports.org/qt5/"
                       component "-everywhere-src-" version ".tar.xz")))

(define-public qtbase-with-bundled-sqlite
  (package (inherit qtbase-5)
    (name "qtbase-with-bundled-sqlite")
    (source (origin
              (inherit (package-source qtbase-5))
              (snippet
                ;; corelib uses bundled harfbuzz, md4, md5, sha3
                '(begin
                  (with-directory-excursion "src/3rdparty"
                    (for-each delete-file-recursively
                              (list "double-conversion" "freetype" "harfbuzz-ng"
                                    "libpng" "libjpeg" "pcre2" "xcb" "zlib"))
                  #t)))))
    (arguments (substitute-keyword-arguments (package-arguments qtbase-5)
      ((#:phases phases '%standard-phases)
      `(modify-phases ,phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute* "configure"
                 (("/bin/pwd") (which "pwd")))
               (substitute* "src/corelib/global/global.pri"
                 (("/bin/ls") (which "ls")))
               ;; The configuration files for other Qt5 packages are searched
               ;; through a call to "find_package" in Qt5Config.cmake, which
               ;; disables the use of CMAKE_PREFIX_PATH via the parameter
               ;; "NO_DEFAULT_PATH". Re-enable it so that the different
               ;; components can be installed in different places.
               (substitute* (find-files "." ".*\\.cmake")
                 (("NO_DEFAULT_PATH") ""))
               ;; do not pass "--enable-fast-install", which makes the
               ;; configure process fail
               (invoke
                 "./configure"
                 "-verbose"
                 "-prefix" out
                 "-docdir" (string-append out "/share/doc/qt5")
                 "-headerdir" (string-append out "/include/qt5")
                 "-archdatadir" (string-append out "/lib/qt5")
                 "-datadir" (string-append out "/share/qt5")
                 "-examplesdir" (string-append
                                  out "/share/doc/qt5/examples")
                 "-opensource"
                 "-confirm-license"

                 ;; These features require higher versions of Linux than the
                 ;; minimum version of the glibc.  See
                 ;; src/corelib/global/minimum-linux_p.h.  By disabling these
                 ;; features Qt5 applications can be used on the oldest
                 ;; kernels that the glibc supports, including the RHEL6
                 ;; (2.6.32) and RHEL7 (3.10) kernels.
                 "-no-feature-getentropy"  ; requires Linux 3.17
                 "-no-feature-renameat2"   ; requires Linux 3.16

                 ;; Do not build examples; if desired, these could go
                 ;; into a separate output, but for the time being, we
                 ;; prefer to save the space and build time.
                 "-no-compile-examples"
                 ;; Most "-system-..." are automatic, but some use
                 ;; the bundled copy by default.
                 ; "-system-sqlite"
                 "-system-harfbuzz"
                 "-system-pcre"
                 ;; explicitly link with openssl instead of dlopening it
                 "-openssl-linked"
                 ;; explicitly link with dbus instead of dlopening it
                 "-dbus-linked"
                 ;; don't use the precompiled headers
                 "-no-pch"
                 ;; drop special machine instructions that do not have
                 ;; runtime detection
                 ,@(if (string-prefix? "x86_64"
                                       (or (%current-target-system)
                                           (%current-system)))
                     '()
                     '("-no-sse2"))
                 "-no-mips_dsp"
                 "-no-mips_dspr2"))))
             ))))
	))

(define-public qt-location-plugin-googlemap
  (package
    (name "qt-location-plugin-googlemap")
    (version "0.0.0.2")
    (source
     (origin
       (method url-fetch)
        (uri (string-append
                 "https://github.com/vladest/googlemaps/archive/refs/tags/v." version ".tar.gz"))
        (sha256
         (base32 "1w2vgall1alc2mw5vd4c0wfxa75vri4q1qqwhvrdyxbyj6azkhma"))))
    (arguments
     `(#:tests? #f ; no tests))
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "googlemaps.pro"
               (("\\$\\$\\[QT_INSTALL_PLUGINS\\]") 
                (string-append (assoc-ref outputs "out") "/lib/qt5/plugins")))
             (invoke "qmake" "googlemaps.pro" ))))))
    (build-system qt-build-system)
    (inputs
     (list qtbase-5
           qtlocation-5
           qtdeclarative-5))
    (native-inputs
      (list pkg-config qttools-5))
    (home-page "https://github.com/vladest/googlemaps")
    (synopsis "Google Maps plugin for QtLocation")
    (description "GoogleMaps plugin for QtLocation module")
    (license license:gpl3+)))

(define-public qtutilities
  (package
    (name "qtutilities")
    (version "6.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/Martchus/qtutilities/archive/refs/tags/v"
             version ".tar.gz"))
       (sha256
        (base32 "1lxly6zikikya7ssh37na8ll073pm80c1jddwi4sk0vybcvb42xp"))))
    (build-system cmake-build-system)
    (native-inputs (list cpputilities qtbase-5 qttools-5))
    (arguments
     `(#:tests? #f))
    (home-page "https://github.com/Martchus/qtutilities")
    (synopsis "Common Qt related C++ classes and routines")
    (description "Common Qt related C++ classes and routines")
    (license license:gpl2+)))