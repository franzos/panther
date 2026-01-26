;;; Package Repository for GNU Guix
;;; Copyright © 2025 Franz Geffke <mail@gofranz.com>

(define-module (px packages music)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages datastructures)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages music)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public strawberry
  (package
    (inherit (@ (gnu packages music) strawberry))
    (name "strawberry")
    (version "1.2.17")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/strawberrymusicplayer/strawberry")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1zwbigdnwbrzbyz7r5cb8df42r82bsrw2rbf6maa15dmjqx9a9b7"))
       (modules '((guix build utils)
                  (ice-9 regex)))
       (snippet
        '(begin
           (use-modules ((ice-9 regex)))
           (for-each
            (lambda (dir)
              ;; TODO: The following dependencies are still bundled:
              ;; - "singleapplication"
              ;; - "discord-rpc"
              (let ((bundled '("singleapplication" "discord-rpc")))
                (if (not
                     (string-match
                      (string-append ".?*(" (string-join bundled "|") ")")
                      dir))
                    (delete-file-recursively dir))))
            (find-files "3rdparty"
                        (lambda (file stat)
                          (string-match "^3rdparty/[^/]*$" file))
                        #:directories? #t))))))
    (inputs
     (modify-inputs (package-inputs (@ (gnu packages music) strawberry))
       (append kdsingleapplication
               (@ (gnu packages music) libgpod)
               rapidjson
               sparsehash)))))

(define-public tidal-hifi
  (package
    (name "tidal-hifi")
    (version "6.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/Mastermindzh/tidal-hifi/releases/download/"
             version "/tidal-hifi_" version "_amd64.deb"))
       (sha256
        (base32 "1k0lyi06qj4mf08fiv54wmhax0r5rksis1axwm6r4izgiykpzj77"))))
    (supported-systems '("x86_64-linux"))
    (build-system gnu-build-system)
    (arguments
     (list
      #:validate-runpath? #f
      #:phases
      #~(modify-phases %standard-phases
          (replace 'unpack
            (lambda* (#:key inputs #:allow-other-keys)
              (invoke "ar" "x" #$source)
              (invoke "tar" "xf" "data.tar.xz")))
          (delete 'configure)
          (delete 'build)
          (delete 'check)
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (bin (string-append out "/bin"))
                     (lib (string-append out "/lib/tidal-hifi"))
                     (share (string-append out "/share")))
                ;; Install the main application
                (copy-recursively "opt/tidal-hifi" lib)
                ;; Create bin directory and symlink
                (mkdir-p bin)
                (symlink (string-append lib "/tidal-hifi")
                         (string-append bin "/tidal-hifi"))
                ;; Install desktop file and icons
                (copy-recursively "usr/share" share)
                ;; Patch desktop file to use correct exec path
                (substitute* (string-append share "/applications/tidal-hifi.desktop")
                  (("/opt/tidal-hifi/tidal-hifi")
                   (string-append out "/bin/tidal-hifi"))))))
          (add-after 'install 'patchelf
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (lib (string-append out "/lib/tidal-hifi"))
                     (ld-so (search-input-file inputs "/lib/ld-linux-x86-64.so.2")))
                ;; Patch the main binary with correct interpreter
                (invoke "patchelf" "--set-interpreter" ld-so
                        (string-append lib "/tidal-hifi"))
                ;; Patch bundled libraries
                (for-each
                 (lambda (lib-file)
                   (system* "patchelf" "--set-rpath"
                           (string-append lib ":" (dirname lib-file))
                           lib-file))
                 (find-files lib "\\.so")))))
          (add-after 'patchelf 'wrap
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (use-modules (guix build utils))
              (let* ((out (assoc-ref outputs "out"))
                     (bin (string-append out "/bin"))
                     (lib (string-append out "/lib/tidal-hifi"))
                     (libs (list
                            #$(this-package-input "alsa-lib")
                            #$(this-package-input "at-spi2-core")
                            #$(this-package-input "cairo")
                            #$(this-package-input "cups")
                            #$(this-package-input "dbus")
                            #$(this-package-input "eudev")
                            #$(this-package-input "expat")
                            #$(this-package-input "glib")
                            #$(this-package-input "gtk+")
                            #$(this-package-input "libdrm")
                            #$(this-package-input "libx11")
                            #$(this-package-input "libxcb")
                            #$(this-package-input "libxcomposite")
                            #$(this-package-input "libxdamage")
                            #$(this-package-input "libxext")
                            #$(this-package-input "libxfixes")
                            #$(this-package-input "libxkbcommon")
                            #$(this-package-input "libxrandr")
                            #$(this-package-input "mesa")
                            #$(this-package-input "nspr")
                            #$(this-package-input "nss")
                            #$(this-package-input "pango")
                            #$(this-package-input "pulseaudio")))
                     (ld-lib-path (string-join
                                   (append
                                    ;; Add the app directory for bundled libs
                                    (list lib)
                                    ;; Add gcc lib
                                    (list (string-append #$gcc:lib "/lib"))
                                    ;; Add all input library paths
                                    (map (lambda (pkg)
                                          (string-append pkg "/lib"))
                                        libs)
                                    ;; Add NSS subdirectory specifically
                                    (list (string-append #$(this-package-input "nss") "/lib/nss")
                                          (string-append #$(this-package-input "nspr") "/lib/nspr")))
                                   ":")))
                (wrap-program (string-append bin "/tidal-hifi")
                  `("LD_LIBRARY_PATH" ":" prefix (,ld-lib-path)))))))))
    (native-inputs
     (list binutils                      ; for ar
           patchelf
           tar
           xz))
    (inputs
     (list `(,gcc "lib")
           alsa-lib
           at-spi2-core
           cairo
           cups
           dbus
           eudev
           expat
           glib
           gtk+
           libdrm
           libx11
           libxcb
           libxcomposite
           libxdamage
           libxext
           libxfixes
           libxkbcommon
           libxrandr
           mesa
           nspr
           nss
           pango
           pulseaudio))
    (home-page "https://github.com/Mastermindzh/tidal-hifi")
    (synopsis "High fidelity TIDAL music streaming client")
    (description
     "TIDAL Hi-Fi is an Electron-based desktop application that provides
a high-fidelity music streaming experience for TIDAL.  It supports Widevine
DRM for high-quality audio playback and includes features like custom themes,
Discord integration, and media key support.")
    (license license:expat)))
