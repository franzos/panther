;;; Package Repository for GNU Guix
;;; Copyright © 2026 Franz Geffke <mail@gofranz.com>

(define-module (px packages browser)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (nonguix build-system chromium-binary)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages photo)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages video)
  #:use-module (gnu packages wget)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xorg))

(define-public thorium-browser
  (package
    (name "thorium-browser")
    (version "138.0.7204.303")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/Alex313031/thorium/releases/download/"
                    "M" version "/thorium-browser_" version "_AVX2.deb"))
              (sha256
               (base32 "163shilxjn5rwxc19alwp2gc96nmw0nrkxk529zl7c5hi9f5l1fz"))))
    (build-system chromium-binary-build-system)
    (arguments
     (list
       #:substitutable? #f
       #:wrapper-plan
        #~(let ((path "opt/chromium.org/thorium/"))
            (map (lambda (file)
                   (string-append path file))
                 '("thorium"
                   "chrome-sandbox"
                   "chrome_crashpad_handler"
                   "libEGL.so"
                   "libGLESv2.so"
                   "libqt5_shim.so"
                   "libqt6_shim.so"
                   "libvk_swiftshader.so"
                   "libvulkan.so.1"
                   "WidevineCdm/_platform_specific/linux_x64/libwidevinecdm.so")))
       #:install-plan
        #~'(("opt/" "/share")
            ("usr/share/" "/share"))
       #:phases
        #~(modify-phases %standard-phases
            (add-before 'install 'patch-assets
              (lambda _
                (let* ((bin (string-append #$output "/bin"))
                       (share (string-append #$output "/share"))
                       (opt "./opt")
                       (usr/share "./usr/share")
                       (old-exe "/opt/chromium.org/thorium/thorium-browser")
                       (exe (string-append bin "/thorium-browser")))
                  (substitute* (string-append opt "/chromium.org/thorium/thorium-browser")
                    (("CHROME_WRAPPER") "WRAPPER"))
                  (substitute* (string-append usr/share "/applications/thorium-browser.desktop")
                    (("^Exec=/usr/bin/thorium-browser") (string-append "Exec=" exe))
                    (("^Exec=/usr/bin/thorium-shell") (string-append "Exec=" exe " --content-shell")))
                  (substitute* (string-append usr/share "/gnome-control-center/default-apps/thorium-browser.xml")
                    ((old-exe) exe)))))
            (add-after 'install 'install-icons
               (lambda _
                 (define (format-icon-size name)
                   (car
                     (string-split
                      (string-drop-right (string-drop name 13) 4)
                      #\_)))
                 (let ((icons (string-append #$output "/share/icons/hicolor"))
                       (share (string-append #$output "/share/chromium.org/thorium")))
                   (for-each (lambda (icon)
                               (let* ((icon-name (basename icon))
                                      (icon-size (format-icon-size icon-name))
                                      (target (string-append icons "/" icon-size "x" icon-size "/apps/thorium-browser.png")))
                                 (mkdir-p (dirname target))
                                 (rename-file icon target)))
                             (find-files share "product_logo_.*\\.png")))))
            (add-before 'install-wrapper 'install-exe
             (lambda _
               (let* ((bin (string-append #$output "/bin"))
                      (exe (string-append bin "/thorium-browser"))
                      (share (string-append #$output "/share"))
                      (chrome-target (string-append share "/chromium.org/thorium/thorium-browser")))
                 (mkdir-p bin)
                 (symlink chrome-target exe)
                 (wrap-program exe
                   '("CHROME_WRAPPER" = ("thorium")))))))))
    (inputs
     (list bzip2
           curl
           flac
           font-liberation
           gdk-pixbuf
           gtk
           harfbuzz
           libexif
           libglvnd
           libpng
           libva
           libxscrnsaver
           opus
           pciutils
           pipewire
           qtbase-5
           qtbase
           snappy
           util-linux
           xdg-utils
           wget))
    (supported-systems '("x86_64-linux"))
    (home-page "https://thorium.rocks/")
    (synopsis "Performance-focused Chromium fork with AVX2 optimizations")
    (description "Thorium is a Chromium fork optimized for performance with compiler
optimizations (thinLTO, PGO, AVX2), all codecs enabled (including HEVC), Widevine
DRM support, and VA-API hardware video acceleration patches.")
    (license license:bsd-3)))
