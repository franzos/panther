;;; Package Repository for GNU Guix
;;; Copyright © 2021-2025 Franz Geffke <mail@gofranz.com>

(define-module (px packages activitywatch)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (ice-9 match)
  #:use-module (nonguix build-system binary)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages gcc))

(define* (make-aw-release-asset version asset hash)
  (origin
    (method url-fetch)
    (uri (string-append
          "https://github.com/ActivityWatch/activitywatch/releases/download/v"
          version
          "/"
          asset
          "-v"
          version
          "-"
          (match (or (%current-system)
                     (%current-target-system))
            ("x86_64-linux" "linux-x86_64")
            ("i686-linux" "linux-ia32")
            ("aarch64-linux" "linux-arm64")
            ("armhf-linux" "linux-armv7l"))
          ".zip"))
    (sha256 (base32 hash))))

(define-public activitywatch
  (package
    (name "activitywatch")
    (version "0.14.0")
    (source
     #f)
    (build-system binary-build-system)
    (arguments
     `(#:patchelf-plan `(("opt/activitywatch/aw-qt" ("libxkbcommon"
                                                     "xkeyboard-config"
                                                     "libxcb" "glibc"
                                                     "gcc:lib"))
                         ("opt/activitywatch/aw-server/aw-server" ("glibc"))
                         ("opt/activitywatch/aw-server-rust/aw-server-rust" ("glibc"))
                         ("opt/activitywatch/aw-watcher-afk/aw-watcher-afk" ("glibc"))
                         ("opt/activitywatch/aw-watcher-window/aw-watcher-window"
                          ("glibc")))
       #:phases (modify-phases %standard-phases
                  (replace 'unpack
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (invoke "unzip"
                              (assoc-ref inputs "activitywatch"))
                      (mkdir "opt")
                      (invoke "mv" "activitywatch" "opt/")
                      (substitute* '("opt/activitywatch/aw-qt.desktop")
                        (("aw-qt")
                         (string-append %output "/bin/aw-qt")))
                      ;; remove for conflicting issue with libpng
                      (invoke "rm" "opt/activitywatch/libz.so.1")
                      ;; remove for conflicting issue with mesa
                      (invoke "rm" "opt/activitywatch/libstdc++.so.6")
                      ;; remove for conflicting wayland libraries
                      (invoke "rm" "opt/activitywatch/libwayland-egl.so.1")
                      (invoke "rm" "opt/activitywatch/libwayland-cursor.so.0")
                      ;; mv desktop file
                      (mkdir-p "share/applications/")
                      (invoke "mv" "opt/activitywatch/aw-qt.desktop"
                              "share/applications/")
                      ;;
                      (mkdir-p "share/icons/hicolor/512x512/apps/")
                      (invoke "cp"
                       "opt/activitywatch/aw-server/aw_server/static/logo.png"
                       "share/icons/hicolor/512x512/apps/activitywatch.png")
                      #t))
                  (add-after 'install 'symlink-binary-file-and-cleanup
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (realexecpath (string-append %output
                                            "/opt/activitywatch/aw-qt "))
                             (sh_path (string-append %output "/bin/aw-qt"))
                             (sh_content (string-append "#!"
                                          (assoc-ref inputs "bash")
                                          "/bin/bash\n"
                                          "export ROCKET_ENV=production\n"
                                          "if [ $# -eq 0 ]; then "
                                          realexecpath
                                          "--autostart-modules aw-server-rust,aw-watcher-afk,aw-watcher-window;"
                                          "else "
                                          realexecpath
                                          "$@;"
                                          "fi")))
                        (invoke "rm"
                                (string-append %output
                                               "/environment-variables"))
                        (mkdir-p (string-append %output "/bin"))
                        (with-output-to-file sh_path
                          (lambda _
                            (format #t sh_content)))
                        (chmod sh_path #o555)) #t))
                  (add-after 'install 'wrap-where-patchelf-does-not-work
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out")))
                        (wrap-program (string-append out
                                       "/opt/activitywatch/aw-qt")
                          `("QT_XKB_CONFIG_ROOT" ":" prefix
                            (,(string-join (list (string-append (assoc-ref
                                                                 inputs
                                                                 "xkeyboard-config")
                                                                "/share/X11/")
                                                 out) ":"))))
                        (wrap-program (string-append out
                                       "/opt/activitywatch/aw-qt")
                          `("FONTCONFIG_PATH" ":" prefix
                            (,(string-join (list (string-append (assoc-ref
                                                                 inputs
                                                                 "fontconfig")
                                                                "/etc/fonts")
                                                 out) ":"))))
                        (wrap-program (string-append out
                                       "/opt/activitywatch/aw-qt")
                          `("LD_LIBRARY_PATH" ":" prefix
                            (,(string-join (list (string-append (assoc-ref
                                                                 inputs "mesa")
                                                                "/lib")
                                                 (string-append (assoc-ref
                                                                 inputs
                                                                 "fontconfig")
                                                                "/lib")
                                                 (string-append (assoc-ref
                                                                 inputs "zlib")
                                                                "/lib")
                                                 (string-append (assoc-ref
                                                                 inputs
                                                                 "libxcb")
                                                                "/lib")
                                                 (string-append (assoc-ref
                                                                 inputs
                                                                 "xcb-util-cursor")
                                                                "/lib")
                                                 (string-append (assoc-ref
                                                                 inputs
                                                                 "glibc")
                                                                "/lib")
                                                 (string-append (assoc-ref
                                                                 inputs "gcc")
                                                                "/lib")
                                                 (string-append (assoc-ref
                                                                 inputs
                                                                 "freetype")
                                                                "/lib")
                                                 (string-append out
                                                  "/opt/activitywatch")
                                                 out) ":"))))) #t)))))
    (native-inputs `(("unzip" ,unzip)))
    (inputs `(("activitywatch" ,(make-aw-release-asset version "activitywatch"
                                 "007g61i0gyzv1jrpcdvf3505qgkimdb649dh31frv55m5pwz46hi"))
              ("gcc:lib" ,gcc "lib")
              ("gcc" ,gcc "lib")
              ("glibc" ,glibc)
              ("fontconfig" ,fontconfig)
              ("freetype" ,freetype)
              ("libxcb" ,libxcb)
              ("xcb-util-cursor" ,xcb-util-cursor)
              ("libxkbcommon" ,libxkbcommon)
              ("mesa" ,mesa)
              ("xkeyboard-config" ,xkeyboard-config)
              ("zlib" ,zlib)
              ("bash-minimal" ,bash-minimal)))
    (home-page "https://activitywatch.net/")
    (synopsis
     "ActivityWatch is an automatic time-tracking software which helps you keep track of what you do.")
    (description
     "ActivityWatch is about recording our digital lives, an evergrowing part of our lives in general, and the new opportunities enabled by such a record.")
    (license license:mpl2.0)))
