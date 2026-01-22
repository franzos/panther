;;; Package Repository for GNU Guix
;;; Copyright © 2021-2025 Franz Geffke <mail@gofranz.com>

(define-module (px packages editors)
  #:use-module (nonguix build-system binary)
  #:use-module ((guix licenses)
                :prefix license:)
  #:use-module (guix build-system cargo)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (px self)
  #:use-module (ice-9 match)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg))

(define* (make-vscode-release-asset version asset hash)
  (origin
    (method url-fetch)
    (uri (string-append "https://update.code.visualstudio.com/" version "/"
                        (match (or (%current-system)
                                   (%current-target-system))
                          ("x86_64-linux" "linux-x64")
                          ("aarch64-linux" "linux-arm64")
                          ("armhf-linux" "linux-armhf")) "/stable"))
    (sha256 (base32 hash))))

(define-public vscode
  (package
    (name "vscode")
    (version "1.108.1")
    (source
     #f)
    (build-system binary-build-system)
    (arguments
     `(#:patchelf-plan `(("opt/vscode/code"
                          ("glib" "at-spi2-core"
                            "libx11"
                            "dbus"
                            "gdk-pixbuf"
                            "gtk+"
                            "pango"
                            "fontconfig"
                            "cairo"
                            "libxcomposite"
                            "libxdamage"
                            "libxext"
                            "libxfixes"
                            "libxshmfence"
                            "libxkbfile"
                            "libxrandr"
                            "expat"
                            "libdrm"
                            "libxkbcommon"
                            "mesa"
                            "alsa-lib"
                            "cups"
                            "libxcb"
                            "nspr"
                            "gcc"
                            "zlib"
                            "gcc:lib"
                            "libsecret"))
                         ("opt/vscode/chrome_crashpad_handler" 
                          ("gcc:lib")))
       #:phases (modify-phases %standard-phases
                  (replace 'unpack
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (mkdir-p "opt/vscode")
                      (invoke "tar"
                              "--strip-components=1"
                              "-xvf"
                              (assoc-ref inputs "vscode")
                              "-C"
                              "opt/vscode") #t))
                  (add-after 'install 'add-desktop-file
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (iconpath (string-append out
                                        "/share/icons/hicolor/512x512/apps/"))
                             (desktoppath (string-append out
                                           "/share/applications/"))
                             (desktopdata (string-append "[Desktop Entry]\n"
                                           "Name=Visual Studio Code\n"
                                           "Comment=Code Editing. Redefined.\n"
                                           "GenericName=Text Editor\n"
                                           "Exec="
                                           out
                                           "/opt/vscode/bin/code\n"
                                           "Icon=vscode\n"
                                           "Type=Application\n"
                                           "StartupNotify=true\n"
                                           "StartupWMClass=Code\n"
                                           "Categories=TextEditor;Development;IDE;\n"
                                           "Actions=new-empty-window;\n"
                                           "Keywords=vscode;\n\n")))
                        (mkdir-p iconpath)
                        (invoke "cp"
                         "opt/vscode/resources/app/resources/linux/code.png"
                         (string-append iconpath "vscode.png"))
                        (mkdir-p desktoppath)
                        (with-output-to-file (string-append desktoppath
                                                            ,name ".desktop")
                          (lambda _
                            (format #t desktopdata))))
                      (invoke "rm"
                              (string-append %output "/environment-variables"))
                      (mkdir-p (string-append %output "/bin"))
                      (invoke "ln" "-s"
                              (string-append %output "/opt/vscode/bin/code")
                              (string-append %output "/bin/"))
                      (invoke "ln" "-s"
                              (string-append %output "/opt/vscode/chrome_crashpad_handler")
                              (string-append %output "/bin/")) #t))
                  (add-after 'install 'wrap-where-patchelf-does-not-work
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out")))
                        (wrap-program (string-append out "/opt/vscode/code")
                          `("FONTCONFIG_PATH" ":" prefix
                            (,(string-join (list (string-append (assoc-ref
                                                                 inputs
                                                                 "fontconfig")
                                                                "/etc/fonts")
                                                 out) ":"))))
                        (wrap-program (string-append out "/opt/vscode/code")
                          `("LD_LIBRARY_PATH" ":" prefix
                            (,(string-join (list (string-append (assoc-ref
                                                                 inputs "nss")
                                                                "/lib/nss")
                                                 (string-append (assoc-ref
                                                                 inputs
                                                                 "eudev")
                                                                "/lib")
                                                 (string-append (assoc-ref
                                                                 inputs "gcc")
                                                                "/lib")
                                                 (string-append (assoc-ref
                                                                 inputs
                                                                 "libxkbfile")
                                                                "/lib")
                                                 (string-append (assoc-ref
                                                                 inputs "zlib")
                                                                "/lib")
                                                 (string-append (assoc-ref
                                                                 inputs
                                                                 "libsecret")
                                                                "/lib")
                                                 (string-append out
                                                                "/opt/vscode")
                                                 out) ":"))))
                        (wrap-program (string-append out "/opt/vscode/chrome_crashpad_handler")
                          `("FONTCONFIG_PATH" ":" prefix
                            (,(string-join (list (string-append (assoc-ref
                                                                 inputs
                                                                 "fontconfig")
                                                                "/etc/fonts")
                                                 out) ":"))))
                        (wrap-program (string-append out "/opt/vscode/chrome_crashpad_handler")
                          `("LD_LIBRARY_PATH" ":" prefix
                            (,(string-join (list (string-append (assoc-ref
                                                                 inputs "nss")
                                                                "/lib/nss")
                                                 (string-append (assoc-ref
                                                                 inputs
                                                                 "eudev")
                                                                "/lib")
                                                 (string-append (assoc-ref
                                                                 inputs "gcc")
                                                                "/lib")
                                                 (string-append (assoc-ref
                                                                 inputs
                                                                 "libxkbfile")
                                                                "/lib")
                                                 (string-append (assoc-ref
                                                                 inputs "zlib")
                                                                "/lib")
                                                 (string-append (assoc-ref
                                                                 inputs
                                                                 "libsecret")
                                                                "/lib")
                                                 (string-append out
                                                                "/opt/chrome_crashpad_handler")
                                                 out) ":"))))) #t)))))
    (native-inputs `(("tar" ,tar)))
    (inputs `(("vscode" ,(make-vscode-release-asset version "vscode"
                          "1dsvrf384qy3jfkwgkc7l0z3kyk17gw3v0rcb2gkx832k64n32x9"))
              ("gcc:lib" ,gcc "lib")
              ("gcc" ,gcc "lib")
              ("glib" ,glib)
              ("nss" ,nss)
              ("nspr" ,nspr)
              ("libx11" ,libx11)
              ("dbus" ,dbus)
              ("librsvg" ,librsvg)
              ("gtk+" ,gtk+)
              ("pango" ,pango)
              ("cairo" ,cairo)
              ("libxcomposite" ,libxcomposite)
              ("libxdamage" ,libxdamage)
              ("libxext" ,libxext)
              ("libxfixes" ,libxfixes)
              ("libxrandr" ,libxrandr)
              ("libxkbfile" ,libxkbfile)
              ("libxshmfence" ,libxshmfence)
              ("libsecret" ,libsecret)
              ("expat" ,expat)
              ("libdrm" ,libdrm)
              ("libxkbcommon" ,libxkbcommon)
              ("mesa" ,mesa)
              ("alsa-lib" ,alsa-lib)
              ("cups" ,cups)
              ("at-spi2-core" ,at-spi2-core)
              ("libxcb" ,libxcb)
              ("eudev" ,eudev)
              ("fontconfig" ,fontconfig)
              ("zlib" ,zlib)
              ("bash-minimal" ,bash-minimal)))
    (home-page "https://code.visualstudio.com/")
    (synopsis
     "Code editor written in TypeScript and Electron")
    (description
     "Visual Studio Code is a code editor redefined and optimized for
building and debugging modern web and cloud applications.")
    (license license:expat)))

(define-public cursor
  (package
    (name "cursor")
    (version "2.3.15")
    (source
     (origin
       (method url-fetch)
       (uri "https://downloads.cursor.com/production/bb2dbaacf30bb7eb9fd48a37812a8f326defa533/linux/x64/deb/amd64/deb/cursor_2.3.15_amd64.deb")
       (sha256 (base32 "1830yjcwfiiqsi8wzvcbwb3nqz47lih2mqjfdk7hm3kz7lbvidq4"))))
    (build-system binary-build-system)
    (arguments
     `(#:patchelf-plan `(("usr/share/cursor/cursor"
                          ("glib" "at-spi2-core"
                           "libx11"
                           "dbus"
                           "gdk-pixbuf"
                           "gtk+"
                           "pango"
                           "fontconfig"
                           "cairo"
                           "libxcomposite"
                           "libxdamage"
                           "libxext"
                           "libxfixes"
                           "libxshmfence"
                           "libxkbfile"
                           "libxrandr"
                           "expat"
                           "libdrm"
                           "libxkbcommon"
                           "mesa"
                           "alsa-lib"
                           "cups"
                           "libxcb"
                           "nspr"
                           "gcc"
                           "zlib"
                           "gcc:lib"
                           "libsecret"))
                         ("usr/share/cursor/chrome_crashpad_handler"
                          ("gcc:lib")))
       #:phases (modify-phases %standard-phases
                  (replace 'unpack
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (invoke "ar" "x" (assoc-ref inputs "source"))
                      (invoke "tar" "-xJf" "data.tar.xz")
                      (invoke "rm" "-rf" "control.tar.xz" "data.tar.xz" "debian-binary")
                      #t))
                  (add-after 'install 'add-desktop-file
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (iconpath (string-append out
                                                      "/share/icons/hicolor/512x512/apps/"))
                             (desktoppath (string-append out
                                                         "/share/applications/"))
                             (desktopdata (string-append "[Desktop Entry]\n"
                                                         "Name=Cursor\n"
                                                         "Comment=The AI Code Editor\n"
                                                         "GenericName=Text Editor\n"
                                                         "Exec="
                                                         out
                                                         "/usr/share/cursor/bin/cursor\n"
                                                         "Icon=cursor\n"
                                                         "Type=Application\n"
                                                         "StartupNotify=true\n"
                                                         "StartupWMClass=Cursor\n"
                                                         "Categories=TextEditor;Development;IDE;\n"
                                                         "Actions=new-empty-window;\n"
                                                         "Keywords=cursor;\n\n")))
                        (mkdir-p iconpath)
                        (invoke "cp"
                                "usr/share/pixmaps/co.anysphere.cursor.png"
                                (string-append iconpath "cursor.png"))
                        (mkdir-p desktoppath)
                        (with-output-to-file (string-append desktoppath
                                                            ,name ".desktop")
                          (lambda _
                            (format #t desktopdata))))
                      (invoke "rm"
                              (string-append %output "/environment-variables"))
                      (mkdir-p (string-append %output "/bin"))
                      (invoke "ln" "-s"
                              (string-append %output "/usr/share/cursor/bin/cursor")
                              (string-append %output "/bin/cursor"))
                      (invoke "ln" "-s"
                              (string-append %output "/usr/share/cursor/chrome_crashpad_handler")
                              (string-append %output "/bin/"))
                      #t))
                  (add-after 'install 'wrap-where-patchelf-does-not-work
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out")))
                        (wrap-program (string-append out "/usr/share/cursor/cursor")
                          `("FONTCONFIG_PATH" ":" prefix
                            (,(string-join (list (string-append (assoc-ref
                                                                 inputs
                                                                 "fontconfig")
                                                                "/etc/fonts")
                                                 out) ":"))))
                        (wrap-program (string-append out "/usr/share/cursor/cursor")
                          `("LD_LIBRARY_PATH" ":" prefix
                            (,(string-join (list (string-append (assoc-ref
                                                                 inputs "nss")
                                                                "/lib/nss")
                                                 (string-append (assoc-ref
                                                                 inputs
                                                                 "eudev")
                                                                "/lib")
                                                 (string-append (assoc-ref
                                                                 inputs "gcc")
                                                                "/lib")
                                                 (string-append (assoc-ref
                                                                 inputs
                                                                 "libxkbfile")
                                                                "/lib")
                                                 (string-append (assoc-ref
                                                                 inputs "zlib")
                                                                "/lib")
                                                 (string-append (assoc-ref
                                                                 inputs
                                                                 "libsecret")
                                                                "/lib")
                                                 (string-append out
                                                                "/usr/share/cursor")
                                                 out) ":"))))
                        (wrap-program (string-append out "/usr/share/cursor/chrome_crashpad_handler")
                          `("FONTCONFIG_PATH" ":" prefix
                            (,(string-join (list (string-append (assoc-ref
                                                                 inputs
                                                                 "fontconfig")
                                                                "/etc/fonts")
                                                 out) ":"))))
                        (wrap-program (string-append out "/usr/share/cursor/chrome_crashpad_handler")
                          `("LD_LIBRARY_PATH" ":" prefix
                            (,(string-join (list (string-append (assoc-ref
                                                                 inputs "nss")
                                                                "/lib/nss")
                                                 (string-append (assoc-ref
                                                                 inputs
                                                                 "eudev")
                                                                "/lib")
                                                 (string-append (assoc-ref
                                                                 inputs "gcc")
                                                                "/lib")
                                                 (string-append (assoc-ref
                                                                 inputs
                                                                 "libxkbfile")
                                                                "/lib")
                                                 (string-append (assoc-ref
                                                                 inputs "zlib")
                                                                "/lib")
                                                 (string-append (assoc-ref
                                                                 inputs
                                                                 "libsecret")
                                                                "/lib")
                                                 (string-append out
                                                                "/usr/share/cursor")
                                                 out) ":"))))
                        #t))))))
    (native-inputs `(("tar" ,tar)
                     ("binutils" ,binutils)))
    (inputs `(("gcc:lib" ,gcc "lib")
              ("gcc" ,gcc "lib")
              ("glib" ,glib)
              ("nss" ,nss)
              ("nspr" ,nspr)
              ("libx11" ,libx11)
              ("dbus" ,dbus)
              ("librsvg" ,librsvg)
              ("gtk+" ,gtk+)
              ("pango" ,pango)
              ("cairo" ,cairo)
              ("libxcomposite" ,libxcomposite)
              ("libxdamage" ,libxdamage)
              ("libxext" ,libxext)
              ("libxfixes" ,libxfixes)
              ("libxrandr" ,libxrandr)
              ("libxkbfile" ,libxkbfile)
              ("libxshmfence" ,libxshmfence)
              ("libsecret" ,libsecret)
              ("expat" ,expat)
              ("libdrm" ,libdrm)
              ("libxkbcommon" ,libxkbcommon)
              ("mesa" ,mesa)
              ("alsa-lib" ,alsa-lib)
              ("cups" ,cups)
              ("at-spi2-core" ,at-spi2-core)
              ("libxcb" ,libxcb)
              ("eudev" ,eudev)
              ("fontconfig" ,fontconfig)
              ("zlib" ,zlib)
              ("bash-minimal" ,bash-minimal)))
    (home-page "https://www.cursor.com/")
    (synopsis "AI-powered code editor")
    (description
     "Cursor is an AI-powered code editor built on VS Code that enhances
developer productivity through artificial intelligence integration.  It
features an AI agent capable of converting ideas into functional code,
predictive code completion, and integrations with development workflows.")
    (license license:expat)))

;; Known issues: https://wiki.nixos.org/wiki/Zed
(define-public zed
  (package
    (name "zed")
    (version "0.220.2")
    (source #f)
    (build-system binary-build-system)
    (arguments
     `(#:patchelf-plan
       '(("opt/zed/bin/zed"
          ("gcc:lib"))
         ("opt/zed/libexec/zed-editor"
          ("gcc:lib" "glib" "gtk+" "libx11" "libxcb" "libxkbcommon"
           "fontconfig" "freetype" "mesa" "vulkan-loader" "alsa-lib"
           "zlib" "libxcursor" "libxi" "wayland" "sqlite")))
       #:phases
       (modify-phases %standard-phases
         (replace 'unpack
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (mkdir-p "opt/zed")
             (invoke "tar"
                     "--strip-components=1"
                     "-xvf"
                     (assoc-ref inputs "zed")
                     "-C"
                     "opt/zed")
             #t))
         (add-after 'install 'create-wrapper
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (zed-cli (string-append out "/opt/zed/bin/zed"))
                    (zed-editor (string-append out "/opt/zed/libexec/zed-editor")))
               ;; Fix .desktop file
               (substitute* (string-append out "/opt/zed/share/applications/zed.desktop")
                 (("Exec=zed") (string-append "Exec=" out "/bin/zed"))
                 (("Icon=zed") (string-append "Icon=" out "/opt/zed/share/icons/hicolor/512x512/apps/zed.png")))
               ;; Create bin directory and wrapper
               (mkdir-p (string-append out "/bin"))
               (mkdir-p (string-append out "/share/applications"))
               (mkdir-p (string-append out "/share/icons"))
               ;; Wrap the editor binary
               (wrap-program zed-editor
                 `("ZED_UPDATE_EXPLANATION" = ("Please use Guix to update Zed."))
                 `("XKB_CONFIG_ROOT" ":" prefix
                   (,(string-append (assoc-ref inputs "xkeyboard-config") "/share/X11/xkb")))
                 `("LD_LIBRARY_PATH" ":" prefix
                   (,(string-append out "/opt/zed/lib")
                    ,(string-append (assoc-ref inputs "vulkan-loader") "/lib")
                    ,(string-append (assoc-ref inputs "wayland") "/lib")
                    ,(string-append (assoc-ref inputs "libxkbcommon") "/lib"))))
               ;; Symlinks: zed -> CLI, zed-editor -> editor
               (symlink zed-cli (string-append out "/bin/zed"))
               (symlink zed-editor (string-append out "/bin/zed-editor"))
               (symlink (string-append out "/opt/zed/share/applications/zed.desktop")
                        (string-append out "/share/applications/zed.desktop"))
               (symlink (string-append out "/opt/zed/share/icons/hicolor")
                        (string-append out "/share/icons/hicolor")))
             #t)))))
    (native-inputs `(("tar" ,tar)))
    (inputs `(("zed"
               ,(origin
                  (method url-fetch)
                  (uri (string-append
                        "https://github.com/zed-industries/zed/releases/download/v"
                        version "/zed-linux-x86_64.tar.gz"))
                  (sha256
                   (base32 "110gi6f63m8cs8mg3fvigpzh90n55a5hvhv1iwmn3cbsdnrbprk5"))))
              ("bash-minimal" ,bash-minimal)
              ("glib" ,glib)
              ("gtk+" ,gtk+)
              ("libx11" ,libx11)
              ("libxcb" ,libxcb)
              ("libxcursor" ,libxcursor)
              ("libxi" ,libxi)
              ("libxkbcommon" ,libxkbcommon)
              ("fontconfig" ,fontconfig)
              ("freetype" ,freetype)
              ("mesa" ,mesa)
              ("vulkan-loader" ,vulkan-loader)
              ("alsa-lib" ,alsa-lib)
              ("zlib" ,zlib)
              ("wayland" ,wayland)
              ("sqlite" ,sqlite)
              ("xkeyboard-config" ,xkeyboard-config)
              ("gcc:lib" ,gcc "lib")))
    (supported-systems '("x86_64-linux"))
    (home-page "https://zed.dev/")
    (synopsis "High-performance multiplayer code editor")
    (description
     "Zed is a high-performance, multiplayer code editor from the creators
of Atom and Tree-sitter.  It features a fast GPU-accelerated UI, built-in
collaboration tools, and AI-powered code assistance.")
    (license license:gpl3+)))

(define-public package-version-server
  (package
    (name "package-version-server")
    (version "0.0.10")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/zed-industries/package-version-server/archive/refs/tags/v"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "07b43fmi3zifilszzz6s343ggag4y3q5rlzfv753y8l177jqm9qf"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f
       #:tests? #f))
    (native-inputs (list pkg-config))
    (inputs (cons* openssl (px-cargo-inputs 'package-version-server)))
    (home-page "https://github.com/zed-industries/package-version-server")
    (synopsis "Language server for package.json version information")
    (description
     "Package Version Server is a language server that provides package version
information when hovering over dependency keys in @file{package.json} files.
It displays the current version, latest available version, and release date
for npm packages.")
    (license license:expat)))

(define-public wakatime-ls
  (package
    (name "wakatime-ls")
    (version "0.1.10")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/wakatime/zed-wakatime/archive/refs/tags/v"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0f4j4w4knggqwkkgnv9zhwi867vgpn5wmgbash08mjh84bv5g0i1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f
       #:cargo-build-flags '("--release" "-p" "wakatime-ls")
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((bin (string-append (assoc-ref outputs "out") "/bin")))
               (mkdir-p bin)
               (install-file "target/release/wakatime-ls" bin)))))))
    (inputs (px-cargo-inputs 'wakatime-ls))
    (home-page "https://github.com/wakatime/zed-wakatime")
    (synopsis "WakaTime language server for automatic time tracking")
    (description
     "WakaTime Language Server is a language server that sends heartbeats to
WakaTime for automatic time tracking of programming activity.  It works with
editors that support the Language Server Protocol.")
    (license license:expat)))

(define-public antigravity
  (package
    (name "antigravity")
    (version "1.11.9-4787439284912128")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://edgedl.me.gvt1.com/edgedl/release2/j0qc3/antigravity/stable/"
             version "/linux-x64/Antigravity.tar.gz"))
       (sha256
        (base32
         "0ba79s00ijp1bm44kkk578frc2mbjbl715ijpipm5330v9hllfhr"))))
    (build-system binary-build-system)
    (arguments
     `(#:patchelf-plan `(("Antigravity/antigravity"
                          ("glib"
                           "libx11"
                           "dbus"
                           "gdk-pixbuf"
                           "gtk+"
                           "pango"
                           "fontconfig"
                           "cairo"
                           "libxcomposite"
                           "libxdamage"
                           "libxext"
                           "libxfixes"
                           "libxshmfence"
                           "libxkbfile"
                           "libxrandr"
                           "expat"
                           "libdrm"
                           "libxkbcommon"
                           "mesa"
                           "alsa-lib"
                           "cups"
                           "at-spi2-core"
                           "libxcb"
                           "nspr"
                           "gcc"
                           "zlib"
                           "gcc:lib"
                           "libsecret"))
                         ("Antigravity/chrome_crashpad_handler"
                          ("gcc:lib"))
                         ("Antigravity/resources/app/extensions/antigravity/bin/language_server_linux_x64"
                          ()))
       #:phases (modify-phases %standard-phases
                  (replace 'unpack
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (invoke "tar" "xzf" (assoc-ref inputs "source"))
                      #t))
                  (add-after 'install 'add-desktop-file
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (iconpath (string-append out
                                        "/share/icons/hicolor/512x512/apps/"))
                             (desktoppath (string-append out
                                           "/share/applications/"))
                             (desktopdata (string-append "[Desktop Entry]\n"
                                           "Name=Google Antigravity\n"
                                           "Comment=Experience liftoff\n"
                                           "GenericName=IDE\n"
                                           "Exec="
                                           out
                                           "/Antigravity/bin/antigravity\n"
                                           "Icon=antigravity\n"
                                           "Type=Application\n"
                                           "StartupNotify=true\n"
                                           "StartupWMClass=Antigravity\n"
                                           "Categories=Development;IDE;\n"
                                           "Keywords=editor;ide;ai;\n\n")))
                        (mkdir-p iconpath)
                        (invoke "cp"
                         "Antigravity/resources/app/resources/linux/code.png"
                         (string-append iconpath "antigravity.png"))
                        (mkdir-p desktoppath)
                        (with-output-to-file (string-append desktoppath
                                                            ,name ".desktop")
                          (lambda _
                            (format #t desktopdata))))
                      (invoke "rm"
                              (string-append %output "/environment-variables"))
                      (mkdir-p (string-append %output "/bin"))
                      (invoke "ln" "-s"
                              (string-append %output "/Antigravity/bin/antigravity")
                              (string-append %output "/bin/antigravity")) #t))
                  (add-after 'install 'wrap-where-patchelf-does-not-work
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out")))
                        (wrap-program (string-append out "/Antigravity/antigravity")
                          `("FONTCONFIG_PATH" ":" prefix
                            (,(string-join (list (string-append (assoc-ref
                                                                 inputs
                                                                 "fontconfig")
                                                                "/etc/fonts")
                                                 out) ":"))))
                        (wrap-program (string-append out "/Antigravity/antigravity")
                          `("LD_LIBRARY_PATH" ":" prefix
                            (,(string-join (list (string-append (assoc-ref
                                                                 inputs "nss")
                                                                "/lib/nss")
                                                 (string-append (assoc-ref
                                                                 inputs
                                                                 "eudev")
                                                                "/lib")
                                                 (string-append (assoc-ref
                                                                 inputs "gcc")
                                                                "/lib")
                                                 (string-append (assoc-ref
                                                                 inputs
                                                                 "libxkbfile")
                                                                "/lib")
                                                 (string-append (assoc-ref
                                                                 inputs "zlib")
                                                                "/lib")
                                                 (string-append (assoc-ref
                                                                 inputs
                                                                 "libsecret")
                                                                "/lib")
                                                 (string-append out
                                                                "/Antigravity")
                                                 out) ":"))))
                        (wrap-program (string-append out "/Antigravity/chrome_crashpad_handler")
                          `("FONTCONFIG_PATH" ":" prefix
                            (,(string-join (list (string-append (assoc-ref
                                                                 inputs
                                                                 "fontconfig")
                                                                "/etc/fonts")
                                                 out) ":"))))
                        (wrap-program (string-append out "/Antigravity/chrome_crashpad_handler")
                          `("LD_LIBRARY_PATH" ":" prefix
                            (,(string-join (list (string-append (assoc-ref
                                                                 inputs "nss")
                                                                "/lib/nss")
                                                 (string-append (assoc-ref
                                                                 inputs
                                                                 "eudev")
                                                                "/lib")
                                                 (string-append (assoc-ref
                                                                 inputs "gcc")
                                                                "/lib")
                                                 (string-append (assoc-ref
                                                                 inputs
                                                                 "libxkbfile")
                                                                "/lib")
                                                 (string-append (assoc-ref
                                                                 inputs "zlib")
                                                                "/lib")
                                                 (string-append (assoc-ref
                                                                 inputs
                                                                 "libsecret")
                                                                "/lib")
                                                 (string-append out
                                                                "/Antigravity")
                                                 out) ":"))))) #t)))))
    (native-inputs `(("tar" ,tar)))
    (inputs `(("gcc:lib" ,gcc "lib")
              ("gcc" ,gcc "lib")
              ("glib" ,glib)
              ("nss" ,nss)
              ("nspr" ,nspr)
              ("libx11" ,libx11)
              ("dbus" ,dbus)
              ("librsvg" ,librsvg)
              ("gtk+" ,gtk+)
              ("pango" ,pango)
              ("cairo" ,cairo)
              ("libxcomposite" ,libxcomposite)
              ("libxdamage" ,libxdamage)
              ("libxext" ,libxext)
              ("libxfixes" ,libxfixes)
              ("libxrandr" ,libxrandr)
              ("libxkbfile" ,libxkbfile)
              ("libxshmfence" ,libxshmfence)
              ("libsecret" ,libsecret)
              ("expat" ,expat)
              ("libdrm" ,libdrm)
              ("libxkbcommon" ,libxkbcommon)
              ("mesa" ,mesa)
              ("alsa-lib" ,alsa-lib)
              ("cups" ,cups)
              ("at-spi2-core" ,at-spi2-core)
              ("libxcb" ,libxcb)
              ("eudev" ,eudev)
              ("fontconfig" ,fontconfig)
              ("zlib" ,zlib)
              ("bash-minimal" ,bash-minimal)))
    (home-page "https://antigravity.google")
    (synopsis
     "AI-powered IDE from Google with Gemini integration")
    (description
     "Google Antigravity is a next-generation AI-powered IDE built on Electron,
featuring deep integration with Gemini 3 for agentic development workflows,
intelligent code generation, and collaborative AI assistance.")
    (license license:expat)))
