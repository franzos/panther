;;; Package Repository for GNU Guix
;;; Copyright © 2021-2023 Reza Alizadeh Majd <r.majd@pantherx.org>
;;; Copyright © 2021-2025 Franz Geffke <mail@gofranz.com>

(define-module (px packages desktop-tools)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module ((nonguix licenses)
                #:prefix nonfree:)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system go)
  #:use-module (guix build-system qt)
  #:use-module (guix build-system copy)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (nonguix build-system binary)
  #:use-module (nonguix build-system chromium-binary)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages base)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image-processing)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnuzilla)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages lxqt)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages mate)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages search)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages video)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xfce)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages kde-plasma)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xml)
  #:use-module (px packages qt)
  #:use-module (px packages common)
  #:use-module (px packages library)
  #:use-module (px packages haskell)
  #:use-module (px packages themes)
  #:use-module ((px packages maths) #:select (libqalculate) #:prefix px:)
  #:use-module (px self)
  #:use-module (srfi srfi-1))

(define-public albert-launcher
  (package
    (name "albert-launcher")
    (version "34.0.10")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/albertlauncher/albert")
             (commit (string-append "v" version))
             (recursive? #t)))
       (sha256
        (base32 "0x528y9x0lq5v8sd53yip4rwqihccpfpkf76i2yi4pylhbrfya27"))
       (file-name (git-file-name name version))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f
      #:configure-flags #~(list "-DBUILD_TESTS=OFF")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'set-plugin-path
            (lambda _
              ;; Add output lib/albert directory to plugin search path
              (substitute* "src/app/qtpluginprovider.cpp"
                (("paths << \"../lib\";")
                 (string-append "paths << \"" #$output "/lib/albert\";\n"
                                "    paths << \"../lib\";")))))
          (add-after 'install 'wrap-albert
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (bin (string-append out "/bin/albert"))
                     (qt (assoc-ref inputs "qtbase")))
                (wrap-program bin
                  `("QT_PLUGIN_PATH" ":" prefix
                    (,(string-append qt "/lib/qt6/plugins"))))))))))
    (native-inputs (list git-minimal libxml2 pkg-config qttools))
    (inputs (list bash-minimal
                  gmp
                  libarchive
                  libqalculate
                  libx11
                  mpfr
                  muparser
                  python
                  qcoro-qt6
                  qtbase
                  qtdeclarative
                  qtkeychain-qt6
                  qtscxml
                  qtsvg))
    (home-page "https://albertlauncher.github.io/")
    (synopsis "Fast and flexible keyboard launcher")
    (description
     "Albert is a desktop agnostic, plugin-based keyboard launcher.  Its goals
are usability and beauty, performance and extensibility.  It is written in C++
and based on the Qt6 framework.")
    (license license:gpl3+)))

(define-public qlipper
  (package
    (name "qlipper")
    (version "6.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/pvanek/qlipper/archive/"
                           version ".tar.gz"))
       (sha256
        (base32 "13yfh9sbqnf10vfjp86b9kdb6vf8jp5pfvcv82p9gkfdc1hah6kd"))))
    (build-system qt-build-system)
    (arguments
     (list #:qtbase qtbase
           #:tests? #f))
    (native-inputs (list qttools))
    (inputs (list kguiaddons libx11 qtbase))
    (home-page "https://github.com/pvanek/qlipper")
    (synopsis "Lightweight and cross-platform clipboard history applet.")
    (description "Lightweight and cross-platform clipboard history applet.")
    (license license:gpl2+)))

(define-public cpputilities
  (package
    (name "cpputilities")
    (version "5.34.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/Martchus/cpp-utilities/archive/refs/tags/v"
             version ".tar.gz"))
       (sha256
        (base32 "13gz84i4s6w46y4zlnhfrjzc47p32yibg97v081nn10jjvwn6fnz"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f
       #:configure-flags '("-DCMAKE_VERBOSE_MAKEFILE=ON"
                           "-DBUILD_VIRTUALBOX=OFF"
                           "-DCMAKE_INSTALL_LIBDIR=libs")))
    (home-page "https://github.com/Martchus/cpp-utilities/")
    (synopsis "Useful C++ classes and routines")
    (description "Useful C++ classes and routines such as
argument parser, IO and conversion utilities.")
    (license license:gpl2+)))

(define-public fork-awesome
  (package
    (name "fork-awesome")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/ForkAwesome/Fork-Awesome/archive/refs/tags/"
             version ".tar.gz"))
       (sha256
        (base32 "1cxxbyklk139cj7hw9jiq51cmmqgn74z8ysl9i0y017jj7qsbyr3"))))
    (build-system copy-build-system)
    (arguments
     '(#:install-plan '(("fonts" "share/")
                        ("src" "share/"))))
    (home-page "https://github.com/ForkAwesome/Fork-Awesome")
    (synopsis "A fork of the iconic font and CSS toolkit ")
    (description
     "Fork Awesome is a suite of 796 pictographic and
brand icons for easy, scalable vector graphics on websites and beyond.")
    (license license:expat)))

(define-public qtforkawesome
  (package
    (name "qtforkawesome")
    (version "0.3.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/Martchus/qtforkawesome/archive/refs/tags/v"
             version ".tar.gz"))
       (sha256
        (base32 "0fzji348vfb384ada8687slrwldxz5g3swwnq9p16jml138284q0"))))
    (build-system cmake-build-system)
    (native-inputs (list qtutilities
                         cpputilities
                         qtbase
                         qtdeclarative
                         perl
                         perl-yaml
                         fork-awesome))
    (arguments
     `(#:tests? #f
       #:configure-flags ,#~(list "-DQT_PACKAGE_PREFIX=Qt6"
                                  (string-append "-DFORK_AWESOME_FONT_FILE="
                                   #$(this-package-native-input "fork-awesome")
                                   "/share/fonts/forkawesome-webfont.woff2")
                                  (string-append
                                   "-DFORK_AWESOME_ICON_DEFINITIONS="
                                   #$(this-package-native-input "fork-awesome")
                                   "/share/src/icons/icons.yml"))))
    (home-page "https://github.com/Martchus/qtforkawesome/")
    (synopsis
     "Useful C++ classes and routines such as argument parser, IO and conversion utilities.")
    (description
     "Useful C++ classes and routines such as argument parser, IO and conversion utilities.")
    (license license:gpl2+)))

(define-public qxkb
  (package
    (name "qxkb")
    (version "0.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/thegala/qxkb/archive/refs/tags/" name "-"
             version ".tar.gz"))
       (sha256
        (base32 "1nprswfdnqmy6xs6pdkzy6c3xkzh79zifdvy4vpw4l41gnqrl94s"))))
    (build-system qt-build-system)
    (inputs (list libxkbfile qtbase-5 qtsvg-5 qtx11extras))
    (native-inputs (list qttools-5))
    (arguments
     (list
      #:tests? #f)) ;no upstream tests
    (home-page "https://github.com/thegala/qxkb")
    (synopsis "Keyboard layout switcher")
    (description "Keyboard layout switcher")
    (license license:gpl2+)))

(define-public syncthingtray
  (package
    (name "syncthingtray")
    (version "2.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/Martchus/syncthingtray/archive/refs/tags/v"
             version ".tar.gz"))
       (sha256
        (base32 "0vmfzgy3zd5v7a9q99r0ra68jakfqdnbyfrmpgs8jvpmd4a1sz0w"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f
       ;; The Plasmoid's Qt Quick GUI calls qt_policy, a Qt6-only CMake
       ;; command, which fails under this Qt5 build.
       #:configure-flags '("-DNO_PLASMOID=ON")
       #:phases (modify-phases %standard-phases
                  (replace 'build
                    (lambda _
                      (invoke "make" "-j" "1") #t))
                  (add-after 'install 'wrap
                    ;; The program fails to find the QtWebEngineProcess program,
                    ;; so we set QTWEBENGINEPROCESS_PATH to help it.
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let ((bin (string-append (assoc-ref outputs "out")
                                                "/bin"))
                            (qtwebengineprocess (string-append (assoc-ref
                                                                inputs
                                                                "qtwebengine")
                                                 "/lib/qt5/libexec/QtWebEngineProcess")))
                        (for-each (lambda (program)
                                    (wrap-program program
                                      `("QTWEBENGINEPROCESS_PATH" =
                                        (,qtwebengineprocess))))
                                  (find-files bin ".*"))) #t)))))
    (native-inputs (list extra-cmake-modules qttools-5))
    (inputs `(("qtbase" ,qtbase-5)
              ("qtquickcontrols2" ,qtquickcontrols2-5)
              ("qtutilities" ,qtutilities)
              ("boost" ,boost)
              ("qtdeclarative" ,qtdeclarative-5)
              ("qtsvg" ,qtsvg-5)
              ("qtwebchannel-5" ,qtwebchannel-5)
              ("qtwebengine" ,qtwebengine-5)
              ("plasma-framework" ,plasma-framework)
              ("kwindowsystem" ,kwindowsystem-5)
              ("kio" ,kio-5)
              ("cppunit" ,cppunit)
              ("cpputilities" ,cpputilities)
              ("qtforkawesome" ,qtforkawesome)
              ("bash-minimal" ,bash-minimal)))
    (home-page "https://github.com/Martchus/syncthingtray")
    (synopsis "Qt-based tray application")
    (description "Qt-based tray application")
    (license license:gpl2+)))

(define-public slack-desktop
  (package
    (name "slack-desktop")
    (version "4.47.69")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://downloads.slack-edge.com/desktop-releases/linux/x64/"
         version "/slack-desktop-" version "-amd64.deb"))
       (sha256
        (base32 "19bbj3lk9vwqgjabsgisjldsxwwq3na7525vvijyfs59kq3y7mbv"))))
    (supported-systems '("x86_64-linux"))
    (build-system chromium-binary-build-system)
    (arguments
     (list #:validate-runpath? #f
           #:wrapper-plan
           #~'(("lib/slack/slack" (("out" "/lib/slack")))
               "lib/slack/chrome_crashpad_handler")
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'binary-unpack 'setup-cwd
                 (lambda _
                   (copy-recursively "usr/" ".")
                   (delete-file-recursively "usr")
                   (delete-file-recursively "etc")
                   (delete-file-recursively "bin")
                   (substitute* '("share/applications/slack.desktop")
                     (("/usr/bin/slack") (string-append #$output "/bin/slack")))))
               (add-after 'install 'symlink-binary-file
                 (lambda _
                   (mkdir-p (string-append #$output "/bin"))
                   (symlink (string-append #$output "/lib/slack/slack")
                            (string-append #$output "/bin/slack")))))))
    (home-page "https://slack.com/")
    (synopsis "Team collaboration and messaging platform")
    (description "Slack Desktop is an Electron-based application for team
communication and collaboration.  It provides messaging, file sharing, and
integration with various productivity tools.")
    (license (nonfree:nonfree "https://slack.com/terms-of-service"))))

(define-public discord
  (package
    (name "discord")
    (version "0.0.129")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://stable.dl2.discordapp.net/apps/linux/" version
         "/discord-" version ".deb"))
       (sha256
        (base32 "08dk2h3sqabnsy53izn06k29bg4mz5q1gf6f3xwkh4y8622ns0rd"))))
    (supported-systems '("x86_64-linux"))
    (build-system chromium-binary-build-system)
    (arguments
     (list #:validate-runpath? #f
           #:wrapper-plan
           #~'(("share/discord/Discord" (("out" "/share/discord")))
               "share/discord/chrome_crashpad_handler")
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'binary-unpack 'setup-cwd
                 (lambda _
                   (copy-recursively "usr/" ".")
                   (delete-file-recursively "usr")
                   (delete-file-recursively "bin")
                   (substitute* '("share/discord/discord.desktop")
                     (("/usr/share/discord/Discord")
                      (string-append #$output "/bin/discord")))))
               (add-after 'install 'symlink-binary-file
                 (lambda _
                   (mkdir-p (string-append #$output "/bin"))
                   (symlink (string-append #$output "/share/discord/Discord")
                            (string-append #$output "/bin/discord")))))))
    (home-page "https://discord.com/")
    (synopsis "Voice and text chat for gamers")
    (description "Discord is an all-in-one voice and text chat application for
gamers that works on desktop and phone.  It features voice chat, text chat,
and rich media support for gaming communities.")
    (license (nonfree:nonfree "https://discord.com/terms"))))

(define-public appflowy
  (package
    (name "appflowy")
    (version "0.12.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/AppFlowy-IO/AppFlowy/releases/download/"
             version "/AppFlowy-" version "-linux-x86_64.deb"))
       (sha256
        (base32 "0x6a0rmvk8qnqpmwfx3bb1h6357phk6jxrm7xg1bckjrbi9j65q1"))))
    (supported-systems '("x86_64-linux"))
    (build-system binary-build-system)
    (arguments
     `(#:validate-runpath? #f
       #:patchelf-plan
       '(("lib/AppFlowy/AppFlowy"
          ("glib" "gtk+" "pango" "harfbuzz" "atk" "cairo" "gdk-pixbuf"
           "fontconfig" "libepoxy" "gcc:lib"))
         ("lib/AppFlowy/lib/libflutter_linux_gtk.so"
          ("glib" "gtk+" "pango" "atk" "fontconfig" "libepoxy" "gcc:lib")))
       #:install-plan
       '(("lib" "lib")
         ("share" "share"))
       #:phases
       (modify-phases %standard-phases
         (replace 'unpack
           (lambda* (#:key inputs #:allow-other-keys)
             (invoke "ar" "x" (assoc-ref inputs "source"))
             (invoke "tar" "-xf" (car (find-files "." "^data\\.tar\\.")))
             (copy-recursively "usr/" ".")
             (delete-file-recursively "usr")))
         (add-after 'install 'fix-desktop-file
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute* (string-append out "/share/applications/AppFlowy.desktop")
                 (("Exec=AppFlowy")
                  (string-append "Exec=" out "/bin/appflowy"))
                 (("Icon=.*")
                  (string-append "Icon=" out "/lib/AppFlowy/data/flutter_assets/assets/images/flowy_logo.svg\n"))))))
         (add-after 'install 'create-wrapper
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (lib-path (string-join
                               (list (string-append out "/lib/AppFlowy/lib")
                                     (string-append (assoc-ref inputs "gstreamer") "/lib")
                                     (string-append (assoc-ref inputs "gst-plugins-base") "/lib")
                                     (string-append (assoc-ref inputs "keybinder") "/lib")
                                     (string-append (assoc-ref inputs "libnotify") "/lib")
                                     (string-append (assoc-ref inputs "libepoxy") "/lib")
                                     (string-append (assoc-ref inputs "xz") "/lib")
                                     (string-append (assoc-ref inputs "libva") "/lib")
                                     (string-append (assoc-ref inputs "libvdpau") "/lib")
                                     (string-append (assoc-ref inputs "libdrm") "/lib")
                                     (string-append (assoc-ref inputs "gnutls") "/lib")
                                     (string-append (assoc-ref inputs "lcms") "/lib")
                                     (string-append (assoc-ref inputs "libarchive") "/lib")
                                     (string-append (assoc-ref inputs "alsa-lib") "/lib")
                                     (string-append (assoc-ref inputs "pulseaudio") "/lib")
                                     (string-append (assoc-ref inputs "mesa") "/lib")
                                     (string-append (assoc-ref inputs "libxscrnsaver") "/lib")
                                     (string-append (assoc-ref inputs "libxv") "/lib"))
                               ":"))
                    (bash (string-append (assoc-ref inputs "bash-minimal") "/bin/bash")))
               (mkdir-p bin)
               (call-with-output-file (string-append bin "/appflowy")
                 (lambda (port)
                   (format port "#!~a~%export LD_LIBRARY_PATH=\"~a:$LD_LIBRARY_PATH\"~%export GDK_BACKEND=x11~%exec ~a/lib/AppFlowy/AppFlowy \"$@\"~%"
                           bash lib-path out)))
               (chmod (string-append bin "/appflowy") #o755)))))))
    (native-inputs `(("binutils" ,binutils)))
    (inputs `(("alsa-lib" ,alsa-lib)
              ("atk" ,at-spi2-core)
              ("bash-minimal" ,bash-minimal)
              ("cairo" ,cairo)
              ("fontconfig" ,fontconfig)
              ("gcc:lib" ,gcc "lib")
              ("gdk-pixbuf" ,gdk-pixbuf)
              ("glib" ,glib)
              ("gnutls" ,gnutls)
              ("gst-plugins-base" ,gst-plugins-base)
              ("gstreamer" ,gstreamer)
              ("gtk+" ,gtk+)
              ("keybinder" ,keybinder)
              ("lcms" ,lcms)
              ("libarchive" ,libarchive)
              ("libdrm" ,libdrm)
              ("libnotify" ,libnotify)
              ("mesa" ,mesa)
              ("harfbuzz" ,harfbuzz)
              ("libepoxy" ,libepoxy)
              ("libva" ,libva)
              ("libvdpau" ,libvdpau)
              ("libxscrnsaver" ,libxscrnsaver)
              ("libxv" ,libxv)
              ("pango" ,pango)
              ("pulseaudio" ,pulseaudio)
              ("xz" ,xz)))
    (home-page "https://appflowy.io/")
    (synopsis "Open-source alternative to Notion")
    (description
     "AppFlowy is an open-source alternative to Notion, providing a privacy-first
workspace for notes, tasks, and projects.  It features a rich text editor,
databases, kanban boards, and AI integration while keeping your data secure
and under your control.")
    (license license:agpl3+)))

(define-public rustdesk
  (package
    (name "rustdesk")
    (version "1.4.9")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/rustdesk/rustdesk/releases/download/"
             version "/rustdesk-" version "-x86_64.deb"))
       (file-name (string-append "rustdesk-" version "-x86_64.deb"))
       (sha256
        (base32 "18zx2bbg21h4ij6fg62cam3cwm3w8rcydysb0ir4300fqi3vli3j"))))
    (supported-systems '("x86_64-linux"))
    (build-system binary-build-system)
    (arguments
     `(#:validate-runpath? #f
       #:patchelf-plan
       '(("share/rustdesk/rustdesk"
          ("glib" "gtk+" "pango" "at-spi2-core" "cairo" "gdk-pixbuf" "gcc:lib"))
         ("share/rustdesk/lib/libflutter_linux_gtk.so"
          ("glib" "gtk+" "pango" "at-spi2-core" "fontconfig" "libepoxy" "gcc:lib"))
         ("share/rustdesk/lib/librustdesk.so"
          ("glib" "gtk+" "gdk-pixbuf" "cairo" "libx11" "libxfixes" "libxtst"
           "xdotool" "pulseaudio" "libxkbcommon" "libxcb" "gstreamer"
           "gst-plugins-base" "dbus" "linux-pam" "zlib" "gcc:lib")))
       #:install-plan
       '(("share" "share")
         ("etc" "etc"))
       #:phases
       (modify-phases %standard-phases
         (replace 'unpack
           (lambda* (#:key inputs #:allow-other-keys)
             (invoke "ar" "x" (assoc-ref inputs "source"))
             (invoke "tar" "-xf" "data.tar.xz")
             (copy-recursively "usr/" ".")
             (delete-file-recursively "usr")))
         (add-after 'install 'fix-desktop-file
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute* (string-append out "/share/applications/rustdesk.desktop")
                 (("Exec=rustdesk")
                  (string-append "Exec=" out "/bin/rustdesk"))
                 (("Icon=rustdesk")
                  (string-append "Icon=" out "/share/icons/hicolor/scalable/apps/rustdesk.svg")))
               (substitute* (string-append out "/share/applications/rustdesk-link.desktop")
                 (("Exec=rustdesk")
                  (string-append "Exec=" out "/bin/rustdesk"))))))
         (add-after 'install 'create-wrapper
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (lib-path (string-join
                               (list (string-append out "/share/rustdesk/lib")
                                     (string-append (assoc-ref inputs "gstreamer") "/lib")
                                     (string-append (assoc-ref inputs "gst-plugins-base") "/lib")
                                     (string-append (assoc-ref inputs "libepoxy") "/lib")
                                     (string-append (assoc-ref inputs "pulseaudio") "/lib")
                                     (string-append (assoc-ref inputs "libxkbcommon") "/lib")
                                     (string-append (assoc-ref inputs "xdotool") "/lib")
                                     (string-append (assoc-ref inputs "libxcb") "/lib")
                                     (string-append (assoc-ref inputs "dbus") "/lib")
                                     (string-append (assoc-ref inputs "linux-pam") "/lib")
                                     (string-append (assoc-ref inputs "mesa") "/lib"))
                               ":"))
                    (bash (string-append (assoc-ref inputs "bash-minimal") "/bin/bash")))
               (mkdir-p bin)
               (call-with-output-file (string-append bin "/rustdesk")
                 (lambda (port)
                   (format port "#!~a~%export LD_LIBRARY_PATH=\"~a:$LD_LIBRARY_PATH\"~%exec ~a/share/rustdesk/rustdesk \"$@\"~%"
                           bash lib-path out)))
               (chmod (string-append bin "/rustdesk") #o755)))))))
    (native-inputs `(("binutils" ,binutils)))
    (inputs `(("at-spi2-core" ,at-spi2-core)
              ("bash-minimal" ,bash-minimal)
              ("cairo" ,cairo)
              ("dbus" ,dbus)
              ("fontconfig" ,fontconfig)
              ("gcc:lib" ,gcc "lib")
              ("gdk-pixbuf" ,gdk-pixbuf)
              ("glib" ,glib)
              ("gst-plugins-base" ,gst-plugins-base)
              ("gstreamer" ,gstreamer)
              ("gtk+" ,gtk+)
              ("libepoxy" ,libepoxy)
              ("libx11" ,libx11)
              ("libxcb" ,libxcb)
              ("libxfixes" ,libxfixes)
              ("libxkbcommon" ,libxkbcommon)
              ("libxtst" ,libxtst)
              ("linux-pam" ,linux-pam)
              ("mesa" ,mesa)
              ("pango" ,pango)
              ("pulseaudio" ,pulseaudio)
              ("xdotool" ,xdotool)
              ("zlib" ,zlib)))
    (home-page "https://rustdesk.com/")
    (synopsis "Open-source remote desktop software")
    (description
     "RustDesk is an open-source remote desktop software that provides secure and
fast remote access to computers.  It works out of the box with no configuration
required, supports self-hosted servers, and is a privacy-focused alternative to
TeamViewer and AnyDesk.")
    (license license:agpl3+)))

(define-public wluma
  (package
    (name "wluma")
    (version "4.11.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/maximbaz/wluma")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0lyk6bzldq85rjm2fzknls3igaswsbkrc82i2rzyyv0ymp99vms6"))
       (snippet
        #~(begin
            (use-modules (guix build utils))
            ;; Fix build.rs to not panic when git-describe fails
            (substitute* "build.rs"
              (("Ok\\(o\\) => panic.*git-describe exited non-zero.*")
               "Ok(_) => version.to_string(),\n"))))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-auxiliary-files
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (udev-rules (string-append out "/lib/udev/rules.d"))
                    (examples (string-append out "/share/doc/wluma/examples")))
               (install-file "90-wluma-backlight.rules" udev-rules)
               (install-file "config.toml" examples)))))))
    (native-inputs
     (list clang git pkg-config))
    (inputs
     (cons* dbus
            eudev
            v4l-utils
            vulkan-loader
            (px-cargo-inputs 'wluma)))
    (home-page "https://github.com/maximbaz/wluma")
    (synopsis "Automatic brightness adjustment for Wayland")
    (description
     "Wluma automatically adjusts screen brightness based on screen contents
and ambient light.  It learns user brightness preferences and applies them
intelligently across different lighting conditions.  The tool uses Vulkan for
GPU-accelerated processing with minimal battery impact and supports multiple
displays through both laptop backlights and external monitors via DDC.")
    (license license:isc)))

(define-public rio
  (package
    (name "rio")
    (version "0.4.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/raphamorim/rio")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1wynhnp0sbhci49d0i5hd0227dg4g01vlx5npkivgh23ackw5piq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f
       #:tests? #f
       #:rust ,rust-1.92
       #:cargo-build-flags '("--release" "-p" "rioterm")
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (share (string-append out "/share"))
                    (apps (string-append share "/applications"))
                    (icons (string-append share "/icons/hicolor/scalable/apps"))
                    (terminfo (string-append share "/terminfo")))
               (mkdir-p bin)
               (install-file "target/release/rio" bin)
               (install-file "misc/rio.desktop" apps)
               (mkdir-p icons)
               (copy-file "misc/logo.svg" (string-append icons "/rio.svg"))
               (mkdir-p terminfo)
               (invoke "tic" "-x" "-o" terminfo "misc/rio.terminfo"))))
         (add-after 'install 'wrap-program
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (mesa (assoc-ref inputs "mesa"))
                   (vulkan (assoc-ref inputs "vulkan-loader"))
                   (wayland (assoc-ref inputs "wayland"))
                   (libxkbcommon (assoc-ref inputs "libxkbcommon")))
               (wrap-program (string-append out "/bin/rio")
                 `("LD_LIBRARY_PATH" ":" prefix
                   (,(string-append mesa "/lib")
                    ,(string-append vulkan "/lib")
                    ,(string-append wayland "/lib")
                    ,(string-append libxkbcommon "/lib"))))))))))
    (native-inputs
     (list cmake ncurses pkg-config shaderc))
    (inputs
     (cons* fontconfig
            libxkbcommon
            libx11
            libxcursor
            libxi
            libxrandr
            libxcb
            mesa
            oniguruma
            vulkan-loader
            wayland
            (px-cargo-inputs 'rioterm)))
    (home-page "https://rioterm.com")
    (synopsis "Hardware-accelerated GPU terminal emulator")
    (description
     "Rio is a hardware-accelerated GPU terminal emulator focusing on running
in desktops and browsers.  It features GPU rendering via WGPU, cross-platform
support, customizable themes, font ligatures, and sixel image support.")
    (license license:expat)))

(define-public networkmanager-dmenu
  (package
    (name "networkmanager-dmenu")
    (version "2.6.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/firecat53/networkmanager-dmenu")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1r57znpd3rkrm836jw2wb1fx82p724r85n8jb1chzi3p8qlhcf9m"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:install-plan #~'(("networkmanager_dmenu" "bin/networkmanager_dmenu")
                         ("networkmanager_dmenu.desktop"
                          "share/applications/networkmanager_dmenu.desktop"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'wrap-program
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let ((out (assoc-ref outputs "out"))
                    (gi-typelib-path (getenv "GI_TYPELIB_PATH")))
                (wrap-program (string-append out "/bin/networkmanager_dmenu")
                  `("GI_TYPELIB_PATH" ":" prefix (,gi-typelib-path))
                  `("GUIX_PYTHONPATH" ":" prefix
                    (,(getenv "GUIX_PYTHONPATH"))))))))))
    (inputs
     (list network-manager
           python
           python-pygobject))
    (home-page "https://github.com/firecat53/networkmanager-dmenu")
    (synopsis "NetworkManager control via dmenu, rofi, or similar launchers")
    (description
     "Networkmanager-dmenu is a Python script that provides a menu-based
interface to control NetworkManager connections.  It supports dmenu, rofi,
wofi, bemenu, and other dmenu-compatible launchers.  Features include managing
WiFi and wired connections, VPN support, and Bluetooth tethering.")
    (license license:expat)))

(define-public arbtt-capture-wl
  (package
    (name "arbtt-capture-wl")
    (version "0.1.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/franzos/arbtt-capture-wl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "18k5chax91zjyamip2brk7lm6hg1b8ry2f5qijr6928ns6drqaiz"))))
      (build-system cargo-build-system)
      (arguments
       `(#:install-source? #f))
      (inputs
       (cons* arbtt (px-cargo-inputs 'arbtt-capture-wl)))
      (home-page "https://github.com/franzos/arbtt-capture-wl")
      (synopsis "arbtt capture for Wayland compositors")
      (description
       "arbtt-capture-wl is a Wayland-native window tracker for the arbtt
(Automatic Rule-Based Time Tracker) system.  It captures active window
information from Wayland compositors like niri and sway, writing to the
standard arbtt log format for analysis with arbtt-stats.")
    (license license:gpl3)))

(define-public darkman
  (package
    (name "darkman")
    (version "2.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/WhyNotHugo/darkman")
             (commit "b7c84de3990977eb4ace8c9719ae708a45739d0d")))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1rkwjs3rjrzw6gkcm4q91d0axhdhnrwfp4f503dji2jvs8wqa33m"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           (substitute* "Makefile"
             ;; Avoid building the binary again when installing.
             (("install: build") "install: darkman.1")
             ;; Don't install the systemd service.
             (("install.*contrib/darkman.service") "true")
             ;; Don't install the openrc service.
             (("install.*openrc") "true")
             ;; The binary will be installed by `go install'.
             ((".@install.*bin.*") ""))))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #f ; No tests.
      #:install-source? #f
      #:import-path "gitlab.com/WhyNotHugo/darkman/cmd/darkman"
      #:unpack-path "gitlab.com/WhyNotHugo/darkman"
      #:build-flags
      #~(list (string-append "-ldflags= -X main.Version=" #$version))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-paths
            (lambda* (#:key unpack-path #:allow-other-keys)
              (substitute*
                  (find-files (string-append "src/" unpack-path "/contrib/dbus/")
                              "\\.service$")
                (("/usr") #$output))))
          (replace 'install
            (lambda* (#:key unpack-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" unpack-path)
                (let ((darkman (string-append #$output "/bin/darkman")))
                  (with-output-to-file "_darkman.zsh"
                    (lambda ()
                      (invoke darkman "completion" "zsh")))
                  (with-output-to-file "darkman.bash"
                    (lambda ()
                      (invoke darkman "completion" "bash")))
                  (with-output-to-file "darkman.fish"
                    (lambda ()
                      (invoke darkman "completion" "fish"))))
                (invoke "make" "install" (string-append "PREFIX=" #$output))))))))
    (native-inputs
     (list gnu-make
           go-github-com-goccy-go-yaml
           go-github-com-godbus-dbus-v5
           go-github-com-lmittmann-tint
           go-github-com-sj14-astral
           go-github-com-spf13-cobra))
    (home-page "https://gitlab.com/WhyNotHugo/darkman")
    (synopsis "Control dark-mode and light-mode transitions")
    (description
     "Darkman is a framework for dark-mode and light-mode transitions on Unix-like
desktops.  This version fixes a bug where scripts were incorrectly detected as
non-executable.")
    (license license:bsd-0)))

(define-public icebreaker
  (package
    (name "icebreaker")
    (version "2026.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/hecrj/icebreaker/archive/refs/tags/"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "19qx02ixnbmqg39fp9s428xvyw2n2236ipsfsla600d4fpswayyz"))))
    (build-system cargo-build-system)
    (arguments
     (list
      #:install-source? #f
      #:tests? #f
      #:rust rust-1.92
      #:cargo-install-paths ''(".")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'wrap-program
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let ((out (assoc-ref outputs "out"))
                    (mesa (assoc-ref inputs "mesa"))
                    (wayland (assoc-ref inputs "wayland"))
                    (libxkbcommon (assoc-ref inputs "libxkbcommon")))
                (wrap-program (string-append out "/bin/icebreaker")
                  `("LD_LIBRARY_PATH" ":" prefix
                    (,(string-append mesa "/lib")
                     ,(string-append wayland "/lib")
                     ,(string-append libxkbcommon "/lib"))))))))))
    (native-inputs (list pkg-config))
    (inputs
     (cons* fontconfig
            libxkbcommon
            mesa
            wayland
            `(,zstd "lib")
            (px-cargo-inputs 'icebreaker)))
    (home-page "https://github.com/hecrj/icebreaker")
    (synopsis "Local AI chat app powered by Rust and iced")
    (description
     "Icebreaker is a desktop application for local AI conversations.  It uses
the iced GUI framework for a responsive interface, llama.cpp for model inference,
and Hugging Face for model sourcing, all running locally without external
dependencies.")
    (license license:expat)))

(define-public vicinae
  (package
    (name "vicinae")
    (version "0.23.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/vicinaehq/vicinae/releases/download/v"
             version "/vicinae-linux-x86_64-v" version ".tar.gz"))
       (sha256
        (base32 "13zjv2rj6rkf7zbj94866bjd2rmjf8ky40nj8qcgrxm89m8jvgya"))))
    (supported-systems '("x86_64-linux"))
    (build-system binary-build-system)
    (arguments
     (list
      #:validate-runpath? #f
      #:patchelf-plan
      #~'(("bin/vicinae"
           ("glibc" "gcc"
            "qtbase" "qtdeclarative" "qtsvg"
            "qtkeychain-qt6" "layer-shell-qt"
            "libqalculate" "openssl" "zlib"
            "wayland" "libglvnd")))
      #:install-plan
      #~'(("bin" "bin")
          ("share/applications" "share/applications")
          ("share/icons" "share/icons")
          ("share/vicinae" "share/vicinae"))
      #:phases
      #~(modify-phases %standard-phases
          (replace 'unpack
            (lambda* (#:key inputs #:allow-other-keys)
              (invoke "tar" "-xzf" (assoc-ref inputs "source"))))
          (add-after 'install 'wrap-binary
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (qtbase (assoc-ref inputs "qtbase"))
                     (qtdeclarative (assoc-ref inputs "qtdeclarative"))
                     (layer-shell (assoc-ref inputs "layer-shell-qt"))
                     (qml-path (string-join
                                (list (string-append qtdeclarative "/lib/qt6/qml")
                                      (string-append layer-shell "/lib/qt6/qml"))
                                ":")))
                (wrap-program (string-append out "/bin/vicinae")
                  `("QT_PLUGIN_PATH" ":" prefix
                    (,(string-append qtbase "/lib/qt6/plugins")))
                  `("QML2_IMPORT_PATH" ":" prefix (,qml-path))
                  `("QML_IMPORT_PATH" ":" prefix (,qml-path)))))))))
    (inputs
     (list bash-minimal
           glibc
           `(,gcc "lib")
           qtbase
           qtdeclarative
           qtsvg
           qtkeychain-qt6
           layer-shell-qt
           px:libqalculate
           openssl
           zlib
           wayland
           libglvnd))
    (home-page "https://vicinae.com")
    (synopsis "Focused launcher for your desktop — native, fast, extensible")
    (description
     "Vicinae is a high-performance, native launcher for the Linux desktop,
inspired by Raycast.  It comes with a rich set of built-in modules and can be
extended via a TypeScript SDK.  Features include app launching, file search,
emoji picker, calculator, clipboard history, window focusing, script commands,
@code{dmenu} compatibility mode, theming, and Raycast extension compatibility.")
    (license license:gpl3+)))

(define-public actiona
  (package
    (name "actiona")
    (version "3.11.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/Jmgr/actiona/releases/download/v"
                           version "/actiona-" version "-source-linux.tar.gz"))
       (sha256
        (base32 "08f7jz1pm507ymh3rrpkvm9vpfn3s7aqzlfb0xbngqya73vmpgr0"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f
      #:configure-flags
      #~(list "-DCMAKE_BUILD_TYPE=Release"
              "-DACT_UPDATER=OFF"
              "-DCMAKE_INSTALL_RPATH=$ORIGIN;$ORIGIN/..")))
    (native-inputs
     (list pkg-config
           qttools))
    (inputs
     (list bluez
           eudev
           libnotify
           libx11
           libxext
           libxtst
           opencv
           qtbase
           qtdeclarative
           qtmultimedia
           qtsvg
           qtspeech
           qt5compat))
    (home-page "https://actiona.tools")
    (synopsis "Cross-platform task automation tool")
    (description
     "Actiona is an automation tool that allows executing many actions on
your computer such as emulating mouse clicks, key presses, showing message
boxes, and editing text files.  Tasks can be created using a simple editor
or written in EcmaScript (JavaScript) for more customization.")
    (license license:gpl3+)))
