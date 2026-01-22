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
  #:use-module (guix build-system qt)
  #:use-module (guix build-system copy)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (nonguix build-system chromium-binary)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnuzilla)
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
  #:use-module (gnu packages python)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages search)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages video)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xfce)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages xml)
  #:use-module (px packages qt)
  #:use-module (px packages common)
  #:use-module (px packages library)
  #:use-module (px packages haskell)
  #:use-module (px packages themes)
  #:use-module (px self)
  #:use-module (srfi srfi-1))

(define-public albert-launcher
  (package
    (name "albert-launcher")
    (version "33.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/albertlauncher/albert")
             (commit (string-append "v" version))
             (recursive? #t)))
       (sha256
        (base32 "19y74xv4acs65d2f80fc7qi1n3pvx7i0za57jdrb8iybbjyg4wnc"))
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
    (version "5.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/pvanek/qlipper/archive/"
                           version ".tar.gz"))
       (sha256
        (base32 "0vbhiyn56qwlssavim02kp0y5rxj6gdffchyigkhpg8qza64afch"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f))
    (native-inputs (list qtbase-5 qttools-5))
    (home-page "https://github.com/pvanek/qlipper")
    (synopsis "Lightweight and cross-platform clipboard history applet.")
    (description "Lightweight and cross-platform clipboard history applet.")
    (license license:gpl2+)))

(define-public cpputilities
  (package
    (name "cpputilities")
    (version "5.22.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/Martchus/cpp-utilities/archive/refs/tags/v"
             version ".tar.gz"))
       (sha256
        (base32 "18mbzw8w0a4l5r0w1fr200m4v8ww65r838618z19v1ymz6aahs7a"))))
    (build-system cmake-build-system)
    (inputs `(("gcc" ,gcc-11)))
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
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/Martchus/qtforkawesome/archive/refs/tags/v"
             version ".tar.gz"))
       (sha256
        (base32 "0pj4w93vrsmgqlgihwz6s2jdazmfsdw3lxns7wk9908l4ilcqw9d"))))
    (build-system cmake-build-system)
    (native-inputs (list gcc-11
                         qtutilities
                         cpputilities
                         qtbase-5
                         qtquickcontrols-5
                         qtquickcontrols2-5
                         qtdeclarative-5
                         perl
                         perl-yaml
                         fork-awesome))
    (arguments
     `(#:tests? #f
       #:configure-flags ,#~(list (string-append "-DFORK_AWESOME_FONT_FILE="
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
    (version "1.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/Martchus/syncthingtray/archive/refs/tags/v"
             version ".tar.gz"))
       (sha256
        (base32 "0cyimd018bknvip6jxz83w0va05kgfcix53jryqdka665p4048ba"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f
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
    (native-inputs (list extra-cmake-modules qttools-5 gcc-11))
    (inputs `(("qtbase" ,qtbase-5)
              ("qtquickcontrols2" ,qtquickcontrols2-5)
              ("qtutilities" ,qtutilities)
              ("boost" ,boost)
              ("gcc" ,gcc-11)
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

(define-public px-terminal-launcher
  (package
    (name "px-terminal-launcher")
    (version "v0.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://source.pantherx.org/" name "_" version
                           ".tgz"))
       (sha256
        (base32 "0b5fjnrb2kggrkfgsc3qkjmrxvp6c54xczhw1mfaq39li3z6sg5c"))))
    (build-system qt-build-system)
    (arguments
     ;; make qtsvg work
     (list #:tests? #f
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'install 'wrap-executable
                 (lambda* (#:key inputs outputs #:allow-other-keys)
                   (let ((out (assoc-ref outputs "out"))
                         (plugin-path (getenv "QT_PLUGIN_PATH")))
                     (wrap-program (string-append out "/bin/launcher")
                       `("QT_PLUGIN_PATH" ":" prefix (,plugin-path)))))))))
    (native-inputs (list qttools pkg-config extra-cmake-modules))
    (inputs (list capnproto
                  qtbase
                  qtsvg
                  qtcharts
                  networkmanager-qt))
    (home-page "https://www.pantherx.dev")
    (synopsis "PantherX Terminal Launcher")
    (description "PantherX Terminal Launcher")
    (license license:expat)))

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
    (version "0.0.120")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://stable.dl2.discordapp.net/apps/linux/" version
         "/discord-" version ".deb"))
       (sha256
        (base32 "07bcz6hnmkvqnv2xnm8v0hf2skc8ggji97rcp41sdfrd55s8651j"))))
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

(define-public wluma
  (package
    (name "wluma")
    (version "4.10.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/maximbaz/wluma")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0jf0gqh9xx0qilgqdv0hyjshw60carjniv9lin7wriszpbji42lb"))
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
    (version "0.2.29")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/raphamorim/rio")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "028vw27yv8lrz30xzipzdz4d6qbjlyrsa1pr0diawihrjcamdkl4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f
       #:tests? #f
       #:rust ,rust-1.88
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
     (list cmake ncurses pkg-config))
    (inputs
     (cons* fontconfig
            libxkbcommon
            libx11
            libxcursor
            libxi
            libxrandr
            libxcb
            mesa
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
    (version "0.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/franzos/arbtt-capture-wl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1wi1kb1rlvchjrhyl8b9c7rjxi4rn3jvrmy0fv15gry3sqbwxymw"))))
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