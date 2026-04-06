;;; Package Repository for GNU Guix
;;; Copyright © 2026 Franz Geffke <mail@gofranz.com>
;;;
;;; Stand-alone Thunderbird package that tracks the latest upstream release
;;; (rapid channel) directly from archive.mozilla.org, with telemetry and
;;; auto-update disabled.  Unlike Guix's icedove (ESR 140 + icecat source),
;;; this package fetches the self-contained Thunderbird source tarball so it
;;; can track non-ESR versions independently of Firefox/IceCat.

(define-module (px packages gnuzilla)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages hunspell)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image)
  #:use-module (gnu packages kerberos)
  #:use-module (gnu packages libcanberra)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages messaging)
  #:use-module (gnu packages node)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages video)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg))

(define %thunderbird-version "149.0.1")
(define %thunderbird-build-id "20260326000000") ;must be YYYYMMDDhhmmss

(define-public thunderbird
  (package
    (name "thunderbird")
    (version %thunderbird-version)
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://archive.mozilla.org/pub/thunderbird/"
                           "releases/" version "/source/"
                           "thunderbird-" version ".source.tar.xz"))
       (sha256
        (base32 "0g7mavaxkb1l5wffdya7gkqbgckxlbzwfm34hng36vxppca8vpgn"))))
    (properties
     `((cpe-name . "thunderbird")))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f                               ;no check target
      #:imported-modules %cargo-utils-modules   ;for generate-all-checksums
      #:modules `((guix build utils)
                  (ice-9 regex)
                  ,@%default-gnu-modules)
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'disable-telemetry-and-updates
            ;; Flip the user-facing prefs that control telemetry uploads,
            ;; application auto-update, extension auto-update, and the
            ;; remote-settings experiment server.  The updater itself is
            ;; also removed via --disable-updater below; these pref flips
            ;; are defense in depth and close off the add-on update path.
            (lambda _
              (substitute* "comm/mail/app/profile/all-thunderbird.js"
                ;; App update
                (("pref\\(\"app\\.update\\.auto\", true\\)")
                 "pref(\"app.update.auto\", false)")
                (("pref\\(\"app\\.update\\.service\\.enabled\", true\\)")
                 "pref(\"app.update.service.enabled\", false)")
                (("pref\\(\"app\\.update\\.staging\\.enabled\", true\\)")
                 "pref(\"app.update.staging.enabled\", false)")
                (("pref\\(\"app\\.update\\.langpack\\.enabled\", true\\)")
                 "pref(\"app.update.langpack.enabled\", false)")
                (("pref\\(\"app\\.update\\.checkInstallTime\", true\\)")
                 "pref(\"app.update.checkInstallTime\", false)")
                ;; Data reporting / health report
                (("pref\\(\"datareporting\\.healthreport\\.uploadEnabled\", true\\)")
                 "pref(\"datareporting.healthreport.uploadEnabled\", false)")
                (("pref\\(\"datareporting\\.policy\\.dataSubmissionEnabled\", true\\)")
                 "pref(\"datareporting.policy.dataSubmissionEnabled\", false)")
                ;; Telemetry pings
                (("pref\\(\"toolkit\\.telemetry\\.server\", \"[^\"]*\"\\)")
                 "pref(\"toolkit.telemetry.server\", \"\")")
                (("pref\\(\"toolkit\\.telemetry\\.archive\\.enabled\", true\\)")
                 "pref(\"toolkit.telemetry.archive.enabled\", false)")
                (("pref\\(\"toolkit\\.telemetry\\.shutdownPingSender\\.enabled\", true\\)")
                 "pref(\"toolkit.telemetry.shutdownPingSender.enabled\", false)")
                (("pref\\(\"toolkit\\.telemetry\\.firstShutdownPing\\.enabled\", true\\)")
                 "pref(\"toolkit.telemetry.firstShutdownPing.enabled\", false)")
                (("pref\\(\"toolkit\\.telemetry\\.newProfilePing\\.enabled\", true\\)")
                 "pref(\"toolkit.telemetry.newProfilePing.enabled\", false)")
                (("pref\\(\"toolkit\\.telemetry\\.updatePing\\.enabled\", true\\)")
                 "pref(\"toolkit.telemetry.updatePing.enabled\", false)")
                (("pref\\(\"toolkit\\.telemetry\\.bhrPing\\.enabled\", true\\)")
                 "pref(\"toolkit.telemetry.bhrPing.enabled\", false)")
                ;; Extension auto-update & remote lookup
                (("pref\\(\"extensions\\.update\\.enabled\", true\\)")
                 "pref(\"extensions.update.enabled\", false)")
                (("pref\\(\"extensions\\.update\\.autoUpdateDefault\", true\\)")
                 "pref(\"extensions.update.autoUpdateDefault\", false)")
                (("pref\\(\"extensions\\.systemAddon\\.update\\.enabled\", true\\)")
                 "pref(\"extensions.systemAddon.update.enabled\", false)")
                (("pref\\(\"extensions\\.getAddons\\.cache\\.enabled\", true\\)")
                 "pref(\"extensions.getAddons.cache.enabled\", false)")
                ;; Remote-settings / Nimbus experiments
                (("pref\\(\"services\\.settings\\.server\", \"[^\"]*\"\\)")
                 "pref(\"services.settings.server\", \"\")"))))
          (add-after 'unpack 'do-not-verify-vendored-rust-dependencies
            (lambda _
              (substitute* "comm/python/rocbuild/rocbuild/rust.py"
                (("result = check_vendored_dependencies\\(topsrcdir)")
                 "sys.exit(0)"))))
          (add-after 'patch-source-shebangs 'patch-cargo-checksums
            (lambda _
              (use-modules (guix build cargo-utils))
              (let ((null-hash "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934\
ca495991b7852b855"))
                (for-each (lambda (file)
                            (format #t "patching checksums in ~a~%" file)
                            (substitute* file
                              (("^checksum = \".*\"")
                               (string-append "checksum = \"" null-hash "\""))))
                          (find-files "." "Cargo.lock$"))
                (for-each generate-all-checksums
                          '("services"
                            "js"
                            "third_party/rust"
                            "dom/media"
                            "dom/webauthn"
                            "toolkit"
                            "gfx"
                            "storage"
                            "modules"
                            "xpcom/rust"
                            "media"
                            "mozglue/static/rust"
                            "netwerk"
                            "remote"
                            "intl"
                            "servo"
                            "security/manager/ssl"
                            "build"
                            "comm")))))
          (add-after 'patch-cargo-checksums 'remove-cargo-frozen-flag
            (lambda _
              (substitute* "build/RunCbindgen.py"
                (("args.append\\(\"--frozen\"\\)") "pass"))
              (substitute* "config/makefiles/rust.mk"
                (("cargo_build_flags \\+= --frozen") ""))))
          (add-after 'patch-source-shebangs 'fix-profile-setting
            ;; Avoid per-installation-directory profiles.
            (lambda _
              (substitute* "comm/mail/moz.configure"
                (("\"MOZ_DEDICATED_PROFILES\", True")
                 "\"MOZ_DEDICATED_PROFILES\", False"))))
          (add-after 'build 'neutralize-store-references
            (lambda _
              (substitute*
                  (find-files "." "buildconfig.html")
                (((format #f "(~a/)([0-9a-df-np-sv-z]{32})"
                          (regexp-quote (%store-directory)))
                  _ store hash)
                 (string-append store
                                (string-take hash 8)
                                "<!-- Guix: not a runtime dependency -->"
                                (string-drop hash 8))))))
          (delete 'bootstrap)
          (replace 'configure
            (lambda* (#:key native-inputs inputs configure-flags
                      #:allow-other-keys)
              (let* ((bash (which "bash"))
                     (mozconfig (string-append (getcwd) "/.mozconfig")))
                (setenv "SHELL" bash)
                (setenv "CONFIG_SHELL" bash)
                (setenv "QA_CONFIGURE_OPTIONS" ".*")
                (setenv "MOZBUILD_STATE_PATH"
                        (string-append (getcwd) "/mach_state"))
                (setenv "MOZCONFIG" mozconfig)
                (setenv "AR" "llvm-ar")
                (setenv "NM" "llvm-nm")
                (setenv "CC" "clang")
                (setenv "CXX" "clang++")
                (setenv "MOZ_NOSPAM" "1")
                (setenv "MACH_BUILD_PYTHON_NATIVE_PACKAGE_SOURCE" "system")
                (setenv "GUIX_PYTHONPATH"
                        (string-append (getcwd)
                                       "/obj-x86_64-pc-linux-gnu/_virtualenvs/build"))
                (setenv "MOZ_BUILD_DATE" #$%thunderbird-build-id)
                (setenv "LDFLAGS"
                        (string-append "-Wl,-rpath=" #$output
                                       "/lib/thunderbird"))
                (mkdir-p (string-append (getcwd) "/builddir"))
                (with-output-to-file mozconfig
                  (lambda ()
                    (display
                     (string-append
                      "ac_add_options --allow-addon-sideload\n"
                      "ac_add_options --with-unsigned-addon-scopes=app,system\n"
                      "ac_add_options --disable-crashreporter\n"
                      "ac_add_options --disable-debug\n"
                      "ac_add_options --disable-debug-symbols\n"
                      "ac_add_options --disable-elf-hack\n"
                      "ac_add_options --disable-jit\n"
                      "ac_add_options --disable-necko-wifi\n"
                      "ac_add_options --disable-tests\n"
                      "ac_add_options --disable-updater\n"
                      "ac_add_options --disable-webrtc\n"
                      "ac_add_options --enable-application=comm/mail\n"
                      "ac_add_options --enable-default-toolkit=\"cairo-gtk3\"\n"
                      "ac_add_options --enable-optimize\n"
                      "ac_add_options --enable-pulseaudio\n"
                      "ac_add_options --enable-release\n"
                      "ac_add_options --enable-rust-simd\n"
                      "ac_add_options --enable-strip\n"
                      "ac_add_options --enable-system-ffi\n"
                      "ac_add_options --enable-system-pixman\n"
                      "ac_add_options --prefix=" #$output "\n"
                      "ac_add_options --with-clang-path="
                      (search-input-file (or native-inputs inputs)
                                         "bin/clang") "\n"
                      "ac_add_options --with-distribution-id=org.panther\n"
                      "ac_add_options --with-libclang-path="
                      #$(this-package-native-input "clang") "/lib\n"
                      "ac_add_options --with-system-bz2\n"
                      "ac_add_options --with-system-icu\n"
                      "ac_add_options --with-system-jpeg\n"
                      "ac_add_options --with-system-libevent\n"
                      "ac_add_options --with-system-nspr\n"
                      "ac_add_options --with-system-nss\n"
                      "ac_add_options --with-system-zlib\n"
                      "ac_add_options --without-wasm-sandboxed-libraries\n"
                      ;; Strip data reporting at compile time.
                      "ac_add_options MOZ_TELEMETRY_REPORTING=\n"
                      "ac_add_options MOZ_DATA_REPORTING=\n"
                      "mk_add_options MOZ_MAKE_FLAGS=-j"
                      (number->string (parallel-job-count)) "\n"))))
                (invoke "./mach" "configure"))))
          (replace 'build
            (lambda _ (invoke "./mach" "build")))
          (replace 'install
            (lambda _ (invoke "./mach" "install")))
          (add-after 'install 'install-desktop-file
            ;; Thunderbird doesn't ship a .desktop file upstream.
            ;; See https://bugzilla.mozilla.org/show_bug.cgi?id=1637575
            (lambda _
              (let ((apps (string-append #$output "/share/applications")))
                (mkdir-p apps)
                (with-output-to-file (string-append apps "/thunderbird.desktop")
                  (lambda _
                    (format #t
                            "[Desktop Entry]~@
                            Name=Thunderbird~@
                            Exec=~a/bin/thunderbird %u~@
                            Icon=thunderbird~@
                            GenericName=Mail/News Client~@
                            Categories=Network;Email;~@
                            Terminal=false~@
                            StartupNotify=true~@
                            MimeType=x-scheme-handler/mailto;~@
                            Type=Application~@
                            Actions=ComposeMessage;~@
                            StartupWMClass=thunderbird;~@
                            [Desktop Action ComposeMessage]~@
                            Name=Write new message~@
                            Exec=~@*~a/bin/thunderbird -compose~%"
                            #$output))))))
          (add-after 'install-desktop-file 'install-icons
            (lambda _
              (with-directory-excursion "comm/mail/branding/thunderbird"
                (for-each
                 (lambda (file)
                   (let* ((size (string-filter char-numeric? file))
                          (icons (string-append #$output "/share/icons/hicolor/"
                                                size "x" size "/apps")))
                     (mkdir-p icons)
                     (copy-file file (string-append icons "/thunderbird.png"))))
                 '("default16.png" "default22.png" "default24.png"
                   "default32.png" "default48.png" "default256.png")))))
          (add-after 'install 'wrap-program
            (lambda* (#:key inputs #:allow-other-keys)
              (let* ((lib (string-append #$output "/lib"))
                     (gpgme #$(this-package-input "gpgme"))
                     (gpgme-lib (string-append gpgme "/lib"))
                     (gtk #$(this-package-input "gtk+"))
                     (gtk-share (string-append gtk "/share"))
                     (pulseaudio #$(this-package-input "pulseaudio"))
                     (pulseaudio-lib (string-append pulseaudio "/lib"))
                     (eudev #$(this-package-input "eudev"))
                     (eudev-lib (string-append eudev "/lib"))
                     (libnotify #$(this-package-input "libnotify"))
                     (libnotify-lib (string-append libnotify "/lib"))
                     (mesa #$(this-package-input "mesa"))
                     (mesa-lib (string-append mesa "/lib"))
                     (pciutils #$(this-package-input "pciutils"))
                     (pciutils-lib (string-append pciutils "/lib"))
                     (libva #$(this-package-input "libva"))
                     (libva-lib (string-append libva "/lib"))
                     (libotr #$(this-package-input "libotr"))
                     (libotr-lib (string-append libotr "/lib")))
                (wrap-program (car (find-files lib "^thunderbird$"))
                  `("XDG_DATA_DIRS" prefix (,gtk-share))
                  `("LD_LIBRARY_PATH" prefix
                    (,pulseaudio-lib ,eudev-lib ,libnotify-lib ,gpgme-lib
                     ,mesa-lib ,libva-lib ,libotr-lib ,pciutils-lib)))))))))
    (inputs
     (list alsa-lib
           bash-minimal
           bzip2
           cairo
           cups
           dbus-glib
           eudev
           ffmpeg
           freetype
           gdk-pixbuf
           glib
           gpgme
           gtk+
           hunspell
           icu4c-78
           libcanberra
           libevent
           libffi
           libgnome
           libjpeg-turbo
           libnotify
           libotr
           libpng-apng
           libva
           libvpx
           libxcomposite
           libxft
           libxinerama
           libxscrnsaver
           libxt
           mesa
           mit-krb5
           nspr
           nss-rapid
           pango
           pciutils
           pixman
           pulseaudio
           sqlite
           startup-notification
           unzip
           zip
           zlib))
    (native-inputs
     (list clang-20
           llvm-20
           m4
           nasm
           node-lts
           perl
           pkg-config
           python-wrapper
           rust
           `(,rust "cargo")
           rust-cbindgen
           which
           yasm))
    (home-page "https://www.thunderbird.net")
    (synopsis "Mozilla Thunderbird email, calendar and chat client")
    (description
     "Thunderbird is a free, cross-platform email, calendar, chat and
newsgroup client from the Mozilla project.  This build tracks the latest
upstream release (rapid channel) and has telemetry, crash reporting and
the binary auto-updater disabled at build time.")
    (license license:mpl2.0)))
