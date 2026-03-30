;;; Package Repository for GNU Guix
;;; Copyright © 2025 Franz Geffke <mail@gofranz.com>

(define-module (px packages version-control)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system copy)
  #:use-module (guix gexp)
  #:use-module (nonguix build-system binary)
  #:use-module (nonguix licenses)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages webkit)
  #:use-module (px packages rust)
  #:use-module (px self))

(define-public gh
  (package
    (name "gh")
    (version "2.89.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/cli/cli/releases/download/v"
                           version "/gh_" version "_linux_amd64.tar.gz"))
       (sha256
        (base32 "1n4h6jxgmmnp1bqpysrx43hsifmfgnj8sm8wdkkk01ajvsm2qhnh"))))
    (build-system copy-build-system)
    (arguments
     '(#:install-plan
       '(("bin/gh" "bin/")
         ("share/man" "share/"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'generate-completions
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bash-comp (string-append out "/etc/bash_completion.d"))
                    (zsh-comp (string-append out "/share/zsh/site-functions"))
                    (fish-comp (string-append out "/share/fish/vendor_completions.d"))
                    (gh (string-append out "/bin/gh")))
               (mkdir-p bash-comp)
               (mkdir-p zsh-comp)
               (mkdir-p fish-comp)
               ;; Generate shell completions
               (with-output-to-file (string-append bash-comp "/gh")
                 (lambda () (invoke gh "completion" "-s" "bash")))
               (with-output-to-file (string-append zsh-comp "/_gh")
                 (lambda () (invoke gh "completion" "-s" "zsh")))
               (with-output-to-file (string-append fish-comp "/gh.fish")
                 (lambda () (invoke gh "completion" "-s" "fish")))
               #t))))))
    (supported-systems '("x86_64-linux"))
    (home-page "https://cli.github.com")
    (synopsis "GitHub command-line tool")
    (description
     "gh is GitHub on the command line.  It brings pull requests, issues, and
other GitHub concepts to the terminal next to where you are already working
with git and your code.")
    (license license:expat)))

(define-public gitbutler
  (package
    (name "gitbutler")
    (version "0.19.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://releases.gitbutler.com/releases/release/"
             version "-2922/linux/x86_64/GitButler_" version "_amd64.deb"))
       (sha256
        (base32 "0j71fpy48r6sbgk0ba8nf6631lq849xfl2cz91rl8ignfjgp6f2f"))))
    (build-system binary-build-system)
    (arguments
     `(#:patchelf-plan `(("usr/bin/gitbutler-tauri"
                          ("glib" "gtk+" "gdk-pixbuf" "cairo"
                           "webkitgtk-for-gtk3" "libsoup" "dbus" "zlib"
                           "gcc:lib"))
                         ("usr/bin/gitbutler-git-askpass"
                          ("gcc:lib" "glibc")))
       #:phases
       (modify-phases %standard-phases
         (replace 'unpack
           (lambda* (#:key inputs #:allow-other-keys)
             (invoke "ar" "x" (assoc-ref inputs "source"))
             (invoke "tar" "-xzf" "data.tar.gz")
             (delete-file "control.tar.gz")
             (delete-file "data.tar.gz")
             (delete-file "debian-binary")))
         (add-after 'install 'install-extras
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (share (string-append out "/share"))
                    (apps (string-append share "/applications"))
                    (icons (string-append share "/icons"))
                    (metainfo (string-append share "/metainfo")))
               (mkdir-p bin)
               (mkdir-p apps)
               (mkdir-p metainfo)
               ;; Install desktop file
               (copy-file "usr/share/applications/GitButler.desktop"
                          (string-append apps "/GitButler.desktop"))
               (substitute* (string-append apps "/GitButler.desktop")
                 (("Exec=gitbutler-tauri")
                  (string-append "Exec=" out "/bin/gitbutler-tauri"))
                 (("Icon=gitbutler-tauri")
                  (string-append "Icon=" out "/share/icons/hicolor/128x128/apps/gitbutler-tauri.png")))
               ;; Install icons
               (copy-recursively "usr/share/icons" icons)
               ;; Install metainfo
               (copy-file "usr/share/metainfo/com.gitbutler.gitbutler.metainfo.xml"
                          (string-append metainfo "/com.gitbutler.gitbutler.metainfo.xml")))))
         (add-after 'install-extras 'create-wrapper
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (wrap-program (string-append out "/usr/bin/gitbutler-tauri")
                 `("LD_LIBRARY_PATH" ":" prefix
                   (,(string-append (assoc-ref inputs "webkitgtk-for-gtk3") "/lib")
                    ,(string-append (assoc-ref inputs "libsoup") "/lib")
                    ,(string-append (assoc-ref inputs "gtk+") "/lib")
                    ,(string-append (assoc-ref inputs "glib") "/lib")
                    ,(string-append (assoc-ref inputs "gdk-pixbuf") "/lib")
                    ,(string-append (assoc-ref inputs "cairo") "/lib")
                    ,(string-append (assoc-ref inputs "dbus") "/lib")
                    ,(string-append (assoc-ref inputs "zlib") "/lib")
                    ,(string-append (assoc-ref inputs "gcc:lib") "/lib"))))
               ;; Create symlinks in bin
               (symlink (string-append out "/usr/bin/gitbutler-tauri")
                        (string-append bin "/gitbutler-tauri"))
               (symlink (string-append out "/usr/bin/gitbutler-git-askpass")
                        (string-append bin "/gitbutler-git-askpass"))
               (symlink (string-append bin "/gitbutler-tauri")
                        (string-append bin "/but"))))))))
    (native-inputs `(("binutils" ,binutils)))
    (inputs `(("bash-minimal" ,bash-minimal)
              ("cairo" ,cairo)
              ("dbus" ,dbus)
              ("gdk-pixbuf" ,gdk-pixbuf)
              ("glib" ,glib)
              ("glibc" ,glibc)
              ("gcc:lib" ,gcc "lib")
              ("gtk+" ,gtk+)
              ("libsoup" ,libsoup)
              ("webkitgtk-for-gtk3" ,webkitgtk-for-gtk3)
              ("zlib" ,zlib)))
    (supported-systems '("x86_64-linux"))
    (home-page "https://gitbutler.com")
    (synopsis "Git branch management tool")
    (description
     "GitButler is a Git client that lets you work on multiple branches at the
same time.  It allows you to quickly organize file changes into separate
branches while still having them applied to your working directory.  Features
include virtual branches, easy commit management, and GitHub integration.")
    (license (nonfree "https://github.com/gitbutlerapp/gitbutler/blob/master/LICENSE.md"
                      "FSL-1.1-Apache-2.0; converts to Apache 2.0 after 2 years."))))

(define-public jj-vcs
  (package
    (name "jj-vcs")
    (version "0.39.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "jj-cli" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "06p4pm9lkxq2fsz3sdl0nf23vf952di2w19mc8f21nxcpdcml209"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f
       #:tests? #f  ; Tests require testutils module
       #:rust ,rust-1.89))
    (native-inputs (list pkg-config))
    (inputs (cons* openssl (px-cargo-inputs 'jj-cli)))
    (home-page "https://github.com/jj-vcs/jj")
    (synopsis "Git-compatible version control system")
    (description
     "Jujutsu (jj) is a Git-compatible version control system that is both
powerful and easy to use.  It features automatic working copy management,
operation logging for easy undo, first-class conflict handling, automatic
rebasing of descendant commits, and comprehensive history rewriting tools.")
    (license license:asl2.0)))

(define-public keifu
  (package
    (name "keifu")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "keifu" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0fi930sxcxxs4cjqhi5h7x9f3n7k9rf9g5kw1y1386vlk0ygjlcw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f))
    (native-inputs (list perl))
    (inputs (px-cargo-inputs 'keifu))
    (home-page "https://github.com/trasta298/keifu")
    (synopsis "TUI tool to visualize Git commit graphs")
    (description
     "Keifu is a terminal user interface tool that visualizes Git commit graphs
with branch genealogy.  It provides color-coded commit graphs, branch
information, file change statistics, and basic Git operations.")
    (license license:expat)))
