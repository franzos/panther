;;; Package Repository for GNU Guix
;;; Copyright © 2021-2025 Franz Geffke <mail@gofranz.com>

(define-module (px packages databases)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages xorg)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix build-system cmake)
  #:use-module (nonguix build-system binary)
  #:use-module ((guix licenses)
                #:prefix license:))

(define-public sqlitecpp
  (package
    (name "sqlitecpp")
    (version "3.3.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/SRombauts/SQLiteCpp/archive/"
                           version ".tar.gz"))
       (sha256
        (base32 "1inav751a06khmgikd8iyl3phpnhcjz45s4fj8bk3i1vv1r47g9k"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f))
    (native-inputs `(("pkg-config" ,pkg-config)
                     ("sqlite" ,sqlite)))
    (home-page "https://github.com/SRombauts/SQLiteCpp")
    (synopsis
     "SQLiteC++ (SQLiteCpp) is a smart and easy to use C++ SQLite3 wrapper.")
    (description
     "SQLiteC++ offers an encapsulation around the native C APIs of
     SQLite, with a few intuitive and well documented C++ classes.")
    (license license:expat)))

(define-public dbeaver
  (package
    (name "dbeaver")
    (version "25.3.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/dbeaver/dbeaver/releases/download/"
             version "/dbeaver-ce-" version "-linux.gtk.x86_64.tar.gz"))
       (sha256
        (base32 "0l94x7l5mg6ipvknb8vqrrfvqpa5npqfn9yv6l3kplm7fjyyk9la"))))
    (build-system binary-build-system)
    (arguments
     (list
      #:validate-runpath? #f
      #:patchelf-plan
      ;; After unpack, we're in the dbeaver/ directory
      ;; glibc is provided automatically by binary-build-system
      #~'(("dbeaver" ())
          ("plugins/org.eclipse.equinox.launcher.gtk.linux.x86_64_1.2.1500.v20250801-0854/eclipse_11916.so" ())
          ;; JRE binaries
          ("jre/bin/java" ())
          ("jre/bin/jcmd" ())
          ("jre/bin/jinfo" ())
          ("jre/bin/jmap" ())
          ("jre/bin/jps" ())
          ("jre/bin/jrunscript" ())
          ("jre/bin/jstack" ())
          ("jre/bin/jstat" ())
          ("jre/bin/jwebserver" ())
          ("jre/bin/keytool" ())
          ("jre/bin/rmiregistry" ())
          ("jre/lib/jexec" ())
          ("jre/lib/jspawnhelper" ()))
      #:install-plan
      #~'(("." "share/dbeaver"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'create-wrapper
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (bin (string-append out "/bin"))
                     (dbeaver-bin (string-append out "/share/dbeaver/dbeaver"))
                     (wrapper (string-append bin "/dbeaver")))
                (mkdir-p bin)
                (call-with-output-file wrapper
                  (lambda (port)
                    (format port "#!~a
export GDK_BACKEND=x11
export GTK_PATH=\"~a/lib/gtk-3.0\"
export XDG_DATA_DIRS=\"~a/share:$XDG_DATA_DIRS\"
export GIO_EXTRA_MODULES=\"~a/lib/gio/modules\"
export LD_LIBRARY_PATH=\"~a/share/dbeaver/jre/lib:~a/share/dbeaver/jre/lib/server:~a/lib:~a/lib:~a/lib:~a/lib:~a/lib:~a/lib:$LD_LIBRARY_PATH\"
exec ~a \"$@\"
"
                            (search-input-file inputs "bin/bash")
                            (assoc-ref inputs "gtk+")
                            (assoc-ref inputs "gsettings-desktop-schemas")
                            (assoc-ref inputs "glib-networking")
                            out
                            out
                            (assoc-ref inputs "gtk+")
                            (assoc-ref inputs "glib")
                            (assoc-ref inputs "libx11")
                            (assoc-ref inputs "libxrender")
                            (assoc-ref inputs "libxtst")
                            (assoc-ref inputs "zlib")
                            dbeaver-bin)))
                (chmod wrapper #o755))))
          (add-after 'create-wrapper 'install-desktop-file
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (apps (string-append out "/share/applications"))
                     (icons (string-append out "/share/icons/hicolor/256x256/apps")))
                (mkdir-p apps)
                (mkdir-p icons)
                ;; Install icon
                (copy-file (string-append out "/share/dbeaver/dbeaver.png")
                           (string-append icons "/dbeaver.png"))
                ;; Create desktop file
                (call-with-output-file (string-append apps "/dbeaver.desktop")
                  (lambda (port)
                    (format port "[Desktop Entry]
Version=1.0
Type=Application
Terminal=false
Name=DBeaver Community
GenericName=Universal Database Manager
Comment=Universal Database Manager and SQL Client
Exec=~a/bin/dbeaver %U
Icon=dbeaver
Categories=IDE;Development;Database;
StartupWMClass=DBeaver
StartupNotify=true
Keywords=Database;SQL;IDE;JDBC;ODBC;MySQL;PostgreSQL;Oracle;MariaDB;
MimeType=application/sql
"
                            out)))))))))
    (inputs
     (list bash-minimal
           glib
           glib-networking
           gsettings-desktop-schemas
           gtk+
           libx11
           libxrender
           libxtst
           zlib))
    (supported-systems '("x86_64-linux"))
    (home-page "https://dbeaver.io/")
    (synopsis "Universal database manager and SQL client")
    (description
     "DBeaver is a multi-platform database tool for developers, SQL programmers,
database administrators and analysts.  It supports all popular databases:
MySQL, PostgreSQL, MariaDB, SQLite, Oracle, DB2, SQL Server, Sybase, MS Access,
Teradata, Firebird, Derby, and more.")
    (license license:asl2.0)))