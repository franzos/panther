;;; Package Repository for GNU Guix
;;; Copyright © 2026 Franz Geffke <mail@gofranz.com>

(define-module (px sandbox)
  #:use-module (gnu packages glib)
  #:use-module ((gnu packages linux) #:select (alsa-plugins))
  #:use-module (gnu packages virtualization)
  #:use-module (guix build-system trivial)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:export (sandbox-policy
            sandbox-policy?
            sandboxed
            %portal-settings-dbus-rules
            %portal-open-dbus-rules
            %portal-screencast-dbus-rules))

;;;
;;; Bubblewrap launchers for existing packages.
;;;
;;; Paths in a policy are relative to $HOME unless absolute or starting with
;;; "$XDG_RUNTIME_DIR/".  Writable paths are directories, created on the host
;;; before launch.
;;;

(define-record-type* <sandbox-policy>
  sandbox-policy make-sandbox-policy
  sandbox-policy?
  (read-only-paths sandbox-policy-read-only-paths
                   (default '()))
  (writable-paths sandbox-policy-writable-paths
                  (default '()))
  (network? sandbox-policy-network?
            (default #f))
  (wayland? sandbox-policy-wayland?
            (default #f))
  ;; Used when no Wayland socket is bound.  X11 clients can watch and drive
  ;; each other.
  (x11? sandbox-policy-x11?
        (default #f))
  (gpu? sandbox-policy-gpu?
        (default #f))
  ;; The PulseAudio socket; PipeWire serves it through pipewire-pulse.
  (audio? sandbox-policy-audio?
          (default #f))
  ;; V4L2 devices, found at launch.
  (camera? sandbox-policy-camera?
           (default #f))
  ;; Chromium and Electron build their own sandbox from user namespaces.
  (user-namespaces? sandbox-policy-user-namespaces?
                    (default #f))
  ;; Files named on the command line, read-only.
  (file-arguments? sandbox-policy-file-arguments?
                   (default #f))
  ;; xdg-dbus-proxy filter options; empty means no session bus.
  (dbus-rules sandbox-policy-dbus-rules
              (default '())))

(define %sandbox-environment
  '("PATH" "HOME" "USER" "LOGNAME" "LANG" "LC_ALL" "TERM" "TZ" "TZDIR"
    "XDG_RUNTIME_DIR" "XDG_SESSION_TYPE" "XDG_CURRENT_DESKTOP"
    "XDG_DATA_DIRS" "XDG_CONFIG_DIRS" "GUIX_LOCPATH"
    "GUIX_GDK_PIXBUF_MODULE_FILES" "GST_PLUGIN_SYSTEM_PATH"
    "ZATHURA_PLUGINS_PATH" "XCURSOR_PATH" "XCURSOR_THEME" "XCURSOR_SIZE"))

;; Theme and colour-scheme preferences, nothing else of the portal.
(define %portal-settings-dbus-rules
  (map (lambda (rule)
         (string-append rule "@/org/freedesktop/portal/desktop"))
       '("--call=org.freedesktop.portal.Desktop=org.freedesktop.DBus.Properties.*"
         "--call=org.freedesktop.portal.Desktop=org.freedesktop.portal.Settings.*"
         "--broadcast=org.freedesktop.portal.Desktop=org.freedesktop.portal.Settings.*")))

;; Screen and window capture; the portal asks the user, then hands over a
;; PipeWire remote that only sees what they picked.
(define %portal-screencast-dbus-rules
  (let ((portal "org.freedesktop.portal.Desktop=org.freedesktop.")
        (path "@/org/freedesktop/portal/desktop"))
    (list (string-append "--call=" portal "DBus.Properties.*" path)
          (string-append "--call=" portal "portal.ScreenCast.*" path)
          (string-append "--call=" portal "portal.Request.*" path "/request/*")
          (string-append "--call=" portal "portal.Session.*" path "/session/*")
          (string-append "--broadcast=" portal "portal.Request.*" path
                         "/request/*")
          (string-append "--broadcast=" portal "portal.Session.*" path
                         "/session/*"))))

;; Files the program can read, opened in the user's default app or shown in
;; the file manager.
(define %portal-open-dbus-rules
  (let ((portal "org.freedesktop.portal.Desktop=org.freedesktop.")
        (path "@/org/freedesktop/portal/desktop"))
    (list (string-append "--call=" portal "DBus.Properties.*" path)
          (string-append "--call=" portal "portal.OpenURI.OpenFile" path)
          (string-append "--call=" portal "portal.OpenURI.OpenDirectory" path)
          (string-append "--call=" portal "portal.Request.*" path "/request/*")
          (string-append "--broadcast=" portal "portal.Request.*" path
                         "/request/*"))))

;; Sends ALSA clients to the PulseAudio socket.
(define %sandbox-asound-conf
  (computed-file
   "asound.conf"
   #~(call-with-output-file #$output
       (lambda (port)
         (let ((lib (string-append #$alsa-plugins:pulseaudio "/lib/alsa-lib/")))
           (format port "pcm_type.pulse { lib \"~alibasound_module_pcm_pulse.so\" }
ctl_type.pulse { lib \"~alibasound_module_ctl_pulse.so\" }
pcm.!default { type pulse }
ctl.!default { type pulse }~%"
                   lib lib))))))

;; Store items behind symlinks; fonts, icons, locales and themes live here.
(define %sandbox-profiles
  '("/run/current-system/profile" ".guix-home/profile" ".guix-profile"))

(define (sandbox-launcher base base-output policy)
  (match-record policy <sandbox-policy>
    (read-only-paths writable-paths network? wayland? x11? gpu? audio? camera?
     user-namespaces? file-arguments? dbus-rules)
    (program-file
     (string-append (package-name base) "-sandbox-launcher")
     (with-imported-modules '((guix build utils))
       #~(begin
           (use-modules (guix build utils)
                        (ice-9 ftw)
                        (ice-9 regex)
                        (srfi srfi-1)
                        (srfi srfi-26)
                        (web uri))
           (let* ((home (getenv "HOME"))
                  (runtime (or (getenv "XDG_RUNTIME_DIR")
                               (format #f "/run/user/~a" (getuid))))
                  (absolute (lambda (path)
                              (cond
                               ((string-prefix? "/" path) path)
                               ((string-prefix? "$XDG_RUNTIME_DIR/" path)
                                (string-append runtime
                                               (string-drop path 16)))
                               (else (string-append home "/" path)))))
                  (bind (lambda (option)
                          (lambda (path)
                            (let ((path (absolute path)))
                              (list option path path)))))
                  (wayland-socket
                   (let ((socket (string-append
                                  runtime "/"
                                  (or (getenv "WAYLAND_DISPLAY") "wayland-0"))))
                     (and #$wayland? (file-exists? socket) socket)))
                  ;; Local displays only.
                  (x11-display
                   (and #$x11? (not wayland-socket)
                        (string-match "^(unix)?:([0-9]+)"
                                      (or (getenv "DISPLAY") ""))))
                  (x11-authority (or (getenv "XAUTHORITY")
                                     (string-append home "/.Xauthority")))
                  (profile (lambda (path)
                             (let ((path (absolute path)))
                               (if (file-exists? path)
                                   (list "--symlink" (canonicalize-path path)
                                         path)
                                   '()))))
                  (file-argument
                   (lambda (argument)
                     (let ((file (if (string-prefix? "file://" argument)
                                     (uri-decode (string-drop argument 7))
                                     argument)))
                       (if (file-exists? file)
                           (let ((path (string-append
                                        (canonicalize-path (dirname file))
                                        "/" (basename file))))
                             (list "--ro-bind" path path))
                           '()))))
                  (session-bus
                   (lambda (rules)
                     (let ((socket (format #f "~a/sandbox-bus-~a"
                                           runtime (getpid)))
                           (bus (string-append runtime "/bus"))
                           (sync (pipe)))
                       ;; The proxy writes to its stdout once it listens, and
                       ;; exits when bwrap closes the other end.
                       (spawn #$(file-append xdg-dbus-proxy
                                             "/bin/xdg-dbus-proxy")
                              (cons* "xdg-dbus-proxy" "--fd=1"
                                     (or (getenv "DBUS_SESSION_BUS_ADDRESS")
                                         (string-append "unix:path=" bus))
                                     socket "--filter" rules)
                              #:output (cdr sync))
                       (close-port (cdr sync))
                       (read-char (car sync))
                       (list "--sync-fd"
                             (number->string (port->fdes (car sync)))
                             "--ro-bind" socket bus
                             "--setenv" "DBUS_SESSION_BUS_ADDRESS"
                             (string-append "unix:path=" bus)))))
                  ;; Every bin/ entry links here; the invoked name picks the program.
                  (program (string-append (ungexp base base-output) "/bin/"
                                          (basename (car (command-line)))))
                  ;; Shared by all instances, for single-instance sockets.
                  (app-runtime (string-append runtime "/app/"
                                              #$(package-name base))))
             (for-each (compose mkdir-p absolute) '#$writable-paths)
             (mkdir-p app-runtime)
             (chmod app-runtime #o700)
             ;; Only the user's own entries; some programs find $HOME with
             ;; getpwuid.
             (let ((user (getpwuid (getuid)))
                   (group (getgrgid (getgid))))
               (call-with-output-file (string-append app-runtime "/passwd")
                 (lambda (port)
                   (format port "~a:x:~a:~a::~a:/bin/sh~%"
                           (passwd:name user) (getuid) (getgid) home)))
               (call-with-output-file (string-append app-runtime "/group")
                 (lambda (port)
                   (format port "~a:x:~a:~%" (group:name group) (getgid)))))
             (apply execl #$(file-append bubblewrap "/bin/bwrap") "bwrap"
                    (append
                     '#$(if user-namespaces?
                            '()
                            '("--disable-userns"))
                     '("--unshare-all" "--unshare-user" "--clearenv"
                       "--ro-bind" "/gnu/store" "/gnu/store"
                       "--proc" "/proc" "--dev" "/dev" "--tmpfs" "/tmp"
                       "--ro-bind-try" "/etc/localtime" "/etc/localtime")
                     (list "--dir" home
                           "--perms" "0700" "--dir" runtime
                           "--bind" app-runtime app-runtime
                           "--ro-bind" (string-append app-runtime "/passwd")
                           "/etc/passwd"
                           "--ro-bind" (string-append app-runtime "/group")
                           "/etc/group"
                           "--setenv" "TMPDIR" app-runtime
                           ;; dconf is out of reach; failed writes crash some
                           ;; programs.
                           "--setenv" "GSETTINGS_BACKEND" "memory")
                     (append-map (lambda (variable)
                                   (let ((value (getenv variable)))
                                     (if value
                                         (list "--setenv" variable value)
                                         '())))
                                 '#$(append %sandbox-environment
                                            (if wayland?
                                                '("WAYLAND_DISPLAY")
                                                '())
                                            (if network?
                                                '("SSL_CERT_FILE" "SSL_CERT_DIR")
                                                '())))
                     (if wayland-socket
                         ((bind "--ro-bind") wayland-socket)
                         '())
                     (if x11-display
                         (append
                          (list "--setenv" "DISPLAY" (getenv "DISPLAY")
                                "--setenv" "XAUTHORITY" x11-authority)
                          ((bind "--ro-bind")
                           (string-append "/tmp/.X11-unix/X"
                                          (match:substring x11-display 2)))
                          ((bind "--ro-bind-try") x11-authority))
                         '())
                     (if #$audio?
                         (cons* "--ro-bind" #$%sandbox-asound-conf
                                "/etc/asound.conf"
                                ((bind "--ro-bind-try")
                                 (string-append runtime "/pulse/native")))
                         '())
                     (if #$camera?
                         (append
                          (append-map (lambda (device)
                                        (let ((path (string-append "/dev/"
                                                                   device)))
                                          (list "--dev-bind" path path)))
                                      (or (scandir "/dev"
                                                   (cut string-prefix?
                                                        "video" <>))
                                          '()))
                          '("--ro-bind-try" "/sys/class/video4linux"
                            "/sys/class/video4linux"
                            "--ro-bind-try" "/sys/devices" "/sys/devices"))
                         '())
                     '#$(if gpu?
                            '("--dev-bind-try" "/dev/dri" "/dev/dri"
                              "--ro-bind-try" "/sys/dev/char" "/sys/dev/char"
                              "--ro-bind-try" "/sys/devices" "/sys/devices")
                            '())
                     '#$(if network?
                            '("--share-net"
                              "--ro-bind-try" "/etc/resolv.conf" "/etc/resolv.conf"
                              "--ro-bind-try" "/etc/hosts" "/etc/hosts"
                              "--ro-bind-try" "/etc/ssl" "/etc/ssl")
                            '())
                     (append-map profile '#$%sandbox-profiles)
                     ;; Before the policy paths, which cover them; a file in
                     ;; a writable directory stays writable.
                     (if #$file-arguments?
                         (append-map file-argument (cdr (command-line)))
                         '())
                     (append-map (bind "--ro-bind-try") '#$read-only-paths)
                     (append-map (bind "--bind") '#$writable-paths)
                     (if (null? '#$dbus-rules)
                         '()
                         (session-bus '#$dbus-rules))
                     (list "--" program)
                     (cdr (command-line))))))))))

(define* (sandboxed base #:optional (policy (sandbox-policy))
                    #:key (output "out"))
  "Return a package with the programs and desktop entries of OUTPUT of BASE,
each program running BASE's under bubblewrap as POLICY allows."
  (sandboxed-package base output policy))

;; Read by package managers, from the package and from the built output.
(define (sandbox-metadata base base-output policy)
  (match-record policy <sandbox-policy>
    (read-only-paths writable-paths network? wayland? x11? gpu? audio? camera?
     user-namespaces? file-arguments? dbus-rules)
    `((base . ,(package-name base))
      (output . ,base-output)
      (policy . ((read-only-paths . ,read-only-paths)
                 (writable-paths . ,writable-paths)
                 (network? . ,network?)
                 (wayland? . ,wayland?)
                 (x11? . ,x11?)
                 (gpu? . ,gpu?)
                 (audio? . ,audio?)
                 (camera? . ,camera?)
                 (user-namespaces? . ,user-namespaces?)
                 (file-arguments? . ,file-arguments?)
                 (dbus-rules . ,dbus-rules))))))

;; Not named OUTPUT, which would shadow the builder's #$output.
(define (sandboxed-package base base-output policy)
  (package
    (name (string-append (package-name base) "-sandboxed"))
    (version (package-version base))
    (source #f)
    (build-system trivial-build-system)
    (arguments
     (list
      #:modules '((guix build utils))
      #:builder
      #~(begin
          (use-modules (guix build utils)
                       (ice-9 ftw)
                       (ice-9 regex))
          (let ((bin (string-append #$output "/bin"))
                (share (string-append #$output "/share"))
                (applications (string-append #$output "/share/applications"))
                (visible? (lambda (file)
                            (not (string-prefix? "." file)))))
            (mkdir-p bin)
            (for-each (lambda (program)
                        (symlink #$(sandbox-launcher base base-output policy)
                                 (string-append bin "/" program)))
                      (scandir (string-append (ungexp base base-output) "/bin") visible?))
            (when (file-exists? (string-append (ungexp base base-output) "/share"))
              (mkdir-p share)
              (for-each
               (lambda (entry)
                 (symlink (string-append (ungexp base base-output) "/share/" entry)
                          (string-append share "/" entry)))
               ;; D-Bus and systemd units would start the program unsandboxed.
               (scandir (string-append (ungexp base base-output) "/share")
                        (lambda (entry)
                          (and (visible? entry)
                               (not (member entry '("applications" "dbus-1"
                                                    "systemd"))))))))
            (when (file-exists? (string-append (ungexp base base-output) "/share/applications"))
              (copy-recursively (string-append (ungexp base base-output) "/share/applications")
                                applications)
              (substitute* (find-files applications "\\.desktop$")
                (((regexp-quote (string-append (ungexp base base-output) "/bin/")))
                 (string-append bin "/"))))
            (mkdir-p (string-append share "/sandbox"))
            (call-with-output-file
                (string-append share "/sandbox/" #$(package-name base) ".scm")
              (lambda (port)
                (write '#$(sandbox-metadata base base-output policy) port)
                (newline port)))))))
    (properties `((sandbox . ,(sandbox-metadata base base-output policy))))
    (home-page (package-home-page base))
    (synopsis (string-append (package-synopsis base) " (sandboxed)"))
    (description
     (string-append (package-description base)
                    "  This variant runs the program in a bubblewrap sandbox."))
    (license (package-license base))))
