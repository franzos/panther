;;; Package Repository for GNU Guix
;;; Copyright © 2021-2025 Franz Geffke <mail@gofranz.com>

(define-module (px packages linux)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system linux-module)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix platform)
  #:use-module (guix utils)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bootloaders)
  #:use-module (gnu packages calendar)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages readline)
  #:use-module (gnu system)
  #:use-module (ice-9 match)
  #:use-module (nongnu packages linux)
  #:use-module (nonguix licenses)
  #:export (%reterminal-kernel-modules
            wireless-regdb-signed))

(define-public bluez
  (package
    (name "bluez")
    (version "5.85")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kernel.org/linux/bluetooth/bluez-"
                    version ".tar.xz"))
              (sha256
               (base32
                "0vsav450lfxv2npfhyypa2k2pl339j8fg3zh2cd5bi2b4m4qw0md"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list "--sysconfdir=/etc"
              "--localstatedir=/var"
              "--enable-library"
              "--enable-wiimote"
              "--disable-systemd"
              ;; TODO: is this needed?  Not installed by default since 5.55.
              "--enable-hid2hci"
              ;; Install dbus/udev files to the correct location.
              (string-append "--with-dbusconfdir=" #$output "/etc")
              (string-append "--with-dbussessionbusdir=" #$output "/share/dbus-1/services")
              (string-append "--with-udevdir=" #$output "/lib/udev"))
      #:phases
      #~(modify-phases %standard-phases
          ;; Test unit/test-gatt fails unpredictably. Seems to be a timing
          ;; issue (discussion on upstream mailing list:
          ;; https://marc.info/?t=149578476300002&r=1&w=2)
          (add-before 'check 'skip-wonky-tests
            (lambda _
              (substitute* "unit/test-gatt.c"
                (("tester_init\\(&argc, &argv\\);") "return 77;"))
              ;; Test unit/test-vcp also fails (exit status 139)
              (substitute* "unit/test-vcp.c"
                (("tester_init\\(&argc, &argv\\);") "return 77;"))))
          (replace 'install
            (lambda* (#:key make-flags #:allow-other-keys #:rest args)
              ;; Override the sysconfdir and localstatedir locations only for
              ;; the installation phase.  Otherwise, the installation fails when
              ;; it tries to write to /etc/bluetooth and /var.
              (define make-flags*
                (append make-flags (list (string-append "sysconfdir="
                                                        #$output "/etc")
                                         (string-append "localstatedir="
                                                        #$output "/var"))))
              (apply (assoc-ref %standard-phases 'install)
                     (append args (list #:make-flags make-flags*)))))
          (add-after 'install 'post-install
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((servicedir (string-append #$output
                                                "/share/dbus-1/services"))
                     (service    "obexd/src/org.bluez.obex.service")
                     (rule       (string-append
                                  #$output "/lib/udev/rules.d/97-hid2hci.rules")))
                ;; Install the obex dbus service file.
                (substitute* service
                  (("/bin/false")
                   (string-append #$output "/libexec/bluetooth/obexd")))
                (install-file service servicedir)
                ;; Fix paths in the udev rule.
                (substitute* rule
                  (("hid2hci --method")
                   (string-append #$output "/lib/udev/hid2hci --method"))
                  (("/sbin/udevadm")
                   (search-input-file inputs "/bin/udevadm")))))))))
    (native-inputs
     (list gettext-minimal
           pkg-config
           python
           python-docutils
           python-pygments))
    (inputs
     (list glib dbus eudev libical readline))
    (home-page "https://www.bluez.org/")
    (synopsis "Linux Bluetooth protocol stack")
    (description
     "BlueZ provides support for the core Bluetooth layers and protocols.  It
is flexible, efficient and uses a modular implementation.")
    (license license:gpl2+)))

(define-public brcm80211-firmware
  (package
    (name "brcm80211-firmware")
    (version "20210818-1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://ftp.debian.org/debian/pool/non-free/f/firmware-nonfree/firmware-brcm80211_"
             version "_all.deb"))
       (sha256
        (base32 "04wg9fqay6rpg80b7s4h4g2kwq8msbh81lb3nd0jj45nnxrdxy7p"))))
    (build-system copy-build-system)
    (native-inputs (list tar bzip2))
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (replace 'unpack
                    (lambda* (#:key inputs #:allow-other-keys)
                      (let ((source (assoc-ref inputs "source")))
                        (invoke "ar" "x" source)
                        (invoke "ls")
                        (invoke "tar" "-xvf" "data.tar.xz"))))
                  (add-after 'install 'make-symlinks
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out")))
                        (symlink (string-append out
                                  "/lib/firmware/brcm/brcmfmac43455-sdio.raspberrypi,4-model-b.txt")
                                 (string-append out
                                  "/lib/firmware/brcm/brcmfmac43455-sdio.txt"))
                        (symlink (string-append out
                                  "/lib/firmware/brcm/brcmfmac43455-sdio.bin")
                                 (string-append out
                                  "/lib/firmware/brcm/brcmfmac43455-sdio.raspberrypi,4-compute-module.bin"))))))
       #:install-plan '(("lib/firmware/" "lib/firmware"))))
    (home-page "https://packages.debian.org/sid/firmware-brcm80211")
    (synopsis "Binary firmware for Broadcom/Cypress 802.11 wireless cards")
    (description "This package contains the binary firmware for wireless
network cards supported by the brcmsmac or brcmfmac driver.")
    (license license:expat)))

;; https://issues.guix.gnu.org/49649
(define-public wireless-regdb-signed
  (package
    (inherit wireless-regdb)
    (name "wireless-regdb-signed")
    (source
     (origin
       (inherit (package-source wireless-regdb))
       ;; Don't delete regulatory.bin - keep all upstream pre-built files
       (snippet #f)))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          ;; Skip build entirely - use upstream pre-built signed files
          (replace 'build
            (lambda _ #t))
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (firmware-dir (string-append out "/lib/firmware"))
                     (crda-dir (string-append out "/lib/crda"))
                     (doc-dir (string-append out "/share/doc/wireless-regdb"))
                     (man5 (string-append out "/share/man/man5")))
                (mkdir-p firmware-dir)
                (mkdir-p crda-dir)
                (mkdir-p doc-dir)
                (mkdir-p man5)
                ;; Install regulatory.db and signature for cfg80211
                (install-file "regulatory.db" firmware-dir)
                (install-file "regulatory.db.p7s" firmware-dir)
                ;; Install regulatory.bin for legacy CRDA
                (install-file "regulatory.bin" crda-dir)
                (install-file "sforshee.key.pub.pem"
                              (string-append crda-dir "/pubkeys"))
                ;; Documentation
                (install-file "LICENSE" doc-dir)
                (install-file "regulatory.db.5" man5)
                (install-file "regulatory.bin.5" man5)))))
      #:tests? #f))
    (native-inputs '())  ;; No build dependencies needed
    (synopsis "Wireless regulatory database with upstream signature")
    (description
     "This package contains the wireless regulatory database for the Linux
cfg80211 subsystem, including the upstream cryptographic signature required
by modern kernels (4.15+) to load the regulatory database.")))

(define-public bluez-firmware
  (let ((commit "31ad68831357d2019624004f1f0846475671088f")
        (revision "1"))
    (package
      (name "bluez-firmware")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/RPi-Distro/bluez-firmware.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "083s9n7kgyqjpr8jk6xw97fszzqpp3ndss3wjxn7c3snl47m9cy2"))))
      (build-system copy-build-system)
      (arguments
       '(#:install-plan '(("broadcom/BCM4345C0.hcd"
                           "lib/firmware/brcm/BCM4345C0.hcd"))))
      (home-page "https://github.com/RPi-Distro/bluez-firmware")
      (synopsis "Bluetooth firmware")
      (description "BlueZ - Bluetooth protocol stack for Linux")
      (license license:expat))))

(define (config->string options)
  (string-join (map (match-lambda
                      ((option quote m)
                       (string-append option "=m"))
                      ((option . #t) (string-append option "=y"))
                      ((option . #f) (string-append option "=n"))) options)
               "\n"))

(define %default-extra-linux-options
  `( ;Some very mild hardening.
     ("CONFIG_SECURITY_DMESG_RESTRICT" . #t)
    ;; All kernels should have NAMESPACES options enabled
    ("CONFIG_NAMESPACES" . #t)
    ("CONFIG_UTS_NS" . #t)
    ("CONFIG_IPC_NS" . #t)
    ("CONFIG_USER_NS" . #t)
    ("CONFIG_PID_NS" . #t)
    ("CONFIG_NET_NS" . #t)
    ;; Various options needed for elogind service:
    ;; https://issues.guix.gnu.org/43078
    ("CONFIG_CGROUP_FREEZER" . #t)
    ("CONFIG_BLK_CGROUP" . #t)
    ("CONFIG_CGROUP_WRITEBACK" . #t)
    ("CONFIG_CGROUP_SCHED" . #t)
    ("CONFIG_CGROUP_PIDS" . #t)
    ("CONFIG_CGROUP_FREEZER" . #t)
    ("CONFIG_CGROUP_DEVICE" . #t)
    ("CONFIG_CGROUP_CPUACCT" . #t)
    ("CONFIG_CGROUP_PERF" . #t)
    ("CONFIG_SOCK_CGROUP_DATA" . #t)
    ("CONFIG_BLK_CGROUP_IOCOST" . #t)
    ("CONFIG_CGROUP_NET_PRIO" . #t)
    ("CONFIG_CGROUP_NET_CLASSID" . #t)
    ("CONFIG_MEMCG" . #t)
    ("CONFIG_MEMCG_SWAP" . #t)
    ("CONFIG_MEMCG_KMEM" . #t)
    ("CONFIG_CPUSETS" . #t)
    ("CONFIG_PROC_PID_CPUSET" . #t)
    ;; Allow disk encryption by default
    ("CONFIG_DM_CRYPT" . m)
    ;; Modules required for initrd:
    ("CONFIG_NET_9P" . m)
    ("CONFIG_NET_9P_VIRTIO" . m)
    ("CONFIG_VIRTIO_BLK" . m)
    ("CONFIG_VIRTIO_NET" . m)
    ("CONFIG_VIRTIO_PCI" . m)
    ("CONFIG_VIRTIO_BALLOON" . m)
    ("CONFIG_VIRTIO_MMIO" . m)
    ("CONFIG_FUSE_FS" . m)
    ("CONFIG_CIFS" . m)
    ("CONFIG_9P_FS" . m)))

(define-public linux-raspberry-5.15
  (package
    (inherit linux-libre-5.15)
    (name "linux-raspberry")
    (version "5.15.32")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/raspberrypi/linux")
             (commit "1.20220331")))
       (file-name (string-append "linux-" version))
       (sha256
        (base32 "1k18cwnsqdy5ckymy92kp8czckzwgn8wn2zdibzrrg9jxrflx6vl"))))
    (supported-systems '("aarch64-linux"))
    (arguments
     (substitute-keyword-arguments (package-arguments linux-libre-5.15)
       ((#:phases phases)
        #~(modify-phases #$phases

            (replace 'configure
              (lambda* (#:key inputs target #:allow-other-keys)
                ;; Avoid introducing timestamps
                (setenv "KCONFIG_NOTIMESTAMP" "1")
                (setenv "KBUILD_BUILD_TIMESTAMP"
                        (getenv "SOURCE_DATE_EPOCH"))

                ;; Other variables useful for reproducibility.
                (setenv "KBUILD_BUILD_USER" "guix")
                (setenv "KBUILD_BUILD_HOST" "guix")

                ;; Set ARCH and CROSS_COMPILE.
                (let ((arch #$(platform-linux-architecture (lookup-platform-by-target-or-system
                                                            (or (%current-target-system)
                                                                (%current-system))))))
                  (setenv "ARCH" arch)
                  (format #t "`ARCH' set to `~a'~%"
                          (getenv "ARCH"))

                  (when target
                    (setenv "C_INCLUDE_PATH"
                            (string-join (cdr (string-split (getenv
                                                             "C_INCLUDE_PATH")
                                                            #\:)) ":"))

                    (setenv "CPLUS_INCLUDE_PATH"
                            (string-join (cdr (string-split (getenv
                                                             "CPLUS_INCLUDE_PATH")
                                                            #\:)) ":"))

                    (setenv "LIBRARY_PATH"
                            (string-join (cdr (string-split (getenv
                                                             "LIBRARY_PATH")
                                                            #\:)) ":"))
                    (setenv "CROSS_COMPILE"
                            (string-append target "-"))
                    (format #t "`CROSS_COMPILE' set to `~a'~%"
                            (getenv "CROSS_COMPILE"))))
                (setenv "KERNEL" "kernel8")
                (invoke "make" "bcm2711_defconfig")
                (let ((port (open-file ".config" "a"))
                      (extra-configuration #$(config->string
                                              %default-extra-linux-options)))
                  (display extra-configuration port)
                  (close-port port))

                ))))))))

;;;
;;; reTerminal related modules
;;;

(define-public bq24179-charger-linux-module
  (package
    (name "bq24179-charger-linux-module")
    (version "0.1")
    (source
     (file-append (origin
                    (method git-fetch)
                    (uri (git-reference (url
                                         "https://github.com/Seeed-Studio/seeed-linux-dtoverlays.git")
                                        (commit
                                         "589dab165f7a55eec0cc5fa25cc0bf892f4aa52c")))
                    (file-name (git-file-name name version))
                    (sha256 (base32
                             "002y8x0dmglhfgm60az6059jjnfm5q1zxdfp0b4s8dqybhjbdhb5")))
                  "/modules/bq24179_charger"))
    (build-system linux-module-build-system)
    (arguments
     (list
      #:tests? #f
      #:linux linux-raspberry-5.15)) ;no test suite, RPI Linux
    (home-page
     "https://github.com/Seeed-Studio/seeed-linux-dtoverlays/tree/master/modules/bq24179_charger")
    (synopsis
     "Linux kernel module for bq24179_charger found in Seeed Studio ReTerminal")
    (description "This is the Linux kernel bq24179_charger driver")
    (license license:gpl2)))

(define-public lis3lv02d-linux-module
  (package
    (name "lis3lv02d-linux-module")
    (version "0.1")
    (source
     (file-append (origin
                    (method git-fetch)
                    (uri (git-reference (url
                                         "https://github.com/Seeed-Studio/seeed-linux-dtoverlays.git")
                                        (commit
                                         "589dab165f7a55eec0cc5fa25cc0bf892f4aa52c")))
                    (file-name (git-file-name name version))
                    (sha256 (base32
                             "002y8x0dmglhfgm60az6059jjnfm5q1zxdfp0b4s8dqybhjbdhb5")))
                  "/modules/lis3lv02d"))
    (build-system linux-module-build-system)
    (arguments
     (list
      #:tests? #f
      #:linux linux-raspberry-5.15)) ;no test suite, RPI Linux
    (home-page
     "https://github.com/Seeed-Studio/seeed-linux-dtoverlays/tree/master/modules/lis3lv02d")
    (synopsis
     "Linux kernel module for GROVE 3-Axis Digital Accelerometer found in Seed Studio ReTerminal")
    (description
     "This is the Linux kernel GROVE 3-Axis Digital Accelerometer driver")
    (license license:gpl2)))

(define-public ltr30x-linux-module
  (package
    (name "ltr30x-linux-module")
    (version "0.1")
    (source
     (file-append (origin
                    (method git-fetch)
                    (uri (git-reference (url
                                         "https://github.com/Seeed-Studio/seeed-linux-dtoverlays.git")
                                        (commit
                                         "589dab165f7a55eec0cc5fa25cc0bf892f4aa52c")))
                    (file-name (git-file-name name version))
                    (sha256 (base32
                             "002y8x0dmglhfgm60az6059jjnfm5q1zxdfp0b4s8dqybhjbdhb5")))
                  "/modules/ltr30x"))
    (build-system linux-module-build-system)
    (arguments
     (list
      #:tests? #f
      #:linux linux-raspberry-5.15)) ;no test suite, RPI Linux
    (home-page
     "https://github.com/Seeed-Studio/seeed-linux-dtoverlays/tree/master/modules/ltr30x")
    (synopsis
     "Linux kernel module for ltr30x ambient light and proximity sensor found in Seed Studio ReTerminal")
    (description
     "This is the Linux kernel ltr30x ambient light and proximity sensor driver")
    (license license:gpl2)))

(define-public mipi_dsi-linux-module
  (package
    (name "mipi_dsi-linux-module")
    (version "0.1")
    (source
     (file-append (origin
                    (method git-fetch)
                    (uri (git-reference (url
                                         "https://github.com/Seeed-Studio/seeed-linux-dtoverlays.git")
                                        (commit
                                         "589dab165f7a55eec0cc5fa25cc0bf892f4aa52c")))
                    (file-name (git-file-name name version))
                    (sha256 (base32
                             "002y8x0dmglhfgm60az6059jjnfm5q1zxdfp0b4s8dqybhjbdhb5")))
                  "/modules/mipi_dsi"))
    (build-system linux-module-build-system)
    (arguments
     (list
      #:tests? #f
      #:linux linux-raspberry-5.15)) ;no test suite, RPI Linux
    (home-page
     "https://github.com/Seeed-Studio/seeed-linux-dtoverlays/tree/master/modules/mipi_dsi")
    (synopsis
     "Linux kernel module for MIPI-DSI panel with touch panel attached to I2C bus found in Seed Studio ReTerminal")
    (description
     "This is a linux kernel driver for MIPI-DSI panel with touch panel attached to I2C bus.")
    (license license:gpl2)))

(define-public seeed-reterminal-dtoverlays
  (package
    (name "seeed-reterminal-dtoverlays")
    (version "1.9")
    (license license:gpl2)
    (source
     (file-append (origin
                    (method git-fetch)
                    (uri (git-reference (url
                                         "https://github.com/Seeed-Studio/seeed-linux-dtoverlays.git")
                                        (commit
                                         "589dab165f7a55eec0cc5fa25cc0bf892f4aa52c")))
                    (file-name (git-file-name name version))
                    (sha256 (base32
                             "002y8x0dmglhfgm60az6059jjnfm5q1zxdfp0b4s8dqybhjbdhb5")))
                  "/overlays/rpi"))
    (build-system gnu-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (delete 'configure)
                  (add-before 'build 'set-prefix-in-makefile
                    (lambda* (#:key outputs #:allow-other-keys)
                      ;; Modify the makefile so that its
                      ;; 'DEST' variable points to "out".
                      (let ((out (assoc-ref outputs "out")))
                        (substitute* "Makefile"
                          (("DEST  :=.*")
                           (string-append "DEST  := " out "\n"))) #t)))
                  (add-before 'install 'create-dir
                    (lambda* (#:key outputs #:allow-other-keys)
                      (mkdir-p (assoc-ref outputs "out")))))
       #:tests? #f))
    (native-inputs `(("dtc" ,dtc)))
    (synopsis "Device Tree overlays for Seed Studio ReTerminal")
    (description "Device Tree overlays for Seed Studio ReTerminal")
    (home-page "https://github.com/Seeed-Studio/seeed-linux-dtoverlays/")))

(define %reterminal-kernel-modules
  (list bq24179-charger-linux-module lis3lv02d-linux-module
        ltr30x-linux-module mipi_dsi-linux-module))
