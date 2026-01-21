;;; Package Repository for GNU Guix
;;; Copyright © 2025 Franz Geffke <mail@gofranz.com>

(define-module (px packages audio)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system meson)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages avahi)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages video)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xiph)
  #:use-module (px self))

(define-public libcamera-0.5
  (package
    (name "libcamera")
    (version "0.5.2")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/libcamera-org/libcamera")
         (commit (string-append "v" version))))
       (file-name
        (git-file-name name version))
       (sha256
        (base32 "0dhqgx63vgp3ish44v6fwlqwwkz0n2dqpxmpb20w2r4xfyd4pgcy"))))
    (build-system meson-build-system)
    (arguments
     (list #:configure-flags
           #~(list "-Dudev=enabled"
                   "-Dtest=false"
                   "-Dv4l2=true"
                   "-Dpycamera=disabled"
                   "-Dgstreamer=disabled"
                   "-Dcam=disabled"
                   "-Dqcam=disabled"
                   "-Ddocumentation=disabled")))
    (native-inputs
     (list pkg-config python python-jinja2 python-ply python-pyyaml))
    (inputs
     (list eudev libyaml))
    (home-page "https://libcamera.org/")
    (synopsis "Camera support library (version 0.5)")
    (description
     "libcamera is a library that deals with heavy hardware image processing
operations of complex camera devices that are shared between the linux host
all while allowing offload of certain aspects to the control of complex camera
hardware such as ISPs.  This is version 0.5.2, packaged for PipeWire compatibility.")
    (license license:lgpl2.1+)))

(define-public pipewire
  (package
    (name "pipewire")
    (version "1.4.9")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.freedesktop.org/pipewire/pipewire")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0mwn3aad8dg5ynxvgnvqi5wxqfy1xhhs2h0nf8awgsp5brihmkfz"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list (string-append "-Dudevrulesdir=" #$output "/lib/udev/rules.d")
              "-Dman=enabled"
              "-Drlimits-install=false"
              "-Dsession-managers=[]"
              "-Dsysconfdir=/etc"
              "-Dsystemd=disabled")))
    (native-inputs
     (list `(,glib "bin")
           pkg-config
           doxygen
           python
           python-docutils))
    (inputs (list alsa-lib
                  avahi
                  bluez
                  dbus
                  eudev
                  ffmpeg
                  gst-plugins-base
                  gstreamer
                  jack-2
                  ldacbt
                  libcamera-0.5
                  libdrm
                  libfdk
                  libfreeaptx
                  libsndfile
                  libusb
                  openssl
                  libva
                  pulseaudio
                  readline
                  sbc
                  vulkan-headers
                  vulkan-loader
                  webrtc-audio-processing))
    (home-page "https://pipewire.org/")
    (synopsis "Server and user space API to deal with multimedia pipelines")
    (description
     "PipeWire is a project that aims to greatly improve handling of audio and
video under Linux.  It aims to support the usecases currently handled by both
PulseAudio and Jack and at the same time provide same level of powerful handling
of Video input and output.  It also introduces a security model that makes
interacting with audio and video devices from containerized applications easy,
with supporting Flatpak applications being the primary goal.  Alongside Wayland
and Flatpak we expect PipeWire to provide a core building block for the future
of Linux application development.")
    (license license:lgpl2.0+)))

(define-public wireplumber
  (package
    (name "wireplumber")
    (version "0.5.13")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url
              "https://gitlab.freedesktop.org/pipewire/wireplumber.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0jk9a7i2c9sfh2i5jwa0pvngimndfl6s8b4g0cdblik69m7lq2l9"))))
    (build-system meson-build-system)
    (arguments
     `(#:configure-flags '("-Dsystemd=disabled"
                           "-Dsystem-lua=true")))
    (native-inputs
     (list `(,glib "bin")
           pkg-config
           python))
    (inputs (list dbus elogind glib lua pipewire))
    (home-page "https://gitlab.freedesktop.org/pipewire/wireplumber")
    (synopsis "Session / policy manager implementation for PipeWire")
    (description "WirePlumber is a modular session / policy manager for
PipeWire and a GObject-based high-level library that wraps PipeWire's API,
providing convenience for writing the daemon's modules as well as external
tools for managing PipeWire.")
    (license license:expat)))

(define-public easyeffects-presets-framework
  (package
    (name "easyeffects-presets-framework")
    (version "0.0.0-1.e5289ec")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/FrameworkComputer/linux-docs")
             (commit "e5289ecc283e0e940536ce48e0ed789adf0280be")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1l78wlxr1w1pgiv030qbz94p1mwjb7235abrz2j9hq43rywwj705"))))
    (build-system copy-build-system)
    (arguments
     '(#:install-plan
       '(("easy-effects/fw13-easy-effects.json"
          "share/easyeffects/output/fw13-easy-effects.json")
         ("easy-effects/fw16-easy-effects.json"
          "share/easyeffects/output/fw16-easy-effects.json")
         ("easy-effects/irs/IR_22ms_27dB_5t_15s_0c.irs"
          "share/easyeffects/irs/IR_22ms_27dB_5t_15s_0c.irs"))))
    (home-page "https://github.com/FrameworkComputer/linux-docs")
    (synopsis "EasyEffects presets for Framework laptops")
    (description
     "Audio presets for EasyEffects optimized for Framework 13 and Framework 16
laptops.  These presets improve speaker output quality on Framework hardware.")
    (license license:bsd-3)))

(define-public deepfilternet-ladspa
  (package
    (name "deepfilternet-ladspa")
    (version "0.5.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Rikorose/DeepFilterNet")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0rmips92mpky748dwgk66hhd3yv8k9wfc5fnkf6xp6b4xmy1pdp5"))
       (snippet
        #~(begin
            (use-modules (guix build utils))
            ;; Remove workspace members that require hdf5 git dependency
            (substitute* "Cargo.toml"
              (("\"pyDF\",") "")
              (("\"pyDF-data\",") "")
              (("\"demo\",") ""))
            ;; Remove hdf5 git dependency - it's optional and not needed for LADSPA
            (substitute* "libDF/Cargo.toml"
              (("hdf5 = \\{ optional = true, git.*\n") "")
              (("\"dep:hdf5\",") "")
              (("hdf5-static = \\[\"hdf5\\?/static\"\\]\n") ""))
            ;; Disable jemalloc - it requires /bin/sh which doesn't exist in Guix
            (substitute* "ladspa/Cargo.toml"
              (("\"use-jemalloc\",") ""))))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f
       #:tests? #f  ; Tests require features not enabled for LADSPA build
       #:cargo-build-flags '("--release" "-p" "deep-filter-ladspa")
       #:phases
       (modify-phases %standard-phases
         (add-after 'patch-cargo-checksums 'fix-time-crate
           ;; Fix type inference issue in time crate 0.3.28 for Rust 1.80+
           (lambda _
             (substitute* "guix-vendor/rust-time-0.3.28.tar.gz/src/format_description/parse/mod.rs"
               (("let items = format_items")
                "let items: Box<[_]> = format_items"))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((ladspa (string-append (assoc-ref outputs "out")
                                          "/lib/ladspa")))
               (mkdir-p ladspa)
               (install-file "target/release/libdeep_filter_ladspa.so"
                             ladspa)))))))
    (inputs (px-cargo-inputs 'deep-filter-ladspa))
    (home-page "https://github.com/Rikorose/DeepFilterNet")
    (synopsis "Real-time noise suppression LADSPA plugin")
    (description
     "DeepFilterNet is a deep learning-based noise suppression filter.  This
package provides the LADSPA plugin for real-time audio processing, suitable
for use with PipeWire and other LADSPA-compatible audio systems.")
    (license (list license:expat license:asl2.0))))
