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
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages polkit)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages vulkan)
  #:use-module (px self))

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

(define-public rtkit
  (package
    (name "rtkit")
    (version "0.14")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.freedesktop.org/pipewire/rtkit")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "07swd78aglawfddc1zjx5v8scm85q1qpmahjskh8hn6lfr47dpnb"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list "-Dlibsystemd=disabled"
              "-Dinstalled_tests=false"
              (string-append "-Ddbus_interfacedir="
                            #$output "/share/dbus-1/interfaces")
              (string-append "-Ddbus_rulesdir="
                            #$output "/share/dbus-1/system.d")
              (string-append "-Ddbus_systemservicedir="
                            #$output "/share/dbus-1/system-services")
              (string-append "-Dpolkit_actiondir="
                            #$output "/share/polkit-1/actions"))))
    (native-inputs
     (list pkg-config vim))
    (inputs
     (list dbus elogind libcap polkit))
    (home-page "https://gitlab.freedesktop.org/pipewire/rtkit")
    (synopsis "Realtime policy and watchdog daemon")
    (description
     "RealtimeKit is a D-Bus system service that changes the scheduling policy
of user processes/threads to @code{SCHED_RR} (realtime scheduling mode) on
request.  It is intended to be used as a secure mechanism to allow real-time
scheduling to be used by normal user processes.

RealtimeKit enforces strict policies when handing out real-time priority and
includes a canary-based watchdog that automatically demotes all real-time
threads to @code{SCHED_OTHER} should the system overload.  The daemon runs as
an unprivileged user and uses capabilities, resource limits, and chroot to
minimize its security impact.")
    (license license:gpl3+)))

(define-public voxtype
  (package
    (name "voxtype")
    (version "0.6.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/peteonrails/voxtype")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1rd2gdwyn1a4q84qwsi2yf8nxsdfcjlxlwldw0nyklnvyy4w1pq7"))
       (snippet
        #~(begin
            (use-modules (guix build utils))
            ;; Replace unstable is_multiple_of with stable modulo operator
            (substitute* "src/daemon.rs"
              (("count\\.is_multiple_of\\(120\\)")
               "count % 120 == 0"))))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f
       #:cargo-test-flags
       '("--release" "--"
         "--skip=test_network")))
    (native-inputs (list clang cmake git pkg-config))
    (inputs (cons* alsa-lib sqlite (px-cargo-inputs 'voxtype)))
    (home-page "https://github.com/peteonrails/voxtype")
    (synopsis "Push-to-talk voice-to-text for Wayland")
    (description
     "Voxtype is a voice-to-text application optimized for Linux.  Hold a hotkey
while speaking, release to transcribe and output the text at your cursor
position.  It supports multiple transcription engines including Whisper,
Parakeet, Moonshine, SenseVoice, Paraformer, Dolphin, and Omnilingual.
Features include meeting mode for continuous transcription, multilingual
support, and multiple output modes.  This package is built for CPU; GPU
acceleration (Vulkan, CUDA) is available but not yet enabled.")
    (license license:expat)))

(define-public voxtype-vulkan
  (package
    (inherit voxtype)
    (name "voxtype-vulkan")
    (arguments
     `(#:install-source? #f
       #:cargo-build-flags '("--release" "--features" "gpu-vulkan")
       #:cargo-test-flags
       '("--release" "--features" "gpu-vulkan" "--"
         "--skip=test_network")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-vulkan-detection
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((vulkan-loader (assoc-ref inputs "vulkan-loader")))
               (substitute* "src/setup/gpu.rs"
                 ;; Patch Vulkan runtime check to find libvulkan in the store
                 (("\"/usr/lib/libvulkan\\.so\\.1\"")
                  (string-append "\"" vulkan-loader "/lib/libvulkan.so.1\""))
                 ;; Always report Vulkan as active backend (compiled with
                 ;; gpu-vulkan feature; FHS path checks don't apply on Guix)
                 (("pub fn detect_current_backend\\(\\) -> Option<Backend> \\{")
                  "pub fn detect_current_backend() -> Option<Backend> {\n        return Some(Backend::Vulkan);")
                 (("pub fn detect_available_backends\\(\\) -> Vec<Backend> \\{")
                  "pub fn detect_available_backends() -> Vec<Backend> {\n        return vec![Backend::Vulkan];")
                 ;; Vulkan is always active; no binary switching needed
                 ;; Keep the let-binding so subsequent references compile
                 (("let vulkan_path = Path::new\\(VOXTYPE_LIB_DIR\\)\\.join\\(\"voxtype-vulkan\"\\);")
                  "println!(\"Vulkan GPU acceleration is already active in this build.\"); return Ok(());\n            let vulkan_path = Path::new(VOXTYPE_LIB_DIR).join(\"voxtype-vulkan\");")))))
         (add-after 'patch-cargo-checksums 'fix-shader-compilation
           (lambda _
             (substitute*
              "guix-vendor/rust-whisper-rs-sys-0.14.1.tar.gz/whisper.cpp/ggml/src/ggml-vulkan/vulkan-shaders/vulkan-shaders-gen.cpp"
              ;; The hardcoded /bin/sh path doesn't exist in the Guix build
              ;; sandbox.  Use execlp to search PATH for sh instead.
              (("execl\\(\"/bin/sh\", \"sh\"")
               "execlp(\"sh\", \"sh\""))))
         (add-after 'install 'wrap-binary
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (mesa (assoc-ref inputs "mesa")))
               (wrap-program (string-append out "/bin/voxtype")
                 ;; Point to hardware GPU Vulkan drivers
                 `("VK_ICD_FILENAMES" ":" prefix
                   (,(string-append mesa
                                    "/share/vulkan/icd.d/radeon_icd.x86_64.json")
                    ,(string-append mesa
                                    "/share/vulkan/icd.d/intel_icd.x86_64.json")
                    ,(string-append mesa
                                    "/share/vulkan/icd.d/nouveau_icd.x86_64.json"))))))))))
    (native-inputs
     (modify-inputs (package-native-inputs voxtype)
       (prepend shaderc vulkan-headers)))
    (inputs
     (modify-inputs (package-inputs voxtype)
       (prepend vulkan-loader mesa)))
    (synopsis "Push-to-talk voice-to-text for Wayland (Vulkan GPU)")
    (description
     "Voxtype is a voice-to-text application optimized for Linux.  Hold a hotkey
while speaking, release to transcribe and output the text at your cursor
position.  It supports multiple transcription engines including Whisper,
Parakeet, Moonshine, SenseVoice, Paraformer, Dolphin, and Omnilingual.
Features include meeting mode for continuous transcription, multilingual
support, and multiple output modes.  This package is built with Vulkan GPU
acceleration for AMD, NVIDIA, and Intel GPUs.")))
