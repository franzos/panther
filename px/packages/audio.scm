;;; Package Repository for GNU Guix
;;; Copyright © 2025 Franz Geffke <mail@gofranz.com>

(define-module (px packages audio)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system copy)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
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
