;;; Package Repository for GNU Guix
;;; Copyright © 2026 Franz Geffke <mail@gofranz.com>

(define-module (px packages fastflowlm)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages algebra)       ;; fftw
  #:use-module (gnu packages video)         ;; ffmpeg
  #:use-module (gnu packages readline)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages ninja)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages linux)         ;; util-linux (uuid)
  #:use-module (gnu packages instrumentation) ;; systemtap
  #:use-module (gnu packages opencl)       ;; opencl-headers
  #:use-module (gnu packages tls)          ;; openssl
  #:use-module (gnu packages web)          ;; rapidjson
  #:use-module (gnu packages xdisorg)       ;; libdrm
  #:use-module (gnu packages rust)
  #:use-module (gnu packages version-control) ;; git-minimal
  #:use-module (gnu packages elf)           ;; patchelf
  #:use-module (gnu packages textutils)    ;; oniguruma
  #:use-module (px packages rust)
  #:use-module (px self))

;; Long-double precision FFTW (not available in Guix upstream).
;; SIMD flags (SSE2, AVX, etc.) are incompatible with long-double.
(define fftwl
  (package/inherit fftw
    (name "fftwl")
    (arguments
     (substitute-keyword-arguments (package-arguments fftw)
       ((#:configure-flags fftw-configure-flags)
        `(cons "--enable-long-double"
               (filter (lambda (f)
                         (not (or (string-prefix? "--enable-sse" f)
                                  (string-prefix? "--enable-avx" f))))
                       ,fftw-configure-flags)))))
    (description
     "FFTW is a C subroutine library for computing the discrete Fourier
transform (DFT).  This package provides the long-double precision variant.")))

;; XRT public API headers (Apache-2.0), needed at compile time.
;; The full XRT runtime is proprietary and must be installed separately.
(define xrt-headers
  (package
    (name "xrt-headers")
    (version "2.20.197")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/Xilinx/XRT/archive/refs/tags/202520."
             version ".tar.gz"))
       (sha256
        (base32 "0i3cvfrjyv8wy5c1kvms5z41y7cwshbqhaid1sirr6ssa93g1brc"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (delete 'build)
          (delete 'check)
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((include (string-append (assoc-ref outputs "out")
                                           "/include")))
                (copy-recursively "src/runtime_src/core/include" include)
                ;; Remove non-header content
                (for-each (lambda (f)
                            (when (file-exists? f)
                              (if (file-is-directory? f)
                                  (delete-file-recursively f)
                                  (delete-file f))))
                          (list (string-append include "/unittests")
                                (string-append include "/windows")
                                (string-append include
                                               "/CMakeLists.txt")))
                ;; FastFlowLM includes "xrt/experimental/xrt_ext.h";
                ;; ensure the path exists.
                (unless (file-exists?
                         (string-append include "/xrt/experimental"))
                  (symlink "../experimental"
                           (string-append include
                                         "/xrt/experimental")))))))))
    (home-page "https://github.com/Xilinx/XRT")
    (synopsis "AMD XRT public API headers")
    (description
     "Public API header files from AMD Xilinx Runtime (XRT), used for building
software that targets AMD FPGAs and NPUs.")
    (license license:asl2.0)))

;; XRT userspace libraries (Apache-2.0).  Builds the open-source runtime
;; components: libxrt_coreutil.so, libxrt_core.so, libxrt++.so, xclbinutil,
;; etc.  The proprietary NPU driver (libxrt_driver_xdna.so) is NOT included.
(define-public xrt
  (package
    (name "xrt")
    (version "2.20.197")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Xilinx/XRT")
             (commit (string-append "202520." version))
             (recursive? #t)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0kjc4b1svmykfmbbj666l3098v5l0byqxan0d8jqm550vvhr2ivh"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f
      #:configure-flags
      #~(list "-DXRT_BASE=ON"
              "-DXRT_ALVEO=OFF"
              "-DXRT_EDGE=OFF"
              "-DXRT_NPU=OFF"
              "-DXRT_STATIC_BUILD=OFF"
              "-DXRT_UPSTREAM=ON"
              "-DXRT_ENABLE_HIP=OFF")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'chdir-to-src
            (lambda _
              ;; CMake root is src/CMakeLists.txt.  Remove the
              ;; top-level build/ directory so cmake-build-system
              ;; can create its own ../build without conflict.
              (when (file-exists? "build")
                (delete-file-recursively "build"))
              (chdir "src")))
          (add-after 'chdir-to-src 'skip-python-bindings
            (lambda _
              (substitute* "CMake/nativeLnx.cmake"
                (("xrt_add_subdirectory\\(python\\)") ""))))
          (add-after 'chdir-to-src 'fix-boost-system
            (lambda _
              ;; boost_system is header-only since Boost 1.86; its cmake
              ;; component file no longer exists.  Remove it from the
              ;; REQUIRED COMPONENTS list.
              (substitute* "CMake/boostUtil.cmake"
                (("system filesystem") "filesystem"))))
          (add-after 'chdir-to-src 'fix-icd-install-path
            (lambda _
              ;; XRT_BASE installs an OpenCL ICD file to /etc which
              ;; fails in the sandbox.  Redirect to the output prefix.
              (substitute* "CMake/icd.cmake"
                (("\"/etc/OpenCL/vendors\"")
                 "\"${CMAKE_INSTALL_PREFIX}/etc/OpenCL/vendors\"")))))))
    (native-inputs
     (list cmake git-minimal pkg-config opencl-headers systemtap))
    (inputs
     (list boost libdrm ncurses ocl-icd (list util-linux "lib")
           openssl rapidjson))
    (home-page "https://github.com/Xilinx/XRT")
    (synopsis "AMD Xilinx Runtime userspace libraries")
    (description
     "Open-source userspace libraries from AMD Xilinx Runtime (XRT), providing
@code{libxrt_coreutil.so}, @code{libxrt_core.so}, @code{libxrt++.so}, and
tools like @command{xclbinutil}.  Required at runtime by software targeting
AMD FPGAs and NPUs.")
    (license license:asl2.0)))

;; Pre-built Rust tokenizer C FFI library, used by the cmake build.
;; The tokenizers-cpp submodule invokes cargo internally, which fails in the
;; Guix sandbox (no network). Building this separately lets cargo-build-system
;; handle crate vendoring.
(define rust-tokenizers-c
  (package
    (name "rust-tokenizers-c")
    (version "0.9.34")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/FastFlowLM/FastFlowLM")
             (commit (string-append "v" version))
             (recursive? #t)))
       (file-name (git-file-name "fastflowlm" version))
       (sha256
        (base32 "0vfidxp0f03bj203v9qjrwmyrh60cxd9ysy7syasjxnalixpawy1"))))
    (build-system cargo-build-system)
    (arguments
     (list
      #:rust rust-1.89
      #:install-source? #f
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'chdir-to-rust
            (lambda _
              (chdir "third_party/tokenizers-cpp/rust")))
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((lib (string-append (assoc-ref outputs "out") "/lib")))
                (mkdir-p lib)
                (install-file "target/release/libtokenizers_c.a" lib)))))))
    (native-inputs
     (list pkg-config))
    (inputs
     (append (list oniguruma)
             (px-cargo-inputs 'tokenizers-c)))
    (home-page "https://github.com/mlc-ai/tokenizers-cpp")
    (synopsis "Rust tokenizer C FFI for FastFlowLM")
    (description
     "Pre-built Rust tokenizer C FFI static library used by the tokenizers-cpp
submodule in FastFlowLM.")
    (license license:asl2.0)))

(define-public fastflowlm
  (package
    (name "fastflowlm")
    (version "0.9.34")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/FastFlowLM/FastFlowLM")
             (commit (string-append "v" version))
             (recursive? #t)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0vfidxp0f03bj203v9qjrwmyrh60cxd9ysy7syasjxnalixpawy1"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f
      #:validate-runpath? #f
      #:build-type "Release"
      #:configure-flags
      #~(list "-GNinja")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'chdir-to-src
            (lambda _
              (chdir "src")
              (setenv "FASTFLOWLM_SRC" (getcwd))))

          (add-after 'chdir-to-src 'patch-xrt-paths
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((xrt (assoc-ref inputs "xrt")))
                (substitute* "CMakeLists.txt"
                  (("/opt/xilinx/xrt/include")
                   (string-append xrt "/include"))
                  (("/opt/xilinx/xrt/lib")
                   (string-append xrt "/lib"))))))

          (add-after 'chdir-to-src 'patch-tokenizers-skip-cargo
            (lambda* (#:key inputs #:allow-other-keys)
              ;; Make submodule writable and patch cmake to use a no-op
              ;; instead of cargo. The pre-built library is injected after
              ;; configure into the build directory.
              (for-each make-file-writable
                        (find-files "../third_party" ".*"))
              (substitute* "../third_party/tokenizers-cpp/CMakeLists.txt"
                (("# Find cargo executable")
                 "set(CARGO_EXECUTABLE \"true\")\n# Find cargo executable")
                ;; Our pre-built libtokenizers_c.a links oniguruma
                ;; dynamically; add -lonig so the final binary finds it.
                (("set\\(TOKENIZERS_C_LINK_LIBS \"\"\\)")
                 "set(TOKENIZERS_C_LINK_LIBS \"onig\")"))))

          (add-after 'configure 'inject-prebuilt-tokenizers
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((lib (string-append
                          (assoc-ref inputs "rust-tokenizers-c")
                          "/lib/libtokenizers_c.a")))
                (mkdir-p "tokenizers-cpp/release")
                (copy-file lib "tokenizers-cpp/release/libtokenizers_c.a")
                (copy-file lib "tokenizers-cpp/libtokenizers_c.a"))))

          (add-after 'install 'install-bundled-libs
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((src (getenv "FASTFLOWLM_SRC"))
                     (out (assoc-ref outputs "out"))
                     (lib (string-append out "/lib"))
                     (share (string-append out "/share/flm")))
                ;; Install proprietary NPU kernel .so files
                (mkdir-p lib)
                (for-each (lambda (f)
                            (install-file f lib))
                          (find-files (string-append src "/lib") "\\.so$"))
                ;; Install xclbin NPU bitstreams
                (copy-recursively (string-append src "/xclbins")
                                  (string-append share "/xclbins"))
                ;; Install model list next to the binary — flm resolves
                ;; it relative to its own executable path.
                (let ((model-list (string-append src "/model_list.json"))
                      (bin (string-append out "/bin")))
                  (when (file-exists? model-list)
                    (install-file model-list bin))))))

          (add-after 'install-bundled-libs 'fix-rpath
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (bin (string-append out "/bin/flm"))
                     (lib (string-append out "/lib"))
                     (xrt-lib (string-append (assoc-ref inputs "xrt")
                                             "/lib"))
                     (rpath (string-append lib ":" xrt-lib)))
                ;; Patch the main binary.
                (when (file-exists? bin)
                  (invoke "patchelf" "--add-rpath" rpath bin))
                ;; Patch bundled NPU .so files so they find
                ;; libxrt_coreutil.so.2 at runtime.
                (for-each (lambda (so)
                            (invoke "patchelf" "--add-rpath" xrt-lib so))
                          (find-files lib "\\.so$"))))))))
    (native-inputs
     (list cmake ninja pkg-config patchelf))
    (inputs
     (list boost curl fftw fftwf fftwl ffmpeg readline ncurses
           (list util-linux "lib") libdrm oniguruma
           rust-tokenizers-c xrt))
    (supported-systems '("x86_64-linux"))
    (home-page "https://github.com/FastFlowLM/FastFlowLM")
    (synopsis "LLM runtime for AMD Ryzen AI NPUs")
    (description
     "FastFlowLM is an NPU-first LLM runtime for AMD Ryzen AI NPUs (XDNA2
architecture).  It supports Llama, Qwen, Gemma, Phi-4, DeepSeek, Whisper and
other models running on Strix, Strix Halo and Kraken processors.  This
package bundles proprietary NPU kernel libraries and requires the AMD XRT
runtime (@code{libxrt-npu2}) to be installed separately.")
    (license license:expat)))
