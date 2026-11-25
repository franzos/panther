;;; Package Repository for GNU Guix
;;; Copyright © 2026 Franz Geffke <mail@gofranz.com>

(define-module (px packages browser)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system qt)
  #:use-module (nonguix build-system chromium-binary)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages c)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages graphics)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image)
  #:use-module (gnu packages libedit)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages photo)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages video)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages web)
  #:use-module (gnu packages wget)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (px self))

(define-public thorium-browser
  (package
    (name "thorium-browser")
    (version "138.0.7204.303")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/Alex313031/thorium/releases/download/"
                    "M" version "/thorium-browser_" version "_AVX2.deb"))
              (sha256
               (base32 "163shilxjn5rwxc19alwp2gc96nmw0nrkxk529zl7c5hi9f5l1fz"))))
    (build-system chromium-binary-build-system)
    (arguments
     (list
       #:substitutable? #f
       #:wrapper-plan
        #~(let ((path "opt/chromium.org/thorium/"))
            (map (lambda (file)
                   (string-append path file))
                 '("thorium"
                   "chrome-sandbox"
                   "chrome_crashpad_handler"
                   "libEGL.so"
                   "libGLESv2.so"
                   "libqt5_shim.so"
                   "libqt6_shim.so"
                   "libvk_swiftshader.so"
                   "libvulkan.so.1"
                   "WidevineCdm/_platform_specific/linux_x64/libwidevinecdm.so")))
       #:install-plan
        #~'(("opt/" "/share")
            ("usr/share/" "/share"))
       #:phases
        #~(modify-phases %standard-phases
            (add-before 'install 'patch-assets
              (lambda _
                (let* ((bin (string-append #$output "/bin"))
                       (share (string-append #$output "/share"))
                       (opt "./opt")
                       (usr/share "./usr/share")
                       (old-exe "/opt/chromium.org/thorium/thorium-browser")
                       (exe (string-append bin "/thorium-browser")))
                  (substitute* (string-append opt "/chromium.org/thorium/thorium-browser")
                    (("CHROME_WRAPPER") "WRAPPER"))
                  (substitute* (string-append usr/share "/applications/thorium-browser.desktop")
                    (("^Exec=/usr/bin/thorium-browser") (string-append "Exec=" exe))
                    (("^Exec=/usr/bin/thorium-shell") (string-append "Exec=" exe " --content-shell")))
                  (substitute* (string-append usr/share "/gnome-control-center/default-apps/thorium-browser.xml")
                    ((old-exe) exe)))))
            (add-after 'install 'install-icons
               (lambda _
                 (define (format-icon-size name)
                   (car
                     (string-split
                      (string-drop-right (string-drop name 13) 4)
                      #\_)))
                 (let ((icons (string-append #$output "/share/icons/hicolor"))
                       (share (string-append #$output "/share/chromium.org/thorium")))
                   (for-each (lambda (icon)
                               (let* ((icon-name (basename icon))
                                      (icon-size (format-icon-size icon-name))
                                      (target (string-append icons "/" icon-size "x" icon-size "/apps/thorium-browser.png")))
                                 (mkdir-p (dirname target))
                                 (rename-file icon target)))
                             (find-files share "product_logo_.*\\.png")))))
            (add-before 'install-wrapper 'install-exe
             (lambda _
               (let* ((bin (string-append #$output "/bin"))
                      (exe (string-append bin "/thorium-browser"))
                      (share (string-append #$output "/share"))
                      (chrome-target (string-append share "/chromium.org/thorium/thorium-browser")))
                 (mkdir-p bin)
                 (symlink chrome-target exe)
                 (wrap-program exe
                   '("CHROME_WRAPPER" = ("thorium")))))))))
    (inputs
     (list bzip2
           curl
           flac
           font-liberation
           gdk-pixbuf
           gtk
           harfbuzz
           libexif
           libglvnd
           libpng
           libva
           libxscrnsaver
           opus
           pciutils
           pipewire
           qtbase-5
           qtbase
           snappy
           util-linux
           xdg-utils
           wget))
    (supported-systems '("x86_64-linux"))
    (home-page "https://thorium.rocks/")
    (synopsis "Performance-focused Chromium fork with AVX2 optimizations")
    (description "Thorium is a Chromium fork optimized for performance with compiler
optimizations (thinLTO, PGO, AVX2), all codecs enabled (including HEVC), Widevine
DRM support, and VA-API hardware video acceleration patches.")
    (license license:bsd-3)))

(define ladybird-commit "0be55a57cc07ed258362c631a13a10bb4f6d707a")

(define ladybird-source
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/LadybirdBrowser/ladybird")
          (commit "2026.47.8")))
    (file-name (git-file-name "ladybird" "2026.47.8"))
    (sha256
     (base32 "1l490jh24hmamh2qhqh2p4rhl17pmlv4g7vri0c0gyw8c922b318"))))

(define-public zmij
  (package
    (name "zmij")
    (version "1.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/vitaut/zmij")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1hdiaqpa5300i5iwch4sfszim83fp1gj9ilcwh9d5jwdwcm6knns"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f
      #:configure-flags
      #~(list "-DBUILD_SHARED_LIBS=ON" "-DZMIJ_TEST=OFF" "-DZMIJ_EXAMPLE=OFF")))
    (home-page "https://github.com/vitaut/zmij")
    (synopsis "Floating-point to string conversion library")
    (description
     "Zmij converts @code{double} and @code{float} values to their shortest
decimal representation that round-trips.")
    (license license:expat)))

(define-public mimalloc-2
  (package
    (inherit mimalloc)
    (version "2.2.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/microsoft/mimalloc")
             (commit (string-append "v" version))))
       (file-name (git-file-name "mimalloc" version))
       (sha256
        (base32 "0gdsf5n44kad22x53nkrm6p237s3kwqmr8chjmdl94866wwqrnng"))))
    (arguments
     (list
      #:build-type "Release"
      #:configure-flags #~(list "-DMI_SECURE=ON")))))

(define-public wuffs-0.3
  (package
    (name "wuffs")
    (version "0.3.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/google/wuffs-mirror-release-c")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "14w3wa1r7h4gsd9rhaina6p7hj292590fdd7g798j9fd4rl81jq0"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:install-plan
      #~'(("release/c/wuffs-v0.3.c" "include/wuffs/"))))
    (home-page "https://github.com/google/wuffs")
    (synopsis "Memory-safe decoders for image and compression formats")
    (description
     "Wuffs is a memory-safe programming language and a standard library of
file-format decoders written in it.  This package provides the single-file C
release of the 0.3 series.")
    (license license:asl2.0)))

(define-public llvm-libc-headers-for-ladybird
  (let ((commit "e54169423e7ee5d8b41c2f1b6f7a59000c901ee0")
        (revision "0"))
    (package
      (name "llvm-libc-headers-for-ladybird")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method url-fetch)
         (uri (string-append "https://github.com/llvm/llvm-project/archive/"
                             commit ".tar.gz"))
         (file-name (string-append "llvm-project-" commit ".tar.gz"))
         (sha256
          (base32 "074cc072ixlgs7l14ha1pllzqbd341wrq47i7y7125npa8byagnv"))))
      (build-system copy-build-system)
      (arguments
       (list
        #:install-plan
        #~(map (lambda (directory)
                 (list directory
                       (string-append "include/llvm-libc/" directory)
                       #:include-regexp '("\\.h$")))
               '("hdr" "include" "shared" "src/__support"
                 "src/string/memory_utils"))
        #:phases
        #~(modify-phases %standard-phases
            (replace 'unpack
              (lambda* (#:key source #:allow-other-keys)
                (let ((libc (string-append "llvm-project-" #$commit "/libc")))
                  (invoke "tar" "xf" source libc)
                  (chdir libc)))))))
      (home-page "https://libc.llvm.org")
      (synopsis "Header-only math functions of LLVM libc")
      (description
       "This package provides the headers of LLVM libc that implement its
math functions, at the revision Ladybird builds against.")
      (license license:asl2.0))))

(define-public curl-openssl
  (package
    (inherit curl)
    (name "curl-openssl")
    (arguments
     (substitute-keyword-arguments (package-arguments curl)
       ((#:tests? _ #f) #f)
       ((#:configure-flags flags)
        #~(cons* "--with-openssl" "--with-brotli" "--with-zstd"
                 (delete "--with-gnutls" #$flags)))))
    (inputs
     (modify-inputs (package-inputs curl)
       (delete "gnutls")
       (append brotli openssl
               `(,zstd "lib"))))
    (synopsis "curl built against OpenSSL, with Brotli and Zstandard")))

(define-public skia-for-ladybird
  (let ((milestone "148")
        (commit "e7c90ecca9444fe09598f1630ab7cee2c0ee027a")
        (revision "0"))
    (package
      (inherit skia)
      (name "skia-for-ladybird")
      (version (git-version milestone revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://skia.googlesource.com/skia.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0dls4zrz2czwzy04w2bcmy3jyk5a9iy66yd96xl2n18dm5dg3ryv"))))
      (arguments
       (list
        #:tests? #f
        #:phases
        #~(modify-phases %standard-phases
            (replace 'configure
              (lambda* (#:key inputs #:allow-other-keys)
                (substitute* "BUILD.gn"
                  (("rebase_path\\(\"//bin/gn\"\\)") "\"gn\"")
                  (("\"//third_party/zlib\",") ""))
                (invoke "gn" "gen" "build"
                        (string-append
                         "--args="
                         "cc=\"" #$(cc-for-target) "\" "
                         "cxx=\"" #$(cxx-for-target) "\" "
                         "is_official_build=true "
                         "is_component_build=true "
                         "skia_enable_tools=false "
                         "skia_use_dng_sdk=false "
                         "skia_use_wuffs=false "
                         "skia_use_vulkan=true "
                         (format #f "extra_cflags=[\"-I~a\",\"-I~a\",~a] "
                                 (search-input-directory inputs
                                                         "include/harfbuzz")
                                 (search-input-directory inputs
                                                         "include/freetype2")
                                 (string-append
                                  "\"-DSK_USE_EXTERNAL_VULKAN_HEADERS\","
                                  "\"-DSKCMS_DLL\""))
                         "extra_ldflags=[\"-Wl,-rpath=" #$output "/lib\"] "))))
            (replace 'build
              (lambda* (#:key parallel-build? #:allow-other-keys)
                (invoke "ninja" "-j"
                        (if parallel-build?
                            (number->string (parallel-job-count))
                            "1")
                        "-C" "build")))
            (replace 'install
              (lambda _
                (for-each (lambda (directory)
                            (for-each
                             (lambda (header)
                               (install-file header
                                             (string-append #$output
                                                            "/include/skia/"
                                                            (dirname header))))
                             (find-files directory "\\.h$")))
                          '("include" "modules" "src"))
                (for-each (lambda (lib)
                            (install-file lib (string-append #$output "/lib")))
                          (find-files "build" "^lib.*\\.so$"))
                (let ((skia.pc (string-append #$output
                                              "/lib/pkgconfig/skia.pc")))
                  (mkdir-p (dirname skia.pc))
                  (call-with-output-file skia.pc
                    (lambda (port)
                      (format port "\
prefix=~a
exec_prefix=${prefix}
libdir=${prefix}/lib
includedir=${prefix}/include/skia

Name: skia
Description: 2D graphic library for drawing text, geometries and images.
URL: https://skia.org/
Version: ~a
Libs: -L${libdir} -lskia
Cflags: -I${includedir} -I${includedir}/include~%"
                              #$output #$milestone)))))))))
      (native-inputs (list gn ninja pkg-config python-wrapper))
      (inputs
       (list expat
             fontconfig
             freetype
             harfbuzz
             icu4c-78
             libjpeg-turbo
             libpng
             libwebp
             mesa
             vulkan-headers
             vulkan-memory-allocator
             zlib)))))

;; Not an input of angle: CMake would find its zlib.h before the system's.
(define angle-chromium-zlib
  (origin
    (method git-fetch)
    (uri (git-reference
          (url (string-append "https://chromium.googlesource.com/"
                              "chromium/src/third_party/zlib"))
          (commit "4028ebf8710ee39d2286cb0f847f9b95c59f84d8")))
    (file-name "chromium-zlib-checkout")
    (sha256
     (base32 "017v4h2698csjjg598wp2zw5gd1d9zwb0z7fdig01qj7p4i0hi9h"))))

(define-public angle
  (let ((commit "d9fc4a372074b1079c193c422fc4a180e79b6636")
        (webkit-commit "0742522b24152262b04913242cb0b3c48de92ba0"))
    (package
      (name "angle")
      (version "7258")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://chromium.googlesource.com/angle/angle")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "11zn62pvnfl4d0kd00a1npf2ff898d4dbd0y5plqnmnxvrhihgab"))))
      (build-system cmake-build-system)
      (arguments
       (list
        #:tests? #f
        #:configure-flags
        #~(list "-DBUILD_SHARED_LIBS=ON"
                "-DANGLE_IS_64_BIT_CPU=1"
                "-DPORT=Linux"
                "-DUSE_METAL=OFF"
                "-DVCPKG_TARGET_IS_WINDOWS=OFF"
                "-DANGLE_USE_D3D11_COMPOSITOR_NATIVE_WINDOW=OFF")
        #:phases
        #~(modify-phases %standard-phases
            ;; ANGLE only ships a GN build; this is the CMake build of
            ;; WebKit as carried by Ladybird's vcpkg port.
            (add-after 'unpack 'patch-egl-path
              (lambda* (#:key inputs #:allow-other-keys)
                (substitute* "src/libANGLE/renderer/gl/egl/DisplayEGL.cpp"
                  (("\"libEGL\\.so\\.1\"")
                   (string-append "\""
                                  (search-input-file inputs "lib/libEGL.so.1")
                                  "\"")))))
            (add-after 'unpack 'add-cmake-build-system
              (lambda _
                (let ((port (string-append
                             #$ladybird-source
                             "/Meta/CMake/vcpkg/overlay-ports/angle"))
                      (gni-to-cmake #$(this-package-native-input
                                       "gni-to-cmake.py"))
                      (short (string-take #$commit 12)))
                  (for-each (lambda (patch)
                              (invoke "patch" "-p1" "-i"
                                      (string-append port "/" patch)))
                            '("001-fix-builder-error.patch"
                              "002-bsd-support.patch"))
                  (for-each
                   (lambda (file)
                     (call-with-output-file file
                       (lambda (out)
                         (format out "\
#define ANGLE_COMMIT_HASH \"~a\"
#define ANGLE_COMMIT_HASH_SIZE 12
#define ANGLE_COMMIT_DATE \"invalid-date\"
#define ANGLE_REVISION ~a~%" short #$version))))
                   '("angle_commit.h" "src/common/angle_commit.h"))
                  (for-each
                   (lambda (file)
                     (call-with-output-file file
                       (lambda (out)
                         (format out "\
#define ANGLE_PROGRAM_VERSION \"~a\"
#define ANGLE_PROGRAM_VERSION_HASH_SIZE 12~%" short))))
                   '("ANGLEShaderProgramVersion.h"
                     "src/common/ANGLEShaderProgramVersion.h"))
                  (for-each
                   (lambda (args)
                     (apply invoke "python3" gni-to-cmake args))
                   '(("src/compiler.gni" "Compiler.cmake")
                     ("src/libGLESv2.gni" "GLESv2.cmake")
                     ("src/libANGLE/renderer/d3d/BUILD.gn" "D3D.cmake"
                      "--prepend" "src/libANGLE/renderer/d3d/")
                     ("src/libANGLE/renderer/gl/BUILD.gn" "GL.cmake"
                      "--prepend" "src/libANGLE/renderer/gl/")
                     ("src/libANGLE/renderer/metal/BUILD.gn" "Metal.cmake"
                      "--prepend" "src/libANGLE/renderer/metal/")))
                  (copy-file #$(this-package-native-input
                                "angle-include-CMakeLists.txt")
                             "include/CMakeLists.txt")
                  (copy-file (string-append port
                                            "/unofficial-angle-config.cmake")
                             "unofficial-angle-config.cmake")
                  (for-each (lambda (file)
                              (install-file
                               (string-append port "/cmake-buildsystem/" file)
                               "."))
                            '("CMakeLists.txt" "linux.cmake"
                              "PlatformLinux.cmake"))
                  (copy-recursively (string-append port
                                                   "/cmake-buildsystem/cmake")
                                    "cmake")
                  ;; Only the wrapper: Chromium's zlib.h renames every
                  ;; symbol, and the library linked is the system's.
                  (copy-recursively
                   (string-append #$angle-chromium-zlib "/google")
                   "third_party/zlib/google")))))))
      (native-inputs
       (list pkg-config
             python
             python-ply
             (origin
               (method url-fetch)
               (uri (string-append "https://github.com/WebKit/WebKit/raw/"
                                   webkit-commit
                                   "/Source/ThirdParty/ANGLE/gni-to-cmake.py"))
               (file-name "gni-to-cmake.py")
               (sha256
                (base32
                 "1gb8c7z7v0sciak5alx7d4qmpvwy3ipmwmg0jlc5cafayycp0zv8")))
             (origin
               (method url-fetch)
               (uri (string-append
                     "https://github.com/WebKit/WebKit/raw/" webkit-commit
                     "/Source/ThirdParty/ANGLE/include/CMakeLists.txt"))
               (file-name "angle-include-CMakeLists.txt")
               (sha256
                (base32
                 "0xs4np5b9hrk7s71al66v0ghcq1dab8wf0g5ci9q0h44ai9mi1f7")))))
      (inputs (list libdrm libx11 libxcb libxext mesa zlib))
      (home-page "https://angleproject.org")
      (synopsis "OpenGL ES implementation on top of native graphics APIs")
      (description
       "ANGLE translates OpenGL ES API calls to the graphics API the platform
provides, such as desktop OpenGL.  Browsers use it to run WebGL content.")
      (license license:bsd-3))))

(define-public ffmpeg-for-ladybird
  (package
    (name "ffmpeg-for-ladybird")
    (version "9.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://ffmpeg.org/releases/ffmpeg-" version
                           ".tar.xz"))
       (sha256
        (base32 "0bh0dslibv4vhjh83vq9gcs1ggp0a50a0y1090ka0pxj7ql50f4c"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          ;; Ladybird's Vulkan video decoder depends on these changes.
          (add-after 'unpack 'apply-ladybird-patches
            (lambda _
              (for-each
               (lambda (patch)
                 (invoke "patch" "-p1" "-i"
                         (string-append
                          #$ladybird-source
                          "/Meta/CMake/vcpkg/overlay-ports/ffmpeg/" patch)))
               '("0100-ladybird-symbol-version-nodes.patch"
                 "0101-vulkan-decode-dpb-distinct-no-drm-modifiers.patch"
                 "0102-vulkan-av1-clear-unused-ref-frame-mvs.patch"))))
          (replace 'configure
            (lambda _
              (substitute* "configure"
                (("#! */bin/sh") (string-append "#!" (which "sh"))))
              (setenv "SHELL" (which "bash"))
              (setenv "CONFIG_SHELL" (which "bash"))
              (invoke
               "./configure"
               (string-append "--prefix=" #$output)
               (string-append "--cc=" #$(cc-for-target))
               (string-append "--extra-ldflags=-Wl,-rpath=" #$output "/lib")
               ;; The suffix lets a system FFmpeg load next to this one.
               "--build-suffix=-ladybird"
               "--enable-shared"
               "--disable-static"
               "--enable-pic"
               "--disable-doc"
               "--disable-programs"
               "--enable-runtime-cpudetect"
               "--disable-autodetect"
               "--enable-pthreads"
               "--disable-avdevice"
               "--disable-avfilter"
               "--disable-swscale"
               "--enable-swresample"
               "--disable-everything"
               "--disable-network"
               (string-append
                "--enable-decoder=vp8,vp9,av1,libdav1d,theora,vorbis,opus,"
                "flac,mp3float,pcm_u8,pcm_s16le,pcm_s24le,pcm_s32le,"
                "pcm_f32le,pcm_alaw,pcm_mulaw")
               "--enable-parser=vp8,vp9,av1,mpegaudio"
               "--enable-encoder=libopus"
               "--enable-hwaccel=vp9_vulkan,av1_vulkan"
               "--enable-libdav1d"
               "--enable-libdrm"
               "--enable-libopus"
               "--enable-vulkan"
               "--enable-zlib"))))))
    (native-inputs (list nasm pkg-config))
    (inputs (list dav1d libdrm opus vulkan-headers vulkan-loader zlib))
    (home-page "https://www.ffmpeg.org/")
    (synopsis "FFmpeg reduced to the codecs Ladybird decodes itself")
    (description
     "This is FFmpeg with Ladybird's changes to Vulkan video decoding, built
with only the decoders and parsers Ladybird uses.  Its libraries carry a
@code{-ladybird} suffix.")
    (license license:lgpl2.1+)))

;; The cargo phases take every plain-file input for a crate, so these two
;; are referenced from the phases instead of being inputs.
(define ladybird-hsts-preload-list
  (origin
    (method url-fetch)
    (uri (string-append
          "https://raw.githubusercontent.com/chromium/chromium/"
          "ba9567c435afe2bb2e71d1a348b3c65790a572db"
          "/net/http/transport_security_state_static.json"))
    (file-name "transport_security_state_static.json")
    (sha256
     (base32 "0c9zzcwyvpwv9i9c320i8jknarxldhr9kf7ivjy6fqwyr0s258ac"))))

(define ladybird-pdfjs
  (origin
    (method url-fetch)
    (uri (string-append "https://github.com/mozilla/pdf.js/releases/download/"
                        "v6.5.438/pdfjs-6.5.438-dist.zip"))
    (file-name "pdfjs-dist.zip")
    (sha256
     (base32 "18bb1i4yahn9xmxqhpq5lhjaajgmlxwc3b826lz884cr3ni9pgpw"))))

(define-public ladybird
  (package
    (name "ladybird")
    (version "2026.47.8")
    (source ladybird-source)
    (build-system qt-build-system)
    (arguments
     (list
      #:qtbase qtbase
      #:generator "Ninja"
      #:build-type "Release"
      #:tests? #f
      #:imported-modules `(,@%qt-build-system-modules
                           ,@%cargo-build-system-modules)
      #:modules '(((guix build cargo-build-system) #:prefix cargo:)
                  (guix build qt-build-system)
                  (guix build utils))
      #:configure-flags
      #~(let ((source (string-append (getcwd) "/source")))
          (list "-DCMAKE_C_COMPILER=clang"
                "-DCMAKE_CXX_COMPILER=clang++"
                (string-append "-DLADYBIRD_VERSION=" #$version)
                "-DENABLE_NETWORK_DOWNLOADS=OFF"
                (string-append "-DLADYBIRD_CACHE_DIR=" source "/caches")
                (string-append "-DPDFJS_DIR=" source "/pdfjs")
                (string-append "-DICU_ROOT=" #$(this-package-input "icu4c"))
                "-DLADYBIRD_ENABLE_UPDATE_CHECK=OFF"
                "-DLADYBIRD_BASELINE_CPU_TARGET=ON"
                "-DLADYBIRD_ENABLE_CCACHE=OFF"
                "-DENABLE_INSTALL_HEADERS=OFF"
                "-DBUILD_TESTING=OFF"
                "-DCMAKE_INSTALL_LIBEXECDIR=libexec"))
      #:phases
      #~(modify-phases %standard-phases
          ;; The cache resolves libpng16 to the libpng without APNG that
          ;; freetype brings in; the RUNPATH has the right one first.
          (delete 'make-dynamic-linker-cache)
          (add-after 'unpack 'use-system-libraries
            (lambda _
              ;; Compiler and library versions differ from upstream's CI.
              (substitute* "Meta/CMake/compile_options.cmake"
                (("add_cxx_compile_options\\(-Werror\\)") ""))
              (substitute* "Meta/CMake/rust_crate.cmake"
                (("set\\(RUSTC_TARGET_FLAGS -Dwarnings ")
                 "set(RUSTC_TARGET_FLAGS "))
              ;; The GPU process sandbox only allows FHS library
              ;; directories; Mesa and the Vulkan drivers live in the store.
              (substitute* "Libraries/LibSandbox/GpuDriverPaths.cpp"
                (("TRY\\(add_path_if_exists\\(paths, \"/usr/local/lib\"sv, PathRule::Access::ReadOnly\\)\\);"
                  all)
                 (string-append
                  all "\n    TRY(add_path_if_exists(paths, \"/gnu/store\"sv, "
                  "PathRule::Access::ReadOnly));")))
              ;; revision.cmake asks git for the commit otherwise.
              (call-with-output-file "REVISION"
                (lambda (port)
                  (display #$ladybird-commit port)))))
          (add-after 'unpack 'add-hsts-preload-list
            (lambda _
              (mkdir-p "caches/HSTSPreload")
              (copy-file #$ladybird-hsts-preload-list
                         (string-append
                          "caches/HSTSPreload/"
                          "transport_security_state_static.json"))))
          (add-after 'unpack 'add-pdfjs
            (lambda _
              (invoke "unzip" "-q" "-d" "pdfjs"
                      #$ladybird-pdfjs)
              (invoke "patch" "-d" "pdfjs" "-p1" "-i"
                      (string-append (getcwd) "/Meta/CMake/vcpkg/"
                                     "overlay-ports/pdfjs/"
                                     "0001-ladybird-embed.patch"))
              (call-with-output-file "pdfjs/web/locale/locale.json"
                (lambda (port)
                  (display "{\"en-us\":\"en-US/viewer.ftl\"}" port)))))
          (add-after 'unpack 'prepare-cargo-build-system
            (lambda args
              (for-each
               (lambda (phase)
                 (format #t "Running cargo phase: ~a~%" phase)
                 (apply (assoc-ref cargo:%standard-phases phase)
                        #:cargo-target #$(cargo-triplet)
                        args))
               '(prepare-rust-crates
                 unpack-rust-crates
                 configure
                 check-for-pregenerated-files
                 patch-cargo-checksums))
              ;; rust_crate.cmake loops forever looking for the lockfile
              ;; that the cargo phases delete.
              (invoke "cargo" "generate-lockfile" "--offline"))))))
    (native-inputs
     (list clang-22        ;GCC 14 leaves a consteval constructor undefined
           git-minimal
           glslang
           ninja
           pkg-config
           python
           rust
           `(,rust "cargo")
           unzip))
    (inputs
     (cons* angle
            cpptrace
            curl-openssl
            dbus
            fast-float
            ffmpeg-for-ladybird
            fontconfig
            glib
            harfbuzz
            icu4c-78
            libavif
            libdrm
            libedit
            libjpeg-turbo
            libpng-apng
            libpsl
            libwebp
            libxml2
            llvm-libc-headers-for-ladybird
            mesa
            mimalloc-2
            openssl
            pulseaudio
            qtpositioning
            qtwayland
            sdl3
            simdjson
            simdutf
            skia-for-ladybird
            sqlite-next                 ;3.39 fails with "SQL logic error"
            vulkan-headers
            vulkan-loader
            vulkan-memory-allocator
            wayland
            woff2
            wuffs-0.3
            xcb-util-wm
            zlib
            zmij
            (px-cargo-inputs 'ladybird)))
    (home-page "https://ladybird.org")
    (synopsis "Web browser with its own engine")
    (description
     "Ladybird is a web browser built on its own engine: HTML, CSS, the DOM,
layout, JavaScript and WebAssembly are implemented from scratch instead of
being derived from Chromium, WebKit or Gecko.  Documents from different sites
run in separate sandboxed processes.  The project is in alpha.")
    (license license:bsd-2)))
