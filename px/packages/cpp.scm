;;; Package Repository for GNU Guix
;;; Copyright © 2021-2025 Franz Geffke <mail@gofranz.com>

(define-module (px packages cpp)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix build-system qt)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages networking) ;; libnice
  #:use-module (gnu packages cpp) ;; cli11
  #:use-module (gnu packages compression) ;; xz
  #:use-module (gnu packages web) ;; rapidjson, websocketpp, gumbo-parser
  #:use-module (gnu packages tls) ;; openssl
  #:use-module (gnu packages networking) ;; asio
  #:use-module (px packages gstreamer) ;; gst-plugins-good-qmlgl
  #:use-module (gnu packages check) ;; googletest
  #:use-module (guix git-download)
  #:use-module (px packages monitoring) ;; sentry
  #:use-module (ice-9 match))

(define-public cpp-qr-code-generator
  (package
    (name "qr-code-generator")
    (version "1.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/nayuki/QR-Code-generator/archive/refs/tags/v"
             version ".tar.gz"))
       (sha256
        (base32 "0q5f6iywzi8hzvpjnzd5rymkkqaz5pa77x7a5sa1qlkg7p9s9h1f"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       ;; The build scripts are are in cpp/.
       (modify-phases %standard-phases
         (add-after 'unpack 'pre-configure
           (lambda _
             (chdir "cpp")))
           ;; The source does not have a ./configure script.
           (delete 'configure)
           (replace 'install
           ;; The Makefile lacks an ‘install’ target.
           (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                      (lib (string-append out "/lib"))
                      (include (string-append out "/include")))
                    (mkdir-p lib)
                    (mkdir-p include)
                    (install-file "qrcodegen.hpp" include)
                    (install-file "qrcodegen.cpp" include)
                    (install-file "qrcodegen.o" lib)
                    (install-file "libqrcodegencpp.a" lib)
             #t))))))
    (home-page "https://github.com/nayuki/QR-Code-generator")
    (synopsis "High-quality QR Code generator library - C++ version")
    (description
     "This project aims to be the best, clearest QR Code generator library
in multiple languages.")
    (license license:expat)))

(define-public cpp-webrtc
  (package
    (name "cpp-webrtc")
    (version "0.6.5")
    (source
    (origin
      (method url-fetch)
      (uri (string-append "https://source.pantherx.org/webrtc-cpp_v" version ".tgz"))
      (sha256
       (base32 "03917g3qs238pikdz5m9m125993hiczcmbj007g475f170casim7"))))
    (build-system qt-build-system)
    (arguments
     `(#:tests? #f))
    (inputs (list qtbase-5
                  qtwayland-5
                  qtdeclarative-5
                  qtmultimedia-5
                  qtquickcontrols-5
                  qtquickcontrols2-5
                  qtwebsockets-5
                  qtsvg-5
                  gstreamer
                  gst-plugins-base
                  gst-plugins-good
                  gst-plugins-bad
                  gst-plugins-good-qmlgl
                  libnice
                  cli11
                  xz))
    (native-inputs (list pkg-config))
    (home-page "https://f-a.nz/")
    (synopsis "GStreamer WebRTC C++ library")
    (description "Ease integration of GStreamer WebRTC in C++/Qt applications.")
    (license license:expat)))

(define-public cpp-webrtc-demo
  (package
    (inherit cpp-webrtc)
    (name "cpp-webrtc-demo")
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'configure-for-demo
           (lambda _
             ;; Create a custom CMakeLists.txt that only builds the demo
             (call-with-output-file "CMakeLists.txt"
               (lambda (port)
                 (display "cmake_minimum_required(VERSION 3.14)
project(webrtc_demo)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_AUTOMOC ON)
set(CMAKE_INCLUDE_CURRENT_DIR ON)

# Find required packages (same as main library)
find_package(PkgConfig REQUIRED)
find_package(CLI11 REQUIRED)
find_package(sentry CONFIG QUIET)

set(GSTREAMER_MODULES
    gstreamer-1.0
    gstreamer-sdp-1.0
    gstreamer-webrtc-1.0
    gstreamer-gl-1.0
)

pkg_check_modules(GSTREAMER REQUIRED IMPORTED_TARGET ${GSTREAMER_MODULES})
find_package(Qt5 COMPONENTS Core Network WebSockets Gui Qml Quick REQUIRED)

# Find webrtclib
find_package(webrtclib REQUIRED)

# Build the demo
add_subdirectory(example)

# Install the demo binary
install(TARGETS webrtc-cpp-demo
    RUNTIME DESTINATION bin
)
" port)))
             #t))
         (add-after 'configure-for-demo 'fix-example-cmake
           (lambda _
             ;; Ensure the example/CMakeLists.txt uses the installed webrtclib
             (substitute* "example/CMakeLists.txt"
               (("webrtclib") "webrtclib::webrtclib"))
             #t))
         (add-after 'fix-example-cmake 'fix-include-path
           (lambda _
             ;; Fix the include path in main.cpp to use the installed header location
             (substitute* "example/main.cpp"
               (("#include <webrtc.hpp>") "#include <webrtclib/webrtc.hpp>")
               (("#include <logger.hpp>") "#include <webrtclib/logger.hpp>")
               (("#include <sentry_integration.hpp>") "#include <webrtclib/sentry_integration.hpp>"))
             #t)))))
    (inputs (list cpp-webrtc
                  qtbase-5
                  qtwayland-5
                  qtdeclarative-5
                  qtmultimedia-5
                  qtquickcontrols-5
                  qtquickcontrols2-5
                  qtwebsockets-5
                  qtsvg-5
                  libnice
                  cli11
                  xz))
    (propagated-inputs (list libnice
                             gstreamer
                             gst-plugins-base
                             gst-plugins-good
                             gst-plugins-bad
                             gst-plugins-good-qmlgl))
    (native-inputs (list pkg-config
                         sentry-native-0.9))
    (home-page "https://f-a.nz/")
    (synopsis "WebRTC C++ library demo application")
    (description "Demo application showcasing the webrtc-cpp library functionality.")
    (license license:expat)))

(define-public cpp-socketio-client
  (package
    (name "cpp-socketio-client")
    (version "3.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/socketio/socket.io-client-cpp/archive/refs/tags/"
             version ".tar.gz"))
       (sha256
        (base32 "15wmj0jvj0kb1xvlpkrrja7sblx0hcd0qby4qwld062nirmx6kgm"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f
       #:configure-flags '("-DCMAKE_BUILD_TYPE=Release")))
    (inputs (list rapidjson
                  openssl
                  websocketpp
                  asio-1.28))
    (native-inputs (list pkg-config))
    (home-page "https://github.com/socketio/socket.io-client-cpp")
    (synopsis "Socket.IO C++ client library")
    (description "A Socket.IO client library written in C++11 with support for
modern Socket.IO server versions. Provides real-time bidirectional
event-based communication.")
    (license license:expat)))

(define-public litehtml
  (package
    (name "litehtml")
    (version "0.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/litehtml/litehtml")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1kk1ga3wv7k14pg0b101vc576s4zf31dx8gqbflk4yip78mwfkv4"))
       (snippet
        #~(begin (use-modules (guix build utils))
                 ;; Remove bundled gumbo-parser
                 (delete-file-recursively "src/gumbo")))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f
       #:configure-flags
       '("-DEXTERNAL_GUMBO=ON"
         "-DLITEHTML_BUILD_TESTING=OFF")))
    (inputs (list gumbo-parser))
    (home-page "https://github.com/litehtml/litehtml")
    (synopsis "Lightweight HTML/CSS rendering engine")
    (description
     "litehtml is a lightweight HTML/CSS rendering engine.  It parses HTML/CSS
and produces a rendered layout, but does not depend on any image, draw, or font
library, making it easy to embed in applications with custom rendering
backends.")
    (license license:bsd-3)))