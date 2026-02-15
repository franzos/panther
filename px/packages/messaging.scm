;;; Package Repository for GNU Guix
;;; Copyright © 2026 Franz Geffke <franz@pantherx.org>
;;;
;;; This is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.

(define-module (px packages messaging)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages kerberos)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module ((guix licenses) :prefix license:)
  #:use-module (nonguix build-system binary)
  #:use-module ((nonguix licenses) :prefix license:)
  #:use-module (nongnu packages messaging))

(define-public px-zoom
  (package
    (inherit zoom)
    (name "zoom")
    (arguments
     (substitute-keyword-arguments (package-arguments zoom)
       ((#:phases phases)
        #~(modify-phases #$phases
            (replace 'wrap-where-patchelf-does-not-work
              (lambda _
                (let ((lib-zoom (string-append #$output "/lib/zoom")))
                  (wrap-program (string-append #$output "/lib/zoom/zopen")
                    `("LD_LIBRARY_PATH" prefix
                      ,(list lib-zoom
                             #$@(map (lambda (pkg)
                                       (file-append (this-package-input pkg) "/lib"))
                                     '("fontconfig-minimal"
                                       "freetype"
                                       "gcc"
                                       "glib"
                                       "libxcomposite"
                                       "libxdamage"
                                       "libxkbcommon"
                                       "libxkbfile"
                                       "libxrandr"
                                       "libxrender"
                                       "zlib")))))
                  (wrap-program (string-append #$output "/lib/zoom/zoom")
                    '("QML2_IMPORT_PATH" = ())
                    '("QT_PLUGIN_PATH" = ())
                    '("QT_SCREEN_SCALE_FACTORS" = ())
                    `("FONTCONFIG_PATH" ":" prefix
                      (,(string-join
                         (list
                          (string-append #$(this-package-input "fontconfig-minimal") "/etc/fonts")
                          #$output)
                         ":")))
                    `("LD_LIBRARY_PATH" prefix
                      ,(list lib-zoom
                             (string-append #$(this-package-input "nss") "/lib/nss")
                             #$@(map (lambda (pkg)
                                       (file-append (this-package-input pkg) "/lib"))
                                     '("alsa-lib"
                                       "at-spi2-core"
                                       "cairo"
                                       "cups"
                                       "dbus"
                                       "eudev"
                                       "expat"
                                       "gcc"
                                       "glib"
                                       "mesa"
                                       "mit-krb5"
                                       "nspr"
                                       "libxcb"
                                       "libxcomposite"
                                       "libxdamage"
                                       "libxext"
                                       "libxkbcommon"
                                       "libxkbfile"
                                       "libxrandr"
                                       "libxshmfence"
                                       "pango"
                                       "pulseaudio"
                                       "xcb-util"
                                       "xcb-util-image"
                                       "xcb-util-keysyms"
                                       "xcb-util-wm"
                                       "xcb-util-renderutil"
                                       "zlib")))))
                  (wrap-program (string-append #$output "/lib/zoom/aomhost")
                    `("FONTCONFIG_PATH" ":" prefix
                      (,(string-join
                         (list
                          (string-append #$(this-package-input "fontconfig-minimal") "/etc/fonts")
                          #$output)
                         ":")))
                    `("LD_LIBRARY_PATH" prefix
                      ,(list lib-zoom
                             (string-append #$(this-package-input "nss") "/lib/nss")
                             #$@(map (lambda (pkg)
                                       (file-append (this-package-input pkg) "/lib"))
                                     '("alsa-lib"
                                       "at-spi2-core"
                                       "cairo"
                                       "cups"
                                       "dbus"
                                       "eudev"
                                       "expat"
                                       "gcc"
                                       "glib"
                                       "mesa"
                                       "mit-krb5"
                                       "nspr"
                                       "libxcb"
                                       "libxcomposite"
                                       "libxdamage"
                                       "libxext"
                                       "libxkbcommon"
                                       "libxkbfile"
                                       "libxrandr"
                                       "libxshmfence"
                                       "pango"
                                       "pulseaudio"
                                       "xcb-util"
                                       "xcb-util-image"
                                       "xcb-util-keysyms"
                                       "xcb-util-wm"
                                       "xcb-util-renderutil"
                                       "zlib"))))))))))))))
