;;; Package Repository for GNU Guix
;;; Copyright © 2026 Franz Geffke <mail@gofranz.com>

(define-module (px packages gnome)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (gnu packages gnome)
  #:export (gdm/greeter-mode))

;; See KNOWN_ISSUES.md; drop both once the upstream GDM greeter is fixed.
(define gnome-session-shepherd/greeter-mode
  (hidden-package
   (package
     (inherit gnome-session-shepherd)
     (source
      (origin
        (inherit (package-source gnome-session-shepherd))
        (patches
         (list (local-file
                "patches/gnome-session-shepherd-honor-session-mode.patch")))))
     (arguments
      (substitute-keyword-arguments (package-arguments gnome-session-shepherd)
        ((#:phases phases)
         #~(modify-phases #$phases
             (add-after 'wrap-scripts 'wrap-schemas
               (lambda* (#:key inputs #:allow-other-keys)
                 (substitute* (map (lambda (script)
                                     (string-append #$output "/bin/" script))
                                   '("gnome-session" "gnome-session-leader"
                                     "gherd"))
                   (("^exec -a" all)
                    (string-append
                     "export XDG_DATA_DIRS=\""
                     (assoc-ref inputs "gsettings-desktop-schemas")
                     "/share${XDG_DATA_DIRS:+:}$XDG_DATA_DIRS\"\n"
                     all))))))))))))

(define gdm/greeter-mode
  (hidden-package
   ((package-input-rewriting
     `((,gnome-session-shepherd . ,gnome-session-shepherd/greeter-mode)))
    gdm)))
