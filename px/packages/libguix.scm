;;; Package Repository for GNU Guix
;;; Copyright © 2026 Franz Geffke <mail@gofranz.com>

(define-module (px packages libguix)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix build-system trivial)
  #:use-module ((guix licenses) #:prefix license:))

;; Polkit action files for libguix. Kept inline so the package is
;; self-contained; the canonical source lives in
;; <https://github.com/franzos/guix-rs> under polkit/.
;;
;; Two distinct actions are shipped: one for `guix system reconfigure'
;; and one for `guix pull'.  Both are argv-constrained so they only
;; match the exact subcommand they cover -- without that constraint a
;; single action would gate every invocation of the trusted guix binary,
;; which is too coarse.

(define %libguix-system-reconfigure-policy
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<!DOCTYPE policyconfig PUBLIC
 \"-//freedesktop//DTD PolicyKit Policy Configuration 1.0//EN\"
 \"http://www.freedesktop.org/standards/PolicyKit/1.0/policyconfig.dtd\">
<policyconfig>

  <vendor>libguix</vendor>
  <vendor_url>https://github.com/franzos/guix-rs</vendor_url>

  <action id=\"org.libguix.system-reconfigure\">
    <description>Reconfigure the Guix system</description>
    <message>Authentication is required to reconfigure the Guix system.</message>
    <defaults>
      <allow_any>auth_admin</allow_any>
      <allow_inactive>auth_admin</allow_inactive>
      <allow_active>auth_admin_keep</allow_active>
    </defaults>
    <annotate key=\"org.freedesktop.policykit.exec.path\">/run/current-system/profile/bin/guix</annotate>
    <annotate key=\"org.freedesktop.policykit.exec.argv1\">system</annotate>
    <annotate key=\"org.freedesktop.policykit.exec.argv2\">reconfigure</annotate>
    <annotate key=\"org.freedesktop.policykit.exec.allow_gui\">true</annotate>
  </action>

</policyconfig>
")

(define %libguix-system-pull-policy
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<!DOCTYPE policyconfig PUBLIC
 \"-//freedesktop//DTD PolicyKit Policy Configuration 1.0//EN\"
 \"http://www.freedesktop.org/standards/PolicyKit/1.0/policyconfig.dtd\">
<policyconfig>

  <vendor>libguix</vendor>
  <vendor_url>https://github.com/franzos/guix-rs</vendor_url>

  <action id=\"org.libguix.system-pull\">
    <description>Update the Guix catalog</description>
    <message>Authentication is required to fetch the latest Guix catalog.</message>
    <defaults>
      <allow_any>auth_admin</allow_any>
      <allow_inactive>auth_admin</allow_inactive>
      <allow_active>auth_admin_keep</allow_active>
    </defaults>
    <annotate key=\"org.freedesktop.policykit.exec.path\">/run/current-system/profile/bin/guix</annotate>
    <annotate key=\"org.freedesktop.policykit.exec.argv1\">pull</annotate>
    <annotate key=\"org.freedesktop.policykit.exec.allow_gui\">true</annotate>
  </action>

</policyconfig>
")

(define-public libguix-polkit
  (package
    (name "libguix-polkit")
    (version "0.0.3")
    (source #f)
    (build-system trivial-build-system)
    (inputs
     `(("reconfigure-policy"
        ,(plain-file "org.libguix.system-reconfigure.policy"
                     %libguix-system-reconfigure-policy))
       ("pull-policy"
        ,(plain-file "org.libguix.system-pull.policy"
                     %libguix-system-pull-policy))))
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((reconf (assoc-ref %build-inputs "reconfigure-policy"))
                (pull   (assoc-ref %build-inputs "pull-policy"))
                (out    (assoc-ref %outputs "out"))
                (dst    (string-append out "/share/polkit-1/actions")))
           (mkdir-p dst)
           (copy-file reconf
                      (string-append dst
                                     "/org.libguix.system-reconfigure.policy"))
           (copy-file pull
                      (string-append dst
                                     "/org.libguix.system-pull.policy"))))))
    (synopsis "Polkit actions for libguix system reconfigure and pull")
    (description
     "Installs two polkit actions that allow desktop tools built on
@code{libguix} to invoke @command{guix system reconfigure} and
@command{guix pull} via @command{pkexec}.  Both actions are keyed on
the system Guix at @file{/run/current-system/profile/bin/guix} and
argv-constrained to their respective subcommand.  They use
@code{auth_admin_keep} so successive invocations within a few minutes
do not re-prompt.

After deploying this package via @code{polkit-service-type}, run
@code{sudo guix system reconfigure} once to activate the new action
IDs.  Until then, @command{pkexec guix @dots{}} calls from libguix fall
back to the generic @code{org.freedesktop.policykit.exec} action --
still functional, just without @code{auth_admin_keep} or the
custom prompts.")
    (home-page "https://github.com/franzos/guix-rs")
    (license license:gpl3+)))
