# Known Issues

Upstream problems this channel works around. Each entry says what to remove once upstream is fixed.

## GDM login screen starts as a plain GNOME session

- **Since:** Guix commit `c5157bf58bc` (2026-11-18), `gnome-session-shepherd` 1.2
- **Affects:** any system that uses GDM, including KDE Plasma and other desktops started from it
- **Upstream report:** none yet

After `guix system reconfigure` and a reboot, GDM shows a GNOME Shell overview with nobody logged in instead of the login prompt. Logging in on a TTY still works, and so does `guix system roll-back`.

GDM starts its greeter with a plain `gnome-session` and marks it through `GNOME_SHELL_SESSION_MODE=gdm`. `gnome-session-shepherd` 1.2 ignores that variable, resolves the session name to `gnome`, and runs `gnome-shell --mode user`.

**Workaround in this channel:** `%os-desktop-services` (and the deprecated `%panther-desktop-services`) run GDM against a patched `gnome-session-shepherd` that honors the variable:

- `px/packages/gnome.scm`
- `px/packages/patches/gnome-session-shepherd-honor-session-mode.patch`
- the `gdm-service-type` clause in `px/system/os.scm`

Configurations built on upstream `%desktop-services` don't get the fix. Use `gdm/greeter-mode` there:

```scheme
(use-modules (gnu services xorg)
             (px packages gnome))

(modify-services %desktop-services
  (gdm-service-type config =>
    (gdm-configuration
      (inherit config)
      (gdm gdm/greeter-mode))))
```

**Remove when:** upstream GDM shows the login prompt again without the patch. Delete the three pieces above and this entry.
