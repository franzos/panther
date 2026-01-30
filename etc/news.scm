;; PantherX channel news, for use by 'guix pull'.
;;
;; Copyright 2026 Franz Geffke <mail@gofranz.com>
;;
;; Copying and distribution of this file, with or without modification, are
;; permitted in any medium without royalty provided the copyright notice and
;; this notice are preserved.

(channel-news
 (version 0)

 (entry (commit "4f51b01")
        (title
         (en "Darkman home service removed - use Guix upstream"))
        (body
         (en "The @code{home-darkman-service-type} has been removed from this
channel.  Guix now provides an equivalent service upstream in
@code{(gnu home services desktop)}.

To migrate, update your home configuration:

@enumerate
@item
Replace @code{(px home services darkman)} with @code{(gnu home services desktop)}
in your imports.

@item
Change @code{(use-geoclue? #f)} to @code{(use-geoclue #f)} in your
@code{home-darkman-configuration}.
@end enumerate

The upstream service is functionally equivalent."))))
