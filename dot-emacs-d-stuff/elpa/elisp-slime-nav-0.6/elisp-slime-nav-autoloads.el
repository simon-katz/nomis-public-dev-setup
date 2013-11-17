;;; elisp-slime-nav-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (elisp-slime-nav-describe-elisp-thing-at-point
;;;;;;  elisp-slime-nav-find-elisp-thing-at-point turn-on-elisp-slime-nav-mode
;;;;;;  elisp-slime-nav-mode) "elisp-slime-nav" "../../../../../../../.emacs.d/elpa/elisp-slime-nav-0.6/elisp-slime-nav.el"
;;;;;;  "799be6a36b2ef938205ade7f95e250ce")
;;; Generated autoloads from ../../../../../../../.emacs.d/elpa/elisp-slime-nav-0.6/elisp-slime-nav.el

(autoload 'elisp-slime-nav-mode "elisp-slime-nav" "\
Enable Slime-style navigation of elisp symbols using M-. and M-,

\(fn &optional ARG)" t nil)

(autoload 'turn-on-elisp-slime-nav-mode "elisp-slime-nav" "\
Explicitly enable `elisp-slime-nav-mode'.

\(fn)" nil nil)

(autoload 'elisp-slime-nav-find-elisp-thing-at-point "elisp-slime-nav" "\
Jump to the elisp thing at point, be it a function, variable, library or face.
With a prefix arg, prompt for the symbol to jump to.
Argument SYM-NAME thing to find.

\(fn SYM-NAME)" t nil)

(autoload 'elisp-slime-nav-describe-elisp-thing-at-point "elisp-slime-nav" "\
Display the full documentation of the elisp thing at point.
The named subject may be a function, variable, library or face.
With a prefix arg, prompt for the symbol to jump to.
Argument SYM-NAME thing to find.

\(fn SYM-NAME)" t nil)

;;;***

;;;### (autoloads nil nil ("../../../../../../../.emacs.d/elpa/elisp-slime-nav-0.6/elisp-slime-nav-pkg.el"
;;;;;;  "../../../../../../../.emacs.d/elpa/elisp-slime-nav-0.6/elisp-slime-nav.el")
;;;;;;  (21127 34683 591069))

;;;***

(provide 'elisp-slime-nav-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; elisp-slime-nav-autoloads.el ends here
