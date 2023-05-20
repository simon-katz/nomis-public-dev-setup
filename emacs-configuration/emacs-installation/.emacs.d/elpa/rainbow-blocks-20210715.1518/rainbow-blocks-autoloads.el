;;; rainbow-blocks-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "rainbow-blocks" "rainbow-blocks.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from rainbow-blocks.el

(autoload 'rainbow-blocks-mode "rainbow-blocks" "\
Highlight nested parentheses, brackets, and braces according to their depth.

This is a minor mode.  If called interactively, toggle the
`Rainbow-Blocks mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `rainbow-blocks-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(autoload 'rainbow-blocks-mode-enable "rainbow-blocks" nil nil nil)

(autoload 'rainbow-blocks-mode-disable "rainbow-blocks" nil nil nil)

(put 'global-rainbow-blocks-mode 'globalized-minor-mode t)

(defvar global-rainbow-blocks-mode nil "\
Non-nil if Global Rainbow-Blocks mode is enabled.
See the `global-rainbow-blocks-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-rainbow-blocks-mode'.")

(custom-autoload 'global-rainbow-blocks-mode "rainbow-blocks" nil)

(autoload 'global-rainbow-blocks-mode "rainbow-blocks" "\
Toggle Rainbow-Blocks mode in all buffers.
With prefix ARG, enable Global Rainbow-Blocks mode if ARG is positive;
otherwise, disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Rainbow-Blocks mode is enabled in all buffers where
`rainbow-blocks-mode-enable' would do it.

See `rainbow-blocks-mode' for more information on Rainbow-Blocks
mode.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "rainbow-blocks" '("rainbow-blocks-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; rainbow-blocks-autoloads.el ends here
