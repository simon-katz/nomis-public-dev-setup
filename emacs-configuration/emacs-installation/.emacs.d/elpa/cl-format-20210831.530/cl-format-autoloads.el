;;; cl-format-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "cl-format" "cl-format.el" (0 0 0 0))
;;; Generated autoloads from cl-format.el

(autoload 'cl-format "cl-format" "\
Format FMT using ARGS and print it to STREAM.

The full documentation not available until this function is
loaded.

\(fn STREAM FMT &rest ARGS)" nil nil)

(autoload 'cl-formatter "cl-format" "\
Compile FMT into a function.

This macro parses and compiles FMT into a function, which may be
passed as format argument to `cl-format'.

\(fn FMT)" nil t)

(autoload 'cl-error "cl-format" "\
Like `error', but use CL format strings.

\(fn FMT &rest ARGS)" nil nil)

(autoload 'cl-message "cl-format" "\
Like `error', but use CL format strings.

\(fn FMT &rest ARGS)" nil nil)

(autoload 'cl-format-font-lock-mode "cl-format" "\
Adds font-lock support for cl format strings.

This is a minor mode.  If called interactively, toggle the
`Cl-Format-Font-Lock mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `cl-format-font-lock-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "cl-format" '("cl-format-"))

;;;***

;;;### (autoloads nil "cl-format-builtins" "cl-format-builtins.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from cl-format-builtins.el

(register-definition-prefixes "cl-format-builtins" '("?$" "?%" "?&" "?*" "?/" "?<" "??" "?\\[" "?^" "?_" "?a" "?b" "?c" "?d" "?e" "?f" "?g" "?i" "?o" "?p" "?r" "?s" "?t" "?w" "?x" "?{" "?~" "cl-format-" "single-key-description-forthcoming"))

;;;***

;;;### (autoloads nil "cl-format-def" "cl-format-def.el" (0 0 0 0))
;;; Generated autoloads from cl-format-def.el

(register-definition-prefixes "cl-format-def" '("cl-" "define-cl-format-directive"))

;;;***

;;;### (autoloads nil "clisp-format" "clisp-format.el" (0 0 0 0))
;;; Generated autoloads from clisp-format.el

(register-definition-prefixes "clisp-format" '("clisp-"))

;;;***

;;;### (autoloads nil nil ("cl-format-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; cl-format-autoloads.el ends here
