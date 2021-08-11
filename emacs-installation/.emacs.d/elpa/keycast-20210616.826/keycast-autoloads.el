;;; keycast-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "keycast" "keycast.el" (0 0 0 0))
;;; Generated autoloads from keycast.el

(defvar keycast-mode nil "\
Non-nil if Keycast mode is enabled.
See the `keycast-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `keycast-mode'.")

(custom-autoload 'keycast-mode "keycast" nil)

(autoload 'keycast-mode "keycast" "\
Show current command and its key binding in the mode line.

\(fn &optional ARG)" t nil)

(defvar keycast-log-mode nil "\
Non-nil if Keycast-Log mode is enabled.
See the `keycast-log-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `keycast-log-mode'.")

(custom-autoload 'keycast-log-mode "keycast" nil)

(autoload 'keycast-log-mode "keycast" "\
Log invoked commands and their key bindings in a buffer.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "keycast" '("keycast-" "mode-line-keycast")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; keycast-autoloads.el ends here
