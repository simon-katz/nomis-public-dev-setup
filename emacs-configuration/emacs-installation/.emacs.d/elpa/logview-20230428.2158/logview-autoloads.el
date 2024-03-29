;;; logview-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "logview" "logview.el" (0 0 0 0))
;;; Generated autoloads from logview.el

(add-to-list 'auto-mode-alist '("\\.log\\(?:\\.[0-9]+\\)?\\'" . logview-mode) t)

(autoload 'logview-mode "logview" "\
Major mode for viewing and filtering various log files.

\(fn)" t nil)

(register-definition-prefixes "logview" '("logview-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; logview-autoloads.el ends here
