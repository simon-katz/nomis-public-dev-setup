;;; kibit-helper-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "kibit-helper" "kibit-helper.el" (23230 12232
;;;;;;  0 0))
;;; Generated autoloads from kibit-helper.el

(autoload 'kibit "kibit-helper" "\
Run kibit on the current Leiningen project.
Display the results in a hyperlinked *compilation* buffer.

\(fn)" t nil)

(autoload 'kibit-current-file "kibit-helper" "\
Run kibit on the current file of a Leiningen project.
Display the results in a hyperlinked *compilation* buffer.

\(fn)" t nil)

(autoload 'kibit-accept-proposed-change "kibit-helper" "\


\(fn)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; kibit-helper-autoloads.el ends here
