;;; yafolding-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "yafolding" "../../../../../../../../../.emacs.d/elpa/yafolding-0.4.0/yafolding.el"
;;;;;;  "c143f47148c795b6d54d819aa41ca940")
;;; Generated autoloads from ../../../../../../../../../.emacs.d/elpa/yafolding-0.4.0/yafolding.el

(defvar yafolding-mode-map (let ((map (make-sparse-keymap))) (define-key map (kbd "<C-S-return>") #'yafolding-hide-parent-element) (define-key map (kbd "<C-M-return>") #'yafolding-toggle-all) (define-key map (kbd "<C-return>") #'yafolding-toggle-element) map))

(autoload 'yafolding-mode "yafolding" "\
Toggle yafolding mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("../../../../../../../../../.emacs.d/elpa/yafolding-0.4.0/yafolding-autoloads.el"
;;;;;;  "../../../../../../../../../.emacs.d/elpa/yafolding-0.4.0/yafolding.el")
;;;;;;  (23420 33987 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; yafolding-autoloads.el ends here
