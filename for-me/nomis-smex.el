;; Init stuff -- smex.

;;;; ___________________________________________________________________________

(setq smex-save-file (concat user-emacs-directory ".smex-items"))
(smex-initialize) ; Can be omitted. This might cause a (minimal) delay
                  ; when Smex is auto-initialized on its first run.

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;;;; ___________________________________________________________________________

(provide 'nomis-smex)
