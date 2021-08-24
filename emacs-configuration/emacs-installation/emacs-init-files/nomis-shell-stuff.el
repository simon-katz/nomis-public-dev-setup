;;;; Init stuff -- Shell stuff.

;;;; ___________________________________________________________________________
;;;; Style

(add-hook 'sh-mode-hook 'flycheck-mode)
(add-hook 'sh-mode-hook 'highlight-indentation-mode)

;; (setq sh-basic-offset 4) ; Leave at the default

;;;; ___________________________________________________________________________

(defun shell-region (start end)
  ;; Copied from http://stackoverflow.com/questions/6286579/emacs-shell-mode-how-to-send-region-to-shell.
  "Execute contents of region in an inferior shell."
  (interactive "r")
  (shell-command (buffer-substring-no-properties start end)))

;;;; ___________________________________________________________________________

(provide 'nomis-shell-stuff)
