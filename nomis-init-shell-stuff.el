;;;; Init stuff -- Shell stuff.

(defun shell-region (start end)
  ;; Copied from http://stackoverflow.com/questions/6286579/emacs-shell-mode-how-to-send-region-to-shell.
  "execute region in an inferior shell"
  (interactive "r")
  (shell-command (buffer-substring-no-properties start end)))

;;;; ___________________________________________________________________________

(provide 'nomis-init-shell-stuff)
