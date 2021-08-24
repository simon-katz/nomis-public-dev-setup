;;;; Init stuff -- Windows.

;;;; ___________________________________________________________________________


(defun nomis/hack-faces ()
  (set-face-attribute 'sh-heredoc nil :foreground "dark blue") ; I find the default of "tan1" hard to read
  )

(add-hook 'sh-mode-hook 'nomis/hack-faces)

;;;; ___________________________________________________________________________

(provide 'nomis-faces)
