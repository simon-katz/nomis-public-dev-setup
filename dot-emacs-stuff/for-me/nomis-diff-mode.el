;;;; Init stuff -- diff-mode.

;;;; ___________________________________________________________________________

(defun nomis-diff-colors ()
  "update the colors for diff faces"
  (set-face-attribute
   'diff-added nil :foreground "green")
  (set-face-attribute
   'diff-removed nil :foreground "red")
  (set-face-attribute
   'diff-changed nil :foreground "purple"))

(eval-after-load "diff-mode" '(nomis-diff-colors))

;;;; ___________________________________________________________________________

(provide 'nomis-diff-mode)
