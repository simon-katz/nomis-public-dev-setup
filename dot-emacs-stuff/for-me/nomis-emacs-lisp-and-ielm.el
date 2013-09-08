;;;; Init stuff -- emacs-lisp and ielm.

;;;; ___________________________________________________________________________

(dolist (hook '(emacs-lisp-mode-hook
                ielm-mode-hook))
  (dolist (hook-fun '(rainbow-delimiters-mode
                      paredit-mode
                      turn-on-elisp-slime-nav-mode
                      turn-on-eldoc-mode))
          (add-hook hook hook-fun)))

(define-key emacs-lisp-mode-map (kbd "RET") 'newline-and-indent)

;;;; ___________________________________________________________________________

(provide 'nomis-emacs-lisp-and-ielm)
