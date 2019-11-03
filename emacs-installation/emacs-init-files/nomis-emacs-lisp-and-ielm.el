;;;; Init stuff -- emacs-lisp and ielm.

;;;; ___________________________________________________________________________

(defvar nomis/lisp-and-ielm-mode-hook-functions
  `(rainbow-delimiters-mode
    paredit-mode
    paxedit-mode ; some commands (at least) don't work in ielm mode
    ,(lambda () (set (make-local-variable 'comment-column) 0))
    turn-on-elisp-slime-nav-mode
    turn-on-eldoc-mode
    ;; aggressive-indent-mode
    ))

(dolist (hook '(emacs-lisp-mode-hook
                ielm-mode-hook))
  (dolist (hook-fun nomis/lisp-and-ielm-mode-hook-functions)
    (add-hook hook hook-fun)))

(define-key emacs-lisp-mode-map (kbd "RET") 'newline-and-indent)

;;; ___________________________________________________________________________

(provide 'nomis-emacs-lisp-and-ielm)
