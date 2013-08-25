;;;; Init stuff -- Very general stuff.

(setq-default indent-tabs-mode nil) ; use spaces, not tabs
(setq sentence-end-double-space nil)
(setq visible-bell t)
(setq whitespace-style '(face trailing lines-tail tabs))
(show-paren-mode 1)
(setq line-move-visual nil) ; the default of T is annoying, and it
                            ; screws up keyboard macros
(setq backup-directory-alist
      `(("." . ,(expand-file-name "~/.emacs-backups"))))

(progn
  (defun nomis-turn-on-idle-highlight-mode ()
    (idle-highlight-mode t))
  (add-hook 'prog-mode-hook 'nomis-turn-on-idle-highlight-mode))

(when (display-graphic-p)
  ;; (mouse-wheel-mode t)
  (blink-cursor-mode -1))

(setq Buffer-menu-sort-column nil) ; 2

(progn
  (defun nomis-turn-off-auto-fill-mode ()
    (auto-fill-mode -1))
  (add-hook 'find-file-hooks 'nomis-turn-off-auto-fill-mode))

(defalias 'yes-or-no-p 'y-or-n-p)

(defmacro defparameter (symbol &optional initvalue docstring)
  `(progn
     (defvar ,symbol nil ,docstring)
     (setq   ,symbol ,initvalue)))

;; (global-set-key (kbd "<escape>") 'keyboard-quit)
                                        ; hmmm, so I can't do ESC C-k -- not a huge deal, but annoying -- have I lost anything else?

;;;; ___________________________________________________________________________

(provide 'nomis-very-general-stuff)
