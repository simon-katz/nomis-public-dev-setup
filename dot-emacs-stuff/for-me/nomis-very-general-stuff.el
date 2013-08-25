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

;; Try not using defparameter, because:
;; - It's not Emacs Lisp.
;; - Lots of Emacs, I think, depends on being able to set values before
;;   the thing that defines them has been loaded.  Maybe want to stick
;;   with that philosophy.
;; (defmacro defparameter (symbol &optional initvalue docstring)
;;   `(progn
;;      (defvar ,symbol nil ,docstring)
;;      (setq   ,symbol ,initvalue)))

;;;; ___________________________________________________________________________

(provide 'nomis-very-general-stuff)
