;;;; Init stuff -- Very general stuff.

(defun nomis-no-op ()
  (interactive))

(define-key global-map (kbd "M-<return>") 'nomis-no-op)

;; (defun nomis-save-buffers-kill-terminal ()
;;   (interactive)
;;   (when (yes-or-no-p "Really do `save-buffers-kill-terminal`?")
;;     (save-buffers-kill-terminal)))

;; (define-key global-map (kbd "C-x C-c") 'nomis-save-buffers-kill-terminal)

(setq confirm-kill-emacs 'y-or-n-p)

(setq-default indent-tabs-mode nil) ; use spaces, not tabs
(setq sentence-end-double-space nil)
(setq visible-bell t)
(show-paren-mode 1)
(setq line-move-visual nil) ; the default of T is annoying, and it
                            ; screws up keyboard macros
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))

(setq nomis-backup-directory (expand-file-name "~/.emacs-backups/"))
(make-directory nomis-backup-directory t)
(setq backup-directory-alist
      `(("." . ,nomis-backup-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,nomis-backup-directory t)))

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

(put 'erase-buffer 'disabled nil)

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
