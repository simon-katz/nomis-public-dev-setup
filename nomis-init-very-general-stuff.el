;;;; Init stuff -- Very general stuff.

(setq visible-bell t)
(setq line-move-visual nil) ; the default of T is annoying, and it
                            ; screws up keyboard macros

;; (setq-default indent-tabs-mode nil)

(setq Buffer-menu-sort-column nil) ; 2

(add-hook 'find-file-hooks (lambda () (auto-fill-mode -1)))

(defmacro defparameter (symbol &optional initvalue docstring)
  `(progn
     (defvar ,symbol nil ,docstring)
     (setq   ,symbol ,initvalue)))

;; (global-set-key (kbd "<escape>") 'keyboard-quit)
                                        ; hmmm, so I can't do ESC C-k -- not a huge deal, but annoying -- have I lost anything else?

;;;; ___________________________________________________________________________

(provide 'nomis-init-very-general-stuff)
