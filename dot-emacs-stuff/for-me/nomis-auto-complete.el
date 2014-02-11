;;;; Init stuff -- auto-complete

;;;; TODO: Check this; maybe move nrepl/cider stuff.

;;;; ___________________________________________________________________________
;;;; Basics

(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)

;; See
;; https://github.com/purcell/ac-nrepl
;; and
;; http://stackoverflow.com/questions/7022898/emacs-autocompletion-in-emacs-lisp-mode
;; and
;; https://github.com/purcell/emacs.d/blob/master/init-auto-complete.el

(setq tab-always-indent 'complete) ; was t
(add-to-list 'completion-styles 'initials t) ; was (basic partial-completion emacs22)

(setq ac-auto-start nil)
(setq ac-expand-on-auto-complete nil)


;;;; ___________________________________________________________________________
;;;; Stuff obtained from ac-nrepl docs -- general

(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))

(add-hook 'auto-complete-mode-hook
          'set-auto-complete-as-completion-at-point-function)


;;;; ___________________________________________________________________________
;;;; Stuff obtained from ac-nrepl docs.

(require 'ac-nrepl)

(add-hook 'cider-repl-mode-hook 'ac-nrepl-setup)

(add-hook 'cider-mode-hook 'ac-nrepl-setup)

(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'cider-repl-mode))

(add-hook 'auto-complete-mode-hook
          'set-auto-complete-as-completion-at-point-function)

(add-hook 'cider-repl-mode-hook
          'set-auto-complete-as-completion-at-point-function)

(add-hook 'cider-mode-hook
          'set-auto-complete-as-completion-at-point-function)


(eval-after-load "cider"
  '(define-key cider-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc))


;;;; ___________________________________________________________________________

(provide 'nomis-auto-complete)
