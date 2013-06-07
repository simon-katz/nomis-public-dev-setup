;;;; Init stuff -- auto-complete

;;; Basics

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


;;; Stuff obtained from nrepl docs -- general

(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))

(add-hook 'auto-complete-mode-hook
          'set-auto-complete-as-completion-at-point-function)


;;; Stuff obtained from nrepl docs -- for nrepl

(require 'ac-nrepl)

(add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
(add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'nrepl-mode))

(add-hook 'nrepl-mode-hook
          'set-auto-complete-as-completion-at-point-function)
(add-hook 'nrepl-interaction-mode-hook
          'set-auto-complete-as-completion-at-point-function)

(define-key nrepl-interaction-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc)

;;;; ___________________________________________________________________________

(provide 'nomis-init-auto-complete)
