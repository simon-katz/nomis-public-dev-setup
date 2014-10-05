;;;; Init stuff -- auto-complete

;; ;;;; TODO: Check this; maybe move nrepl/cider stuff.

;; ;;;; ___________________________________________________________________________
;; ;;;; Basics

;; (require 'auto-complete-config)
;; (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
;; (ac-config-default)

;; ;; See
;; ;; https://github.com/purcell/ac-nrepl
;; ;; and
;; ;; http://stackoverflow.com/questions/7022898/emacs-autocompletion-in-emacs-lisp-mode
;; ;; and
;; ;; https://github.com/purcell/emacs.d/blob/master/init-auto-complete.el

;; (setq tab-always-indent 'complete) ; was t
;; (add-to-list 'completion-styles 'initials t) ; was (basic partial-completion emacs22)

;; (setq ac-auto-start nil)
;; (setq ac-expand-on-auto-complete nil)


;; ;;;; ___________________________________________________________________________
;; ;;;; Stuff obtained from ac-nrepl docs -- general

;; (defun set-auto-complete-as-completion-at-point-function ()
;;   (setq completion-at-point-functions '(auto-complete)))

;; (add-hook 'auto-complete-mode-hook
;;           'set-auto-complete-as-completion-at-point-function)


;; ;;;; ___________________________________________________________________________
;; ;;;; Stuff obtained from ac-nrepl docs.

;; (require 'ac-nrepl)

;; (add-hook 'cider-repl-mode-hook 'ac-nrepl-setup)

;; (add-hook 'cider-mode-hook 'ac-nrepl-setup)

;; (eval-after-load "auto-complete"
;;   '(add-to-list 'ac-modes 'cider-repl-mode))

;; (add-hook 'auto-complete-mode-hook
;;           'set-auto-complete-as-completion-at-point-function)

;; (add-hook 'cider-repl-mode-hook
;;           'set-auto-complete-as-completion-at-point-function)

;; (add-hook 'cider-mode-hook
;;           'set-auto-complete-as-completion-at-point-function)


;; (eval-after-load "cider"
;;   '(define-key cider-mode-map (kbd "C-c C-d C-c") 'ac-nrepl-popup-doc))


;;;; ___________________________________________________________________________

;;;; There's more to sort out here.
;;;; For ideas see e.g. https://gitlab.com/bodil/emacs-d/blob/6cd2e63ccc5412fa8474f6f1c38b13663f1c3bf3/bodil-bindings.el

(global-company-mode)

;; From https://github.com/company-mode/company-mode/issues/94

(setq tab-always-indent 'complete)

(defvar completion-at-point-functions-saved nil)

(defun company-indent-for-tab-command (&optional arg)
  (interactive "P")
  (let ((completion-at-point-functions-saved completion-at-point-functions)
        (completion-at-point-functions '(company-complete-common-wrapper)))
    (indent-for-tab-command arg)))

(defun company-complete-common-wrapper ()
  (let ((completion-at-point-functions completion-at-point-functions-saved))
    (company-complete-common)))

(global-set-key [tab] 'indent-for-tab-command)

(define-key company-mode-map [remap indent-for-tab-command]
  'company-indent-for-tab-command)

;;;; ___________________________________________________________________________

(provide 'nomis-auto-complete)
