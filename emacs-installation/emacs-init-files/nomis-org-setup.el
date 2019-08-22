;;;; nomis-org-setup ---  -*- lexical-binding: t -*-

;;;; ___________________________________________________________________________
;;;; ____ * Require things

(progn
  (setq org-replace-disputed-keys t) ; must be done before requiring org
  (require 'nomis-org))

(require 'nomis-org-personal)

;;;; ___________________________________________________________________________
;;;; ____ * General

;;; The following lines are always needed. Choose your own keys.

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

(add-hook 'org-mode-hook 'turn-on-font-lock) ; not needed when global-font-lock-mode is on

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(define-key global-map "\C-cc" 'org-capture)

;;;; ___________________________________________________________________________
;;;; ____ * Hooks

(add-hook 'org-mode-hook 'nomis/turn-on-idle-highlight-mode)

;;;; ___________________________________________________________________________
;;;; ____ * Reporting

(define-key org-mode-map (kbd "C-c =") 'nomis/org/report-org-info)

;;;; ___________________________________________________________________________
;;;; ____ * Navigation and cycling

(define-key org-mode-map (kbd "M-.") 'org-open-at-point)
(define-key org-mode-map (kbd "M-,") 'org-mark-ring-goto)

(define-key org-mode-map (kbd "H-M-.") 'nomis/org-show-only/cycle/more)
(define-key org-mode-map (kbd "H-M-,") 'nomis/org-show-only/cycle/less)

(define-key org-mode-map [remap org-forward-heading-same-level]
  'nomis/org-forward-heading-same-level-with-extras)
(define-key org-mode-map [remap org-backward-heading-same-level]
  'nomis/org-backward-heading-same-level-with-extras)

(define-key org-mode-map (kbd "H-]") 'nomis/org/step-forward)
(define-key org-mode-map (kbd "H-[") 'nomis/org/step-backward)
(define-key org-mode-map (kbd "H-M-]") 'nomis/org/step-forward/jumping-parent-allowed)
(define-key org-mode-map (kbd "H-M-[") 'nomis/org/step-backward/jumping-parent-allowed)

;;;; ___________________________________________________________________________
;;;; ____ * Agenda

(progn
  (defun nomis/setup-org-keys ()
    ;; I don't like RETURN in org agenda giving ORG-AGENDA-SWITCH-TO.
    ;; I prefer this:
    (org-defkey org-agenda-mode-map "\C-m" 'org-agenda-show-and-scroll-up)
    ;; Stuff that got changed when I upgraded to Emacs 26.1 -- this is mad!
    (org-defkey org-mode-map (kbd "M-S-<down>") 'org-move-subtree-down)
    (org-defkey org-mode-map (kbd "M-S-<up>")   'org-move-subtree-up))
  (add-hook 'org-mode-hook 'nomis/setup-org-keys))

;;;; ___________________________________________________________________________
;;;; ____ * End

(provide 'nomis-org-setup)
