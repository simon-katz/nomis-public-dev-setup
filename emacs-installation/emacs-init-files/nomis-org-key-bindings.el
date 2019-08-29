;;;; nomis-org-key-bindings  ---  -*- lexical-binding: t -*-

;;;; ___________________________________________________________________________
;;;; ____ * General

;;; The following lines are always needed. Choose your own keys.

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

(add-hook 'org-mode-hook 'turn-on-font-lock) ; not needed when global-font-lock-mode is on

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(org-defkey global-map "\C-cc" 'org-capture)

;;;; ___________________________________________________________________________
;;;; ____ * Hooks

(add-hook 'org-mode-hook 'nomis/turn-on-idle-highlight-mode)

;;;; ___________________________________________________________________________
;;;; ____ * Navigation and cycling

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;;; ____ ** Following links

(org-defkey org-mode-map (kbd "M-.") 'org-open-at-point)
(org-defkey org-mode-map (kbd "M-,") 'org-mark-ring-goto)

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;;; ____ ** "Visibility" -- the org-show-only stuff (which is badly named)

(org-defkey org-mode-map (kbd "H-C-\\") 'nomis/org-show-only/cycle/more)
(org-defkey org-mode-map (kbd "H-C-'") 'nomis/org-show-only/cycle/less)

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;;; ____ ** Expand/collapse

(org-defkey org-mode-map (kbd "H-q =") 'norg/show-all-to-current-level)

(org-defkey org-mode-map (kbd "H-M-'")  'norg/show-children-from-point/set-0)
(org-defkey org-mode-map (kbd "H-M-\\") 'norg/show-children-from-point/fully-expand)
(org-defkey org-mode-map (kbd "H-M-[")  'norg/show-children-from-root/set-0)
(org-defkey org-mode-map (kbd "H-M-]")  'norg/show-children-from-root/fully-expand)
(org-defkey org-mode-map (kbd "H-M--")  'norg/show-children-from-all-roots/set-0)
(org-defkey org-mode-map (kbd "H-M-=")  'norg/show-children-from-all-roots/fully-expand)

(org-defkey org-mode-map (kbd "H-'")  'norg/show-children-from-point/incremental/less)
(org-defkey org-mode-map (kbd "H-\\") 'norg/show-children-from-point/incremental/more)
(org-defkey org-mode-map (kbd "H-[")  'norg/show-children-from-root/incremental/less)
(org-defkey org-mode-map (kbd "H-]")  'norg/show-children-from-root/incremental/more)
(org-defkey org-mode-map (kbd "H--")  'norg/show-children-from-all-roots/incremental/less)
(org-defkey org-mode-map (kbd "H-=")  'norg/show-children-from-all-roots/incremental/more)

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;;; ____ ** Movement

(org-defkey org-mode-map (kbd "H-.") 'org-forward-heading-same-level) ; also C-c C-f
(org-defkey org-mode-map (kbd "H-,") 'org-backward-heading-same-level) ; also C-c C-b
(org-defkey org-mode-map (kbd "H-C-.") 'nomis/org-forward-heading-same-level-with-extras)
(org-defkey org-mode-map (kbd "H-C-,") 'nomis/org-backward-heading-same-level-with-extras)

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;;; ____ ** Movement + expand/collapse

(org-defkey org-mode-map (kbd "H-M-.") 'nomis/org/step-forward)
(org-defkey org-mode-map (kbd "H-M-,") 'nomis/org/step-backward)
(org-defkey org-mode-map (kbd "H-C-M-.") 'nomis/org/step-forward/jumping-parent-allowed)
(org-defkey org-mode-map (kbd "H-C-M-,") 'nomis/org/step-backward/jumping-parent-allowed)

;;;; ___________________________________________________________________________
;;;; ____ * Agenda

(progn ; TODO This is not only agenda stuff (contrary to the comment above), and this does not need to be done in a hook (or else everything should go in a hook)
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

(provide 'nomis-org-key-bindings)
