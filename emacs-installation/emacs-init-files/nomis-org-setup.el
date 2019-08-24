;;;; nomis-org-setup ---  -*- lexical-binding: t -*-

;;;; ___________________________________________________________________________
;;;; ____ * Require things

(require 'cl)

(progn ; require nomis-org
  (cl-flet
      ((do-stuff-that-must-be-done-before-requiring-org
        ()
        (setq org-replace-disputed-keys t)
        (setq org-disputed-keys
              '(([(shift up)]               . [(meta p)])
                ([(shift down)]             . [(meta n)])
                ;; M-- was being taken away from `negative-argument`, so
                ;; change the following from the default value of
                ;; `org-disputed-keys`.
                ;; I'd really like to have no keys on the RHS, but I think
                ;; that's not possible. So add alt, hyper and control to get
                ;; unlikely-to-be-wanted key chords.
                ([(shift left)]             . [(alt hyper control meta -)])
                ([(shift right)]            . [(alt hyper control meta +)])
                ([(control shift right)]    . [(alt hyper control meta shift +)])
                ([(control shift left)]     . [(alt hyper control meta shift -)])))))
    (do-stuff-that-must-be-done-before-requiring-org)
    (require 'nomis-org)))

(require 'nomis-org-personal)

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
;;;; ____ * Reporting

(org-defkey org-mode-map (kbd "C-c =") 'nomis/org/report-org-info)

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

(org-defkey org-mode-map (kbd "H-M-'")  'nomis/org/show-children/set-0)
(org-defkey org-mode-map (kbd "H-M-\\") 'nomis/org/show-children/fully-expand)
(org-defkey org-mode-map (kbd "H-M-[")  'nomis/org/show-children-from-root/set-0)
(org-defkey org-mode-map (kbd "H-M-]")  'nomis/org/show-children-from-root/fully-expand)
(org-defkey org-mode-map (kbd "H-M--")  'nomis/org/show-children-from-all-roots/set-0)
(org-defkey org-mode-map (kbd "H-M-=")  'nomis/org/show-children-from-all-roots/fully-expand)

(org-defkey org-mode-map (kbd "H-'")  'nomis/org/show-children/incremental/less)
(org-defkey org-mode-map (kbd "H-\\") 'nomis/org/show-children/incremental/more)
(org-defkey org-mode-map (kbd "H-[")  'nomis/org/show-children-from-root/incremental/less)
(org-defkey org-mode-map (kbd "H-]")  'nomis/org/show-children-from-root/incremental/more)
(org-defkey org-mode-map (kbd "H--")  'nomis/org/show-children-from-all-roots/incremental/less)
(org-defkey org-mode-map (kbd "H-=")  'nomis/org/show-children-from-all-roots/incremental/more)

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

(provide 'nomis-org-setup)
