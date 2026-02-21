;;;; nomis-org-key-bindings  ---  -*- lexical-binding: t -*-

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
;;;; ____ * Scrolling

;;;; These normally do `org-shiftcontrolup` and `org-shiftcontroldown`, which do
;;;; things with timestamps. I don't use timestamps. So use the global
;;;; key bindings.
(define-key org-mode-map (kbd "C-S-<up>")     nil)
(define-key org-mode-map (kbd "C-S-<down>")   nil)

;;;; ___________________________________________________________________________
;;;; ____ * Navigation and cycling

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;;; ____ ** Following links

(define-key org-mode-map (kbd "M-.") 'org-open-at-point)
(define-key org-mode-map (kbd "M-,") 'org-mark-ring-goto)

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;;; ____ ** Expand/collapse











;;; The following keys are copied from org.el.
;; TAB key with modifiers
(define-key org-mode-map "\C-i"          'norg/cycle)
(define-key org-mode-map [(tab)]         'norg/cycle)
;; The following line is necessary under Suse GNU/Linux
(define-key org-mode-map [S-iso-lefttab] 'norg/shifttab)
(define-key org-mode-map [(shift tab)]   'norg/shifttab)
(define-key org-mode-map [backtab]       'norg/shifttab)

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;;; ____ ** Movement



;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;;; ____ ** Movement + expand/collapse



(define-key org-mode-map (kbd "C-H-,")   'norg/backward-heading/any-level)
(define-key org-mode-map (kbd "C-H-.")   'norg/forward-heading/any-level)
(define-key org-mode-map (kbd "C-H-M-,") 'norg/backward-heading/any-level/set-tree+body)
(define-key org-mode-map (kbd "C-H-M-.") 'norg/forward-heading/any-level/set-tree+body)

;; (define-key org-mode-map (kbd "C-H-<")   ????) ; No real meaning -- with the M we are already crossing parent levels
;; (define-key org-mode-map (kbd "C-H->")   ????) ; No real meaning -- with the M we are already crossing parent levels
;; (define-key org-mode-map (kbd "C-H-M-<") ????) ; No real meaning -- with the M we are already crossing parent levels
;; (define-key org-mode-map (kbd "C-H-M->") ????) ; No real meaning -- with the M we are already crossing parent levels

;;;; ___________________________________________________________________________
;;;; ____ * Agenda

(progn ; TODO This is not only agenda stuff (contrary to the comment above), and this does not need to be done in a hook (or else everything should go in a hook)
  (defun nomis/setup-org-keys ()
    ;; I don't like RETURN in org agenda giving ORG-AGENDA-SWITCH-TO.
    ;; I prefer this:
    (define-key org-agenda-mode-map "\C-m" 'org-agenda-show)
    (define-key org-agenda-mode-map (kbd "<SPC>") 'org-agenda-show)
    ;; Stuff that got changed when I upgraded to Emacs 26.1 -- this is mad!
    (define-key org-mode-map (kbd "M-S-<down>") 'org-move-subtree-down)
    (define-key org-mode-map (kbd "M-S-<up>")   'org-move-subtree-up))
  (add-hook 'org-mode-hook 'nomis/setup-org-keys))

;;;; ___________________________________________________________________________
;;;; ____ * End

(provide 'nomis-org-key-bindings)
