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
;;;; ____ * Scrolling

;;;; These normally do `org-shiftcontrolup` and `org-shiftcontroldown`. They do
;;;; things with timestamps, which I don't use.
;;;; So use the key bindings to do my normal thing:
(org-defkey org-mode-map (kbd "C-S-<up>")     'nomis/scroll-down-line-in-place)
(org-defkey org-mode-map (kbd "C-S-<down>")   'nomis/scroll-up-line-in-place)

;;;; ___________________________________________________________________________
;;;; ____ * Navigation and cycling

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;;; ____ ** The design of some of these key chords

(defconst -nomis/org/navigation-and-cycling-help
  "Nomis Org Navigation and Cycling Help
=====================================

Use H with various other keys:

    Move forward or backward headlines
        , .
        < > (add S to , . on my keyboard) to cross the parent level
        Add M to step (ie collapse then move then expand)
        Add C to visit headlines at any level
        Add C-M to visit headlines at any level collapsing to current tree

    Expand and collapse from current point
        ' \\
        Add M to fully expand or collapse
        Add C for visibility cycling of spans

    Expand and collapse from root of current point
        \" | (that's S-' and S-\ on my keyboard.)
        Add M to fully expand or collapse

    Expand and collapse from parent of current point
        [ ]
        Add M to fully expand or collapse

    Expands and collapses all roots
        - =
        Add M to fully expand or collapse

H-q H-]  norg/show-children-from-root/to-current-level
H-q H-=  norg/show-children-from-all-roots/to-current-level

H-q H-s  nomis/scrolling/toggle-maintain-line-no-in-window

H-?  Show this help")

(defun nomis/org/pop-up-navigation-and-cycling-help ()
  (interactive)
  (case 2
    (1 (let* ((*nomis/popup/message/auto-dismiss?* nil))
         (nomis/popup/message "%s" -nomis/org/navigation-and-cycling-help)))
    (2 (with-help-window (help-buffer)
         (princ -nomis/org/navigation-and-cycling-help)))))

(org-defkey org-mode-map (kbd "H-?") 'nomis/org/pop-up-navigation-and-cycling-help)

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;;; ____ ** Following links

(org-defkey org-mode-map (kbd "M-.") 'org-open-at-point)
(org-defkey org-mode-map (kbd "M-,") 'org-mark-ring-goto)

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;;; ____ ** nomis/org-search-heading-text

(org-defkey org-mode-map (kbd "H-S")      'nomis/org-search-heading-text)
(org-defkey org-mode-map (kbd "H-s")      'nomis/org-search-heading-text-again)

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;;; ____ ** Visibility span

(org-defkey org-mode-map (kbd "C-H-'")    'nomis/org-visibility-span/less)
(org-defkey org-mode-map (kbd "C-H-\\")   'nomis/org-visibility-span/more)
(org-defkey org-mode-map (kbd "C-H-M-'")  'nomis/org-visibility-span/set-min)
(org-defkey org-mode-map (kbd "C-H-M-\\") 'nomis/org-visibility-span/set-max)

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;;; ____ ** Expand/collapse

(org-defkey org-mode-map (kbd "H-'")     'norg/show-children-from-point/incremental/less)
(org-defkey org-mode-map (kbd "H-\\")    'norg/show-children-from-point/incremental/more)
(org-defkey org-mode-map (kbd "H-M-'")   'norg/show-children-from-point/set-min)
(org-defkey org-mode-map (kbd "H-M-\\")  'norg/show-children-from-point/fully-expand)

(org-defkey org-mode-map (kbd "H-\"")    'norg/show-children-from-root/incremental/less)
(org-defkey org-mode-map (kbd "H-|")     'norg/show-children-from-root/incremental/more)
(org-defkey org-mode-map (kbd "H-M-\"")  'norg/show-children-from-root/set-min)
(org-defkey org-mode-map (kbd "H-M-|")   'norg/show-children-from-root/fully-expand)

(org-defkey org-mode-map (kbd "H-[")     'norg/show-children-from-parent/incremental/less)
(org-defkey org-mode-map (kbd "H-]")     'norg/show-children-from-parent/incremental/more)
(org-defkey org-mode-map (kbd "H-M-[")   'norg/show-children-from-parent/set-min)
(org-defkey org-mode-map (kbd "H-M-]")   'norg/show-children-from-parent/fully-expand)

(org-defkey org-mode-map (kbd "H--")     'norg/show-children-from-all-roots/incremental/less)
(org-defkey org-mode-map (kbd "H-=")     'norg/show-children-from-all-roots/incremental/more)
(org-defkey org-mode-map (kbd "H-M--")   'norg/show-children-from-all-roots/set-min)
(org-defkey org-mode-map (kbd "H-M-=")   'norg/show-children-from-all-roots/fully-expand)

(org-defkey org-mode-map (kbd "H-q H-]") 'norg/show-children-from-root/to-current-level)
(org-defkey org-mode-map (kbd "H-q H-=") 'norg/show-children-from-all-roots/to-current-level)

;;; The following keys are copied from org.el.
;; TAB key with modifiers
(org-defkey org-mode-map "\C-i"          'norg/cycle)
(org-defkey org-mode-map [(tab)]         'norg/cycle)
;; The following line is necessary under Suse GNU/Linux
(org-defkey org-mode-map [S-iso-lefttab] 'norg/shifttab)
(org-defkey org-mode-map [(shift tab)]   'norg/shifttab)
(define-key org-mode-map [backtab]       'norg/shifttab)

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;;; ____ ** Movement

(org-defkey org-mode-map (kbd "H-,")     'norg/backward-heading-same-level)
(org-defkey org-mode-map (kbd "H-.")     'norg/forward-heading-same-level)
(org-defkey org-mode-map (kbd "H-<")     'norg/backward-heading-same-level/allow-cross-parent)
(org-defkey org-mode-map (kbd "H->")     'norg/forward-heading-same-level/allow-cross-parent)

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;;; ____ ** Movement + expand/collapse

(org-defkey org-mode-map (kbd "H-M-,")   'norg/step-backward)
(org-defkey org-mode-map (kbd "H-M-.")   'norg/step-forward)
(org-defkey org-mode-map (kbd "H-M-<")   'norg/step-backward/allow-cross-parent)
(org-defkey org-mode-map (kbd "H-M->")   'norg/step-forward/allow-cross-parent)

(org-defkey org-mode-map (kbd "C-H-,")   'norg/backward-heading/any-level)
(org-defkey org-mode-map (kbd "C-H-.")   'norg/forward-heading/any-level)
(org-defkey org-mode-map (kbd "C-H-M-,") 'norg/backward-heading/any-level/set-tree+body)
(org-defkey org-mode-map (kbd "C-H-M-.") 'norg/forward-heading/any-level/set-tree+body)

;; (org-defkey org-mode-map (kbd "C-H-<")   ????) ; No real meaning -- with the M we are already crossing parent levels
;; (org-defkey org-mode-map (kbd "C-H->")   ????) ; No real meaning -- with the M we are already crossing parent levels
;; (org-defkey org-mode-map (kbd "C-H-M-<") ????) ; No real meaning -- with the M we are already crossing parent levels
;; (org-defkey org-mode-map (kbd "C-H-M->") ????) ; No real meaning -- with the M we are already crossing parent levels

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;;; ____ ** Maintain cursor position

(org-defkey org-mode-map (kbd "H-q H-s") 'nomis/scrolling/toggle-maintain-line-no-in-window)

;;;; ___________________________________________________________________________
;;;; ____ * Agenda

(progn ; TODO This is not only agenda stuff (contrary to the comment above), and this does not need to be done in a hook (or else everything should go in a hook)
  (defun nomis/setup-org-keys ()
    ;; I don't like RETURN in org agenda giving ORG-AGENDA-SWITCH-TO.
    ;; I prefer this:
    (org-defkey org-agenda-mode-map "\C-m" 'org-agenda-show)
    (org-defkey org-agenda-mode-map (kbd "<SPC>") 'org-agenda-show)
    ;; Stuff that got changed when I upgraded to Emacs 26.1 -- this is mad!
    (org-defkey org-mode-map (kbd "M-S-<down>") 'org-move-subtree-down)
    (org-defkey org-mode-map (kbd "M-S-<up>")   'org-move-subtree-up))
  (add-hook 'org-mode-hook 'nomis/setup-org-keys))

;;;; ___________________________________________________________________________
;;;; ____ * End

(provide 'nomis-org-key-bindings)
