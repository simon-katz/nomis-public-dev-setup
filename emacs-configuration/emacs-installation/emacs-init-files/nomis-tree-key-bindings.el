;;; nomis-tree-key-bindings -*- lexical-binding: t -*-

;;; Temporary keybindings to train myself for change of Projectile keybindings

(defun -nomis/outline/projectile-keybinding-error ()
  (interactive)
  (nomis/msg/pulse-buffer-error)
  (nomis/temporarily-disable-keys t) ; avoid accidental input
  (error "Nope, Projectile is M-o now"))

(define-key projectile-mode-map (kbd "H-o d") '-nomis/outline/projectile-keybinding-error)
(define-key projectile-mode-map (kbd "H-o f") '-nomis/outline/projectile-keybinding-error)
(define-key projectile-mode-map (kbd "H-o g") '-nomis/outline/projectile-keybinding-error)

;;; nomis/tree/pop-up-help

(defconst -nomis/tree/help
  "Nomis Tree Help
===============

Use H with various other keys:

    Move forward or backward headings
        , .
        < > (add S to , . on my keyboard) to cross the parent level
        Add M to step (ie collapse then move then expand)
        Add C to visit headings at any level
        Add C-M to visit headings at any level collapsing to current tree

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


H-M-m    norg/show-tree-only

H-q H-s  norg/set-step-n-levels-to-show
H-q H-m  nomis/scrolling/toggle-maintain-line-no-in-window

H-q H-]  norg/show-children-from-root/to-current-level
H-q H-=  norg/show-children-from-all-roots/to-current-level

H-q H-/  Show this help")

(defun nomis/tree/pop-up-help ()
  (interactive)
  (cl-case 2
    (1 (let* ((*nomis/popup/message/auto-dismiss?* nil))
         (nomis/popup/message "%s" -nomis/tree/help)))
    (2 (with-help-window (help-buffer)
         (princ -nomis/tree/help)))))

(define-key nomis/tree-mode-map (kbd "H-q H-q /") 'nomis/tree/pop-up-help)

;;; End

(provide 'nomis-tree-key-bindings)
