;;; nomis-tree-key-bindings.el ---  -*- lexical-binding: t; -*-

;;; Code:

;;;; Requires

(require 'nomis-msg)
(require 'nomis-outline-common)
(require 'nomis-tree)
(require 'nomis-very-general-stuff-new-lexical)
(require 'org)
(require 'projectile)

;;;; Temporary keybindings to train myself for change of Projectile keybindings

(defun -nomis/outline/projectile-keybinding-error ()
  (interactive)
  (nomis/msg/pulse-buffer-error)
  (nomis/temporarily-disable-keys t) ; avoid accidental input
  (error "Nope, Projectile is M-o now"))

(define-key projectile-mode-map (kbd "H-o d") '-nomis/outline/projectile-keybinding-error)
;; We are using this for `outline-minor-faces-mode` now:
;; (define-key projectile-mode-map (kbd "H-o f") 'nomis/outline/projectile-keybinding-error)
(define-key projectile-mode-map (kbd "H-o g") '-nomis/outline/projectile-keybinding-error)

;;;; nomis/tree/pop-up-help

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


H-M-m        nomis/tree/show-tree-only

H-q H-q H-s  nomis/tree/set-step-n-levels-to-show
H-q H-q H-m  nomis/scrolling/toggle-maintain-line-no-in-window

H-q H-q H-]  nomis/tree/show-children-from-root/to-current-level
H-q H-q H-=  nomis/tree/show-children-from-all-roots/to-current-level

H-q H-q H-/  Show this help")

(defun nomis/tree/pop-up-help ()
  (interactive)
  (cl-case 2
    (1 (let* ((*nomis/popup/message/auto-dismiss?* nil))
         (nomis/popup/message "%s" -nomis/tree/help)))
    (2 (with-help-window (help-buffer)
         (princ -nomis/tree/help)))))

(define-key nomis/tree-mode-map (kbd "H-q H-q H-/") 'nomis/tree/pop-up-help)


;;;; Search heading text

(define-key nomis/tree-mode-map (kbd "H-S")      'nomis/tree/search-heading-text)
(define-key nomis/tree-mode-map (kbd "H-s")      'nomis/tree/search-heading-text-again)

;;;; Visibility span

(define-key nomis/tree-mode-map (kbd "C-H-'")    'nomis/tree/visibility-span/less)
(define-key nomis/tree-mode-map (kbd "C-H-\\")   'nomis/tree/visibility-span/more)
(define-key nomis/tree-mode-map (kbd "C-H-M-'")  'nomis/tree/visibility-span/set-min)
(define-key nomis/tree-mode-map (kbd "C-H-M-\\") 'nomis/tree/visibility-span/set-max)

;;;; nomis/tree/show-tree-only and nomis/tree/max-lineage

(define-key nomis/tree-mode-map (kbd "H-M-m")     'nomis/tree/show-tree-only)
(define-key nomis/tree-mode-map (kbd "H-M-M")     'nomis/tree/max-lineage)

;;;; nomis/tree/set-step-n-levels-to-show

(define-key nomis/tree-mode-map (kbd "H-q H-q H-s") 'nomis/tree/set-step-n-levels-to-show)

;;;; Expand/collapse

(define-key nomis/tree-mode-map (kbd "H-'")       'nomis/tree/show-children-from-point/incremental/less)
(define-key nomis/tree-mode-map (kbd "H-\\")      'nomis/tree/show-children-from-point/incremental/more)
(define-key nomis/tree-mode-map (kbd "H-M-'")     'nomis/tree/show-children-from-point/set-min)
(define-key nomis/tree-mode-map (kbd "H-M-\\")    'nomis/tree/show-children-from-point/fully-expand)
(define-key nomis/tree-mode-map (kbd "H-\"")      'nomis/tree/show-children-from-root/incremental/less)
(define-key nomis/tree-mode-map (kbd "H-|")       'nomis/tree/show-children-from-root/incremental/more)
(define-key nomis/tree-mode-map (kbd "H-M-\"")    'nomis/tree/show-children-from-root/set-min)
(define-key nomis/tree-mode-map (kbd "H-M-|")     'nomis/tree/show-children-from-root/fully-expand)
(define-key nomis/tree-mode-map (kbd "H-[")       'nomis/tree/show-children-from-parent/incremental/less)
(define-key nomis/tree-mode-map (kbd "H-]")       'nomis/tree/show-children-from-parent/incremental/more)
(define-key nomis/tree-mode-map (kbd "H-M-[")     'nomis/tree/show-children-from-parent/set-min)
(define-key nomis/tree-mode-map (kbd "H-M-]")     'nomis/tree/show-children-from-parent/fully-expand)
(define-key nomis/tree-mode-map (kbd "H--")       'nomis/tree/show-children-from-all-roots/incremental/less)
(define-key nomis/tree-mode-map (kbd "H-=")       'nomis/tree/show-children-from-all-roots/incremental/more)
(define-key nomis/tree-mode-map (kbd "H-M--")     'nomis/tree/show-children-from-all-roots/set-min)
(define-key nomis/tree-mode-map (kbd "H-M-=")     'nomis/tree/show-children-from-all-roots/fully-expand)
(define-key nomis/tree-mode-map (kbd "H-q H-q H-]") 'nomis/tree/show-children-from-root/to-current-level)
(define-key nomis/tree-mode-map (kbd "H-q H-q H-=") 'nomis/tree/show-children-from-all-roots/to-current-level)

;;;; Tab and shifttab

;;;;; Tab

;; Preserve access to built-in `tab` functionality. Arguably this
;; belongs in `nomis-org-key-bindings`, but perhaps having it here is clearer.

(dolist (key `(,(kbd "H-TAB")
               ,(kbd "<f18>") ; H-TAB via Karabiner Elements
               ))
  (define-key org-mode-map key 'org-cycle))

;; Override the `tab` key binding.

(dolist (key `(;; These keys are copied from `org`.
               ,(kbd "TAB")))
  (nomis/define-key-with-filter nomis/tree-mode-map
                                key
                                'nomis/tree/show-children-from-point/incremental/more
                                (nomis/outline/on-heading?)))

;;;;; Shifttab

;; Preserve access to built-in `shifttab` functionality. See the comment for
;; `Tab`.

(dolist (key `(,(kbd "H-S-TAB")
               ,(kbd "<f19>") ; H-S-TAB via Karabiner Elements
               ))
  (define-key org-mode-map key 'org-shifttab))

;; Override the `shifttab` key binding.

(dolist (key `(;; These keys are copied from `org`.
               ,(kbd "S-TAB")
               ,(kbd "<backtab>")))
  (nomis/define-key-with-filter nomis/tree-mode-map
                                key
                                'nomis/tree/show-children-from-point/incremental/less
                                (nomis/outline/on-heading?)))

;;;; `bicycle-cycle-global`

;; Keep this until we implement
;; `nomis/tree/show-children-from-all-roots/incremental/less--aux` and
;; `nomis/tree/show-children-from-all-roots/incremental/more--aux`.
(define-key nomis/tree-mode-map (kbd "H-o <tab>")   'bicycle-cycle-global)

;;;; Movement

(define-key nomis/tree-mode-map (kbd "H-,")       'nomis/tree/previous-sibling)
(define-key nomis/tree-mode-map (kbd "H-.")       'nomis/tree/next-sibling)
(define-key nomis/tree-mode-map (kbd "H-<")       'nomis/tree/previous-peer)
(define-key nomis/tree-mode-map (kbd "H->")       'nomis/tree/next-peer)

;;;; Movement + expand/collapse

(define-key nomis/tree-mode-map (kbd "H-M-,")     'nomis/tree/step-backward-sibling)
(define-key nomis/tree-mode-map (kbd "H-M-.")     'nomis/tree/step-forward-sibling)
(define-key nomis/tree-mode-map (kbd "H-M-<")     'nomis/tree/step-backward-peer)
(define-key nomis/tree-mode-map (kbd "H-M->")     'nomis/tree/step-forward-peer)

(define-key nomis/tree-mode-map (kbd "C-H-,")     'nomis/tree/previous-heading)
(define-key nomis/tree-mode-map (kbd "C-H-.")     'nomis/tree/next-heading)

(define-key nomis/tree-mode-map (kbd "C-H-M-,")   'nomis/tree/previous-heading/set-tree+body)
(define-key nomis/tree-mode-map (kbd "C-H-M-.")   'nomis/tree/next-heading/set-tree+body)

;; The following key bindings have no good meaning in our scheme -- Shift means
;; "navigat to peer", and we are already doing that without the Shift.
;;
;; `(kbd "C-H-<")`
;; `(kbd "C-H->")`
;; `(kbd "C-H-M-<")`
;; `(kbd "C-H-M->")`

;;;; nomis/scrolling/toggle-maintain-line-no-in-window

(dolist (key `(,(kbd "H-q H-q H-m")
               ,(kbd "H-q H-q <f17>") ; H-m via Karabiner Elements
               ))
  (define-key nomis/tree-mode-map key 'nomis/scrolling/toggle-maintain-line-no-in-window))

;;; End

(provide 'nomis-tree-key-bindings)
