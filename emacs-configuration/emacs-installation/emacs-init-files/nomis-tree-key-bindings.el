;;; nomis-tree-key-bindings.el ---  -*- lexical-binding: t; -*-

;;; Code:

;;;; Requires

(require 'nomis-msg)
(require 'nomis-outline-wrappers)
(require 'nomis-tree)
(require 'nomis-very-general-stuff-new-lexical)
(require 'org)
(require 'projectile)

;;;; Temporary keybindings to train myself for change of Projectile keybindings

(defun -nomis/tree-key-bindings/projectile-keybinding-error ()
  (interactive)
  (nomis/msg/pulse-buffer-error)
  (nomis/temporarily-disable-keys t) ; avoid accidental input
  (error "Nope, Projectile is M-o now"))

;;;; nomis/tree/pop-up-help

(defconst -nomis/tree/help
  "Nomis Tree Help
===============

Escape hatch to Org mode
------------------------

H-TAB    org-cycle
H-S-TAB  org-shifttab


The main commands
-----------------

Use H with various other keys:

    Move forward or backward headings
        H-, H-.  -- Navigate to siblings
        H-< H->  -- Cross the parent level (add S to , . on my keyboard)
        Add M    -- Step (ie collapse then move then expand)
        Add C    -- Visit headings at any level
        Add C-M  -- Step + visit headings at any level

    Expand and collapse from current point
        H-' H-\\
        Add M    -- Fully expand or collapse
        Add C    -- Cycle lineage levels

    When on a heading
        Tab is the same as H-\\, but can't be used with M and C
        Shittab is the same as H-', but can't be used with M and C

    Expand and collapse from root of current point
        \" | (that's S-' and S-\ on my keyboard.)
        Add M to fully expand or collapse

    Expand and collapse from parent of current point
        [ ]
        Add M to fully expand or collapse

    Expands and collapses all roots
        - =
        Add M to fully expand or collapse


Other commands
--------------

H-M-m  Show fat parents
H-M-M  Show fat parents and fully expand children

H-o u  Move up one heading level

H-o h  Show only headings
H-o ]  Show children from root to current level
H-o =  Show children from all roots to current level

H-o l  Set # levels to show when stepping
H-o m  Toggle maintain-line-no-in-window when scrolling

H-o S  Search heading text for the text of this heading
H-o s  Repeat search heading text

H-o ?  Show this help")

(defun nomis/tree/pop-up-help ()
  (interactive)
  (cl-case 2
    (1 (let* ((*nomis/popup/message/auto-dismiss?* nil))
         (nomis/popup/message "%s" -nomis/tree/help)))
    (2 (with-help-window (help-buffer)
         (princ -nomis/tree/help)))))

;;;; Lineage

(define-key nomis/tree-mode-map (kbd "C-H-'")    'nomis/tree/lineage/less)
(define-key nomis/tree-mode-map (kbd "C-H-\\")   'nomis/tree/lineage/more)
(define-key nomis/tree-mode-map (kbd "C-H-M-'")  'nomis/tree/lineage/set-min)
(define-key nomis/tree-mode-map (kbd "C-H-M-\\") 'nomis/tree/lineage/set-max)

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
                                (nomis/outline/w/on-heading?)))

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
                                (nomis/outline/w/on-heading?)))

;;;; Movement

(define-key nomis/tree-mode-map (kbd "C-H-,")     'nomis/tree/previous-heading)
(define-key nomis/tree-mode-map (kbd "C-H-.")     'nomis/tree/next-heading)
(define-key nomis/tree-mode-map (kbd "H-,")       'nomis/tree/previous-sibling)
(define-key nomis/tree-mode-map (kbd "H-.")       'nomis/tree/next-sibling)
(define-key nomis/tree-mode-map (kbd "H-<")       'nomis/tree/previous-peer)
(define-key nomis/tree-mode-map (kbd "H->")       'nomis/tree/next-peer)

;;;; Movement + expand/collapse

(define-key nomis/tree-mode-map (kbd "C-H-M-,")   'nomis/tree/step-backward-any-level)
(define-key nomis/tree-mode-map (kbd "C-H-M-.")   'nomis/tree/step-forward-any-level)
(define-key nomis/tree-mode-map (kbd "H-M-,")     'nomis/tree/step-backward-sibling)
(define-key nomis/tree-mode-map (kbd "H-M-.")     'nomis/tree/step-forward-sibling)
(define-key nomis/tree-mode-map (kbd "H-M-<")     'nomis/tree/step-backward-peer)
(define-key nomis/tree-mode-map (kbd "H-M->")     'nomis/tree/step-forward-peer)

;; The following key bindings have no good meaning in our scheme -- Shift means
;; "navigat to peer", and we are already doing that without the Shift.
;;
;; `(kbd "C-H-<")`
;; `(kbd "C-H->")`
;; `(kbd "C-H-M-<")`
;; `(kbd "C-H-M->")`

;;;; Projectile commands are no longer here

(define-key projectile-mode-map (kbd "H-o d") '-nomis/tree-key-bindings/projectile-keybinding-error)
;; We are using this for `outline-minor-faces-mode` now:
;; (define-key projectile-mode-map (kbd "H-o f") '-nomis/tree-key-bindings/projectile-keybinding-error)
(define-key projectile-mode-map (kbd "H-o g") '-nomis/tree-key-bindings/projectile-keybinding-error)

;;;; nomis/tree/search-heading-text

(define-key nomis/tree-mode-map (kbd "H-S") 'nomis/tree/search-heading-text)
(define-key nomis/tree-mode-map (kbd "H-s") 'nomis/tree/search-heading-text-again)

;;;; nomis/tree/kb/map

(defvar-keymap nomis/tree/kb/map
  :doc "Keymap for nomis/tree commands.")

(define-key nomis/tree-mode-map (kbd "H-o") nomis/tree/kb/map)

;; TODO: Keep this until we implement
;; `nomis/tree/show-children-from-all-roots/incremental/less--aux` and
;; `nomis/tree/show-children-from-all-roots/incremental/more--aux`.
(define-key nomis/tree/kb/map (kbd "<tab>") 'bicycle-cycle-global)

(define-key nomis/tree/kb/map (kbd "u") 'nomis/tree/up-heading)

(define-key nomis/tree/kb/map (kbd "h") 'outline-show-only-headings)
(define-key nomis/tree/kb/map (kbd "]") 'nomis/tree/show-children-from-root/to-current-level)
(define-key nomis/tree/kb/map (kbd "=") 'nomis/tree/show-children-from-all-roots/to-current-level)

(define-key nomis/tree/kb/map (kbd "l") 'nomis/tree/set-step-n-levels-to-show)
(define-key nomis/tree/kb/map (kbd "m") 'nomis/scrolling/toggle-maintain-line-no-in-window)

(define-key nomis/tree/kb/map (kbd "?") 'nomis/tree/pop-up-help)

;;;; nomis/tree/fat-parents-no-children and nomis/tree/max-lineage

;; TODO: outline-todo: Reconsider what we want here.

(define-key nomis/tree-mode-map (kbd "H-M-m")     'nomis/tree/fat-parents-no-children)
(define-key nomis/tree-mode-map (kbd "H-M-M")     'nomis/tree/max-lineage)

;;; End

(provide 'nomis-tree-key-bindings)
