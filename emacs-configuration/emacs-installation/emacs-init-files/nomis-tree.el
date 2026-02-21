;;; nomis-tree -*- lexical-binding: t -*-

;;; Code

;;;; nomis/tree-mode

(defvar nomis/tree-mode-map
  (make-sparse-keymap)
  "Keymap for `nomis/tree-mode'.")

(define-minor-mode nomis/tree-mode
  "Nomis-Tree Minor Mode"
  :group 'nomis/tree
  :keymap nomis/tree-mode-map
  ;; This minor mode exists only as a place to define key bindings, so we don't
  ;; need to do anything when turning on and off.
  )

;;;; Turn on the mode

(add-hook 'org-mode-hook 'nomis/tree-mode)
(add-hook 'outline-minor-mode-hook 'nomis/tree-mode)

;; TODO: Do we want this? Or should we be testing whether `hs-minor-mode` is
;;       active when deciding what to do?
;; (add-hook 'hs-minor-mode-hook 'nomis/tree-mode)

;;;; Utilities

(defun -nomis/tree/outline-mode? ()
  (or (eq major-mode 'outline-mode)
      outline-minor-mode))

(defun -nomis/tree/org-mode? ()
  (eq major-mode 'org-mode))

(defun -nomis/tree/mode ()
  (cond ((-nomis/tree/outline-mode?)
         :outline)
        ((-nomis/tree/org-mode?)
         :org)
        (t
         (error "Unexpected: None of outline-mode, outline-minor-mode or org-mode is active"))))

;;;; Search heading text

(cl-defgeneric nomis/tree/search-heading-text--aux (k))
(cl-defgeneric nomis/tree/search-heading-text-again--aux (k))

(defun nomis/tree/search-heading-text ()
  (interactive)
  (nomis/tree/search-heading-text--aux (-nomis/tree/mode)))

(defun nomis/tree/search-heading-text-again ()
  (interactive)
  (nomis/tree/search-heading-text-again--aux (-nomis/tree/mode)))

;;;; Visibility span

(cl-defgeneric nomis/tree/visibility-span/less--aux (k))
(cl-defgeneric nomis/tree/visibility-span/more--aux (k))
(cl-defgeneric nomis/tree/visibility-span/set-min--aux (k))
(cl-defgeneric nomis/tree/visibility-span/set-max--aux (k))

(defun nomis/tree/visibility-span/less ()
  (interactive)
  (nomis/tree/visibility-span/less--aux (-nomis/tree/mode)))

(defun nomis/tree/visibility-span/more ()
  (interactive)
  (nomis/tree/visibility-span/more--aux (-nomis/tree/mode)))

(defun nomis/tree/visibility-span/set-min ()
  (interactive)
  (nomis/tree/visibility-span/set-min--aux (-nomis/tree/mode)))

(defun nomis/tree/visibility-span/set-max ()
  (interactive)
  (nomis/tree/visibility-span/set-max--aux (-nomis/tree/mode)))

;;;; nomis/tree/show-tree-only and nomis/tree/max-lineage

(cl-defgeneric nomis/tree/show-tree-only--aux (k))
(cl-defgeneric nomis/tree/max-lineage--aux (k))

(defun nomis/tree/show-tree-only ()
  (interactive)
  (nomis/tree/show-tree-only--aux (-nomis/tree/mode)))

(defun nomis/tree/max-lineage ()
  (interactive)
  (nomis/tree/max-lineage--aux (-nomis/tree/mode)))

;;;; nomis/tree/set-step-n-levels-to-show

(cl-defgeneric nomis/tree/set-step-n-levels-to-show--aux (k n))

(defun nomis/tree/set-step-n-levels-to-show (n)
  (interactive "P")
  (nomis/tree/set-step-n-levels-to-show--aux (-nomis/tree/mode) n))

;;;; Expand/collapse from point

(cl-defgeneric nomis/tree/show-children-from-point/incremental/less--aux (k n))

(defun nomis/tree/show-children-from-point/incremental/less (n)
  "Incrementally collapse the current heading by `arg` levels, default 1.
When in a body, \"current heading\" means the current body's parent heading."
  (interactive "P")
  (nomis/tree/show-children-from-point/incremental/less--aux (-nomis/tree/mode)
                                                             n))

(cl-defgeneric nomis/tree/show-children-from-point/incremental/more--aux (k n))

(defun nomis/tree/show-children-from-point/incremental/more (n)
  "Incrementally expand the current heading by `arg` levels, default 1.
When in a body, \"current heading\" means the current body's parent heading."
  (interactive "P")
  (nomis/tree/show-children-from-point/incremental/more--aux (-nomis/tree/mode)
                                                             n))

(cl-defgeneric nomis/tree/show-children-from-point/set-min--aux (k))

(defun nomis/tree/show-children-from-point/set-min ()
  "Fully collapse the current heading.
When in a body, \"current heading\" means the current body's parent heading."
  (interactive)
  (nomis/tree/show-children-from-point/set-min--aux (-nomis/tree/mode)))

(cl-defgeneric nomis/tree/show-children-from-point/fully-expand--aux (k))

(defun nomis/tree/show-children-from-point/fully-expand ()
  "Fully expand the current heading.
When in a body, \"current heading\" means the current body's parent heading."
  (interactive)
  (nomis/tree/show-children-from-point/fully-expand--aux (-nomis/tree/mode)))

;;;; Expand/collapse from parent

(cl-defgeneric nomis/tree/show-children-from-parent/incremental/less--aux (k n))

(defun nomis/tree/show-children-from-parent/incremental/less (n)
  "Like `nomis/tree/show-children-from-point/incremental/less`, but
from the current entry's parent and with the parent always
expanded at least one level."
  (interactive "P")
  (nomis/tree/show-children-from-parent/incremental/less--aux (-nomis/tree/mode) n))

(cl-defgeneric nomis/tree/show-children-from-parent/incremental/more--aux (k n))

(defun nomis/tree/show-children-from-parent/incremental/more (n)
  "Like `nomis/tree/show-children-from-point/incremental/more`, but
from the current entry's parent."
  (interactive "P")
  (nomis/tree/show-children-from-parent/incremental/more--aux (-nomis/tree/mode) n))

(cl-defgeneric nomis/tree/show-children-from-parent/set-min--aux (k))

(defun nomis/tree/show-children-from-parent/set-min ()
  "Like `nomis/tree/show-children-from-point/set-min`, but from the
current entry's parent and showing one level."
  (interactive)
  (nomis/tree/show-children-from-parent/set-min--aux (-nomis/tree/mode)))

(cl-defgeneric nomis/tree/show-children-from-parent/fully-expand--aux (k))

(defun nomis/tree/show-children-from-parent/fully-expand ()
  "Like `nomis/tree/show-children-from-point/fully-expand`, but from
the current entry's parent."
  (interactive)
  (nomis/tree/show-children-from-parent/fully-expand--aux (-nomis/tree/mode)))

;;;; Expand/collapse from root -- to current level, and from all roots -- to current level

(cl-defgeneric nomis/tree/show-children-from-root/to-current-level--aux (k))

(defun nomis/tree/show-children-from-root/to-current-level ()
  (interactive)
  (nomis/tree/show-children-from-root/to-current-level--aux (-nomis/tree/mode)))

(cl-defgeneric nomis/tree/show-children-from-all-roots/to-current-level--aux (k))

(defun nomis/tree/show-children-from-all-roots/to-current-level ()
  (interactive)
  (nomis/tree/show-children-from-all-roots/to-current-level--aux (-nomis/tree/mode)))

;;;; Expand/collapse from all roots

(cl-defgeneric nomis/tree/show-children-from-all-roots/incremental/less--aux (k n))

(defun nomis/tree/show-children-from-all-roots/incremental/less (n)
  "Incrementally collapse all roots by `arg` levels, default 1."
  (interactive "P")
  (nomis/tree/show-children-from-all-roots/incremental/less--aux (-nomis/tree/mode) n))

(cl-defgeneric nomis/tree/show-children-from-all-roots/incremental/more--aux (k n))

(defun nomis/tree/show-children-from-all-roots/incremental/more (n)
  "Incrementally expand all roots by `arg` levels, default 1."
  (interactive "P")
  (nomis/tree/show-children-from-all-roots/incremental/more--aux (-nomis/tree/mode) n))

(cl-defgeneric nomis/tree/show-children-from-all-roots/set-min--aux (k))

(defun nomis/tree/show-children-from-all-roots/set-min ()
  (interactive)
  (nomis/tree/show-children-from-all-roots/set-min--aux (-nomis/tree/mode)))

(cl-defgeneric nomis/tree/show-children-from-all-roots/fully-expand--aux (k))

(defun nomis/tree/show-children-from-all-roots/fully-expand ()
  (interactive)
  (nomis/tree/show-children-from-all-roots/fully-expand--aux (-nomis/tree/mode)))

;;;; Expand/collapse from root

(cl-defgeneric nomis/tree/show-children-from-root/incremental/less--aux (k n))

(defun nomis/tree/show-children-from-root/incremental/less (n)
  "Incrementally collapse the current root by `arg` levels, default 1."
  (interactive "P")
  (nomis/tree/show-children-from-root/incremental/less--aux (-nomis/tree/mode) n))

(cl-defgeneric nomis/tree/show-children-from-root/incremental/more--aux (k n))

(defun nomis/tree/show-children-from-root/incremental/more (n)
  "Incrementally expand the current root by `arg` levels, default 1."
  (interactive "P")
  (nomis/tree/show-children-from-root/incremental/more--aux (-nomis/tree/mode) n))

(cl-defgeneric nomis/tree/show-children-from-root/set-min--aux (k))

(defun nomis/tree/show-children-from-root/set-min ()
  (interactive)
  (nomis/tree/show-children-from-root/set-min--aux (-nomis/tree/mode)))

(cl-defgeneric nomis/tree/show-children-from-root/fully-expand--aux (k))

(defun nomis/tree/show-children-from-root/fully-expand ()
  (interactive)
  (nomis/tree/show-children-from-root/fully-expand--aux (-nomis/tree/mode)))

;;;; Movement

(cl-defgeneric nomis/tree/previous-sibling--aux (k))

(defun nomis/tree/previous-sibling ()
  "Move backward one heading at the same level as this one."
  (interactive)
  (nomis/tree/previous-sibling--aux (-nomis/tree/mode)))

(cl-defgeneric nomis/tree/next-sibling--aux (k))

(defun nomis/tree/next-sibling ()
  "Move forward one heading at the same level as this one."
  (interactive)
  (nomis/tree/next-sibling--aux (-nomis/tree/mode)))

(cl-defgeneric nomis/tree/previous-sibling/allow-cross-parent--aux (k))

(defun nomis/tree/previous-sibling/allow-cross-parent ()
  "Move backward one heading at the same level, crossing parent boundaries."
  (interactive)
  (nomis/tree/previous-sibling/allow-cross-parent--aux (-nomis/tree/mode)))

(cl-defgeneric nomis/tree/next-sibling/allow-cross-parent--aux (k))

(defun nomis/tree/next-sibling/allow-cross-parent ()
  "Move forward one heading at the same level, crossing parent boundaries."
  (interactive)
  (nomis/tree/next-sibling/allow-cross-parent--aux (-nomis/tree/mode)))

;;; End

(provide 'nomis-tree)
