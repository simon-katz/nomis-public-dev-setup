;;; nomis-tree.el --- A protocol for navigating trees in files  -*- lexical-binding: t; -*-

;;; Code:

;;;; Requires

(require 'a)
(require 'nomis-scrolling)
(require 'nomis-tree-impl)
(require 'outline)
(require 'nomis-msg)
(require 'nomis-outline-wrappers)

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

;;;; Utilities

;;;;; -nomis/tree/command

(defun -nomis/tree/command* (opts f)
  (cl-flet* ((do-it () (nomis/scrolling/with-maybe-maintain-line-no-in-window
                         (funcall f))))
    (if (or mark-active
            (a-get opts :no-push-mark))
        (do-it)
      (let* ((start-point (point)))
        (prog1
            (do-it)
          (let* ((end-point (point)))
            (unless (= start-point end-point)
              (push-mark start-point
                         ;; Use `nomsg` arg in case `f` has displayed
                         ;; a message already.
                         t))))))))

(cl-defmacro -nomis/tree/command (opts &body body)
  (declare (indent 1))
  `(-nomis/tree/command* ,opts (lambda () ,@body)))

;;;; API

;;;;; Whether to show bodies

(defun nomis/tree/toggle-show-bodies ()
  (interactive)
  (nomis/tree/impl/toggle-show-bodies))

;;;;; Search heading text

(defun nomis/tree/search-heading-text ()
  (interactive)
  (-nomis/tree/command
      nil
    (nomis/tree/impl/search-heading-text)))

(defun nomis/tree/search-heading-text-again ()
  (interactive)
  (-nomis/tree/command
      (a-list :no-push-mark t)
    (nomis/tree/impl/search-heading-text-again)))

;;;;; Lineage

(defun nomis/tree/lineage/less ()
  (interactive)
  (-nomis/tree/command
      nil
    (nomis/tree/ls/lineage/less)))

(defun nomis/tree/lineage/more ()
  (interactive)
  (-nomis/tree/command
      nil
    (nomis/tree/ls/lineage/more)))

(defun nomis/tree/lineage/set-min ()
  (interactive)
  (-nomis/tree/command
      nil
    (nomis/tree/ls/lineage/set-min)))

(defun nomis/tree/lineage/set-max ()
  (interactive)
  (-nomis/tree/command
      nil
    (nomis/tree/ls/lineage/set-max)))

;;;;; nomis/tree/set-step-n-levels-to-show

(defun nomis/tree/set-step-n-levels-to-show (n)
  (interactive "P")
  (-nomis/tree/command
      nil
    (nomis/tree/impl/set-step-n-levels-to-show n)))

;;;;; Expand/collapse from point

(defun nomis/tree/show-children-from-point/incremental/less (n)
  "Incrementally collapse the current heading by 1 level.
With a numeric prefix `N`, set the number of visible levels to exactly `N`.
When in a body, \"current heading\" means the current body's parent heading."
  (interactive "P")
  (-nomis/tree/command
      nil
    (nomis/tree/impl/show-children-from-point/incremental/less n)))

(defun nomis/tree/show-children-from-point/incremental/more (n)
  "Incrementally expand the current heading by 1 level.
With a numeric prefix `N`, set the number of visible levels to exactly `N`.
When in a body, \"current heading\" means the current body's parent heading."
  (interactive "P")
  (-nomis/tree/command
      nil
    (nomis/tree/impl/show-children-from-point/incremental/more n)))

(defun nomis/tree/show-children-from-point/set-min ()
  "Fully collapse the current heading.
When in a body, \"current heading\" means the current body's parent heading."
  (interactive)
  (-nomis/tree/command
      nil
    (nomis/tree/impl/show-children-from-point/set-min)))

(defun nomis/tree/show-children-from-point/fully-expand ()
  "Fully expand the current heading.
When in a body, \"current heading\" means the current body's parent heading."
  (interactive)
  (-nomis/tree/command
      nil
    (nomis/tree/impl/show-children-from-point/fully-expand)))

;;;;; Expand/collapse from parent

(defun nomis/tree/show-children-from-parent/incremental/less (n)
  "Like `nomis/tree/show-children-from-point/incremental/less`, but from
the current entry's parent and with the parent always expanded at least
one level."
  (interactive "P")
  (-nomis/tree/command
      nil
    (nomis/tree/impl/show-children-from-parent/incremental/less n)))

(defun nomis/tree/show-children-from-parent/incremental/more (n)
  "Like `nomis/tree/show-children-from-point/incremental/more`, but from
the current entry's parent."
  (interactive "P")
  (-nomis/tree/command
      nil
    (nomis/tree/impl/show-children-from-parent/incremental/more n)))

(defun nomis/tree/show-children-from-parent/set-min ()
  "Like `nomis/tree/show-children-from-point/set-min`, but from the
current entry's parent and showing one level."
  (interactive)
  (-nomis/tree/command
      nil
    (nomis/tree/impl/show-children-from-parent/set-min)))

(defun nomis/tree/show-children-from-parent/fully-expand ()
  "Like `nomis/tree/show-children-from-point/fully-expand`, but from
the current entry's parent."
  (interactive)
  (-nomis/tree/command
      nil
    (nomis/tree/impl/show-children-from-parent/fully-expand)))

;;;;; Expand/collapse from root -- to current level, and from all roots -- to current level

(defun nomis/tree/show-children-from-root/to-current-level ()
  "Expand the root of the current heading to the current heading's level."
  (interactive)
  (-nomis/tree/command
      nil
    (nomis/tree/impl/show-children-from-root/to-current-level)))

(defun nomis/tree/show-children-from-all-roots/to-current-level ()
  "Expand all roots to the current heading's level."
  (interactive)
  (-nomis/tree/command
      nil
    (nomis/tree/impl/show-children-from-all-roots/to-current-level)))

;;;;; Expand/collapse from all roots

(defun nomis/tree/show-children-from-all-roots/incremental/less (n)
  "Incrementally collapse all roots by 1 level.
With a numeric prefix `N`, set the number of visible levels to exactly `N`."
  (interactive "P")
  (-nomis/tree/command
      nil
    (nomis/tree/impl/show-children-from-all-roots/incremental/less n)))

(defun nomis/tree/show-children-from-all-roots/incremental/more (n)
  "Incrementally expand all roots by 1 level.
With a numeric prefix `N`, set the number of visible levels to exactly `N`."
  (interactive "P")
  (-nomis/tree/command
      nil
    (nomis/tree/impl/show-children-from-all-roots/incremental/more n)))

(defun nomis/tree/show-children-from-all-roots/set-min ()
  "Fully collapse all roots."
  (interactive)
  (-nomis/tree/command
      nil
    (nomis/tree/impl/show-children-from-all-roots/set-min)))

(defun nomis/tree/show-children-from-all-roots/fully-expand ()
  "Fully expand all roots."
  (interactive)
  (-nomis/tree/command
      nil
    (nomis/tree/impl/show-children-from-all-roots/fully-expand)
    (nomis/msg/pulse-buffer)))

;;;;; Expand/collapse from root

(defun nomis/tree/show-children-from-root/incremental/less (n)
  "Incrementally collapse the current root by 1 level.
With a numeric prefix `N`, set the number of visible levels to exactly `N`."
  (interactive "P")
  (-nomis/tree/command
      nil
    (nomis/tree/impl/show-children-from-root/incremental/less n)))

(defun nomis/tree/show-children-from-root/incremental/more (n)
  "Incrementally expand the current root by 1 level.
With a numeric prefix `N`, set the number of visible levels to exactly `N`."
  (interactive "P")
  (-nomis/tree/command
      nil
    (nomis/tree/impl/show-children-from-root/incremental/more n)))

(defun nomis/tree/show-children-from-root/set-min ()
  "Fully collapse the root of the current heading."
  (interactive)
  (-nomis/tree/command
      nil
    (nomis/tree/impl/show-children-from-root/set-min)))

(defun nomis/tree/show-children-from-root/fully-expand ()
  "Fully expand the root of the current heading."
  (interactive)
  (-nomis/tree/command
      nil
    (nomis/tree/impl/show-children-from-root/fully-expand)))

;;;;; Movement

(defun nomis/tree/up-heading (n)
  (interactive "p")
  (-nomis/tree/command
      nil
    (nomis/outline/w/up-heading n)))

(defun nomis/tree/previous-heading ()
  "Move backward to the previous heading at any level."
  (interactive)
  (-nomis/tree/command
      nil
    (nomis/tree/impl/previous-heading)))

(defun nomis/tree/next-heading ()
  "Move forward to the next heading at any level."
  (interactive)
  (-nomis/tree/command
      nil
    (nomis/tree/impl/next-heading)))

(defun nomis/tree/previous-sibling ()
  "Move backward one heading at the same level as this one."
  (interactive)
  (-nomis/tree/command
      nil
    (nomis/tree/impl/previous-sibling)))

(defun nomis/tree/next-sibling ()
  "Move forward one heading at the same level as this one."
  (interactive)
  (-nomis/tree/command
      nil
    (nomis/tree/impl/next-sibling)))

(defun nomis/tree/previous-peer ()
  "Move backward one heading at the same level, crossing parent boundaries."
  (interactive)
  (-nomis/tree/command
      nil
    (nomis/tree/impl/previous-peer)))

(defun nomis/tree/next-peer ()
  "Move forward one heading at the same level, crossing parent boundaries."
  (interactive)
  (-nomis/tree/command
      nil
    (nomis/tree/impl/next-peer)))

;;;;; Movement + expand/collapse

(defun nomis/tree/step-backward-any-level (n)
  "Move backward to the previous heading at any level, then expand it."
  (interactive "P")
  (-nomis/tree/command
      nil
    (nomis/tree/impl/step-backward-any-level n)))

(defun nomis/tree/step-forward-any-level (n)
  "Move forward to the next heading at any level, then expand it."
  (interactive "P")
  (-nomis/tree/command
      nil
    (nomis/tree/impl/step-forward-any-level n)))

(defun nomis/tree/step-backward-sibling (n-levels-to-show-or-nil)
  "Move backward to the previous heading at the same level, then expand it.
Stops at parent boundaries."
  (interactive "P")
  (-nomis/tree/command
      nil
    (nomis/tree/impl/step-backward-sibling n-levels-to-show-or-nil)))

(defun nomis/tree/step-forward-sibling (n-levels-to-show-or-nil)
  "Move forward to the next heading at the same level, then expand it.
Stops at parent boundaries."
  (interactive "P")
  (-nomis/tree/command
      nil
    (nomis/tree/impl/step-forward-sibling n-levels-to-show-or-nil)))

(defun nomis/tree/step-backward-peer (n-levels-to-show-or-nil)
  "Move backward to the previous heading at the same level, then expand it.
Can cross parent boundaries."
  (interactive "P")
  (-nomis/tree/command
      nil
    (nomis/tree/impl/step-backward-peer n-levels-to-show-or-nil)))

(defun nomis/tree/step-forward-peer (n-levels-to-show-or-nil)
  "Move forward to the next heading at the same level, then expand it.
Can cross parent boundaries."
  (interactive "P")
  (-nomis/tree/command
      nil
    (nomis/tree/impl/step-forward-peer n-levels-to-show-or-nil)))

;;; End

(provide 'nomis-tree)
