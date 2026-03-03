;;; nomis-tree.el --- A protocol for navigating trees in files  -*- lexical-binding: t; -*-


;;; To dos

;; TODO: There's lots of potential work here and it's not straightforward.
;;       So for now do nothing apart from (a) mull things over, and (b) look at
;;       existing code.

;;;; Bicycle

;; TODO: Have we decided that we don't want `bicycle`? (I think we have.)
;;       - See `nomis-bicycle`.

;;;; Fill out the implementation for `:outline`

;; TODO: Replace uses of `nomis/tree/unimplemented-method` with implementations.

;; TODO: Compare `noutline` commands with `norg` commands.
;;       - Indefinite number of levels.
;;       - Behaviour when reaching last sibling
;;       - Implement more commands here.
;;       - Make a shared low-level API around org and outline, and
;;         make higher-level functions use that.
;;       - Hmmmm, it might be non-trivial. I see that `norg` uses a mix of
;;         `outline` and `org` functionality as its base layer. The `org`
;;         stuff won't be available for outlines.

;; TODO: Extract common functionality from `:outline` and `:org` methods, where
;;       possible. (Especially when replacing some of the current uses of
;;       `nomis/tree/unimplemented-method`.

;;;; Hide-show

;; TODO: We could integrate hide-show -- so eg
;;       `nomis/tree/show-children-from-point/incremental/more` `:outline`
;;       method could do `nomis/hs/adjust/more` for children that are code.

;; TODO: The key bindings for hide-show will need to be made different (because
;;       they duplicate our `norg` key bindings, which we want for outline
;;       stuff). Maybe a prefix in front of the existing bindings. Oh, or maybe
;;       a modal UI.
;;       - Done (different key bindings.)

;;; Code:

;;;; Requires

(require 'nomis-scrolling)
(require 'outline)

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

;;;;; -nomis/tree/command

(defun -nomis/tree/command* (f)
  (cl-flet* ((do-it () (nomis/scrolling/with-maybe-maintain-line-no-in-window
                         (funcall f))))
    (if mark-active
        (do-it)
      (let* ((start-point (point)))
        (prog1
            (do-it)
          (let* ((end-point (point)))
            (unless (= start-point end-point)
              (push-mark (point)
                         ;; Use `nomsg` arg in case `f` has displayed
                         ;; a message already.
                         t))))))))

(cl-defmacro -nomis/tree/command (_opts &body body)
  (declare (indent 1))
  `(-nomis/tree/command* (lambda () ,@body) ))

;;;;; nomis/tree/unimplemented-method

(defun nomis/tree/unimplemented-method (k)
  (error "Not supported: %s %s" k this-command))

;;;;; -nomis/tree/mode

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

;;;; API

;;;;; Search heading text

(cl-defgeneric nomis/tree/search-heading-text--aux (k))
(cl-defgeneric nomis/tree/search-heading-text-again--aux (k))

(defun nomis/tree/search-heading-text ()
  (interactive)
  (-nomis/tree/command
      nil
    (nomis/tree/search-heading-text--aux (-nomis/tree/mode))))

(defun nomis/tree/search-heading-text-again ()
  (interactive)
  (-nomis/tree/command
      nil
    (nomis/tree/search-heading-text-again--aux (-nomis/tree/mode))))

;;;;; Visibility span

(cl-defgeneric nomis/tree/visibility-span/less--aux (k))
(cl-defgeneric nomis/tree/visibility-span/more--aux (k))
(cl-defgeneric nomis/tree/visibility-span/set-min--aux (k))
(cl-defgeneric nomis/tree/visibility-span/set-max--aux (k))

(defun nomis/tree/visibility-span/less ()
  (interactive)
  (-nomis/tree/command
      nil
    (nomis/tree/visibility-span/less--aux (-nomis/tree/mode))))

(defun nomis/tree/visibility-span/more ()
  (interactive)
  (-nomis/tree/command
      nil
    (nomis/tree/visibility-span/more--aux (-nomis/tree/mode))))

(defun nomis/tree/visibility-span/set-min ()
  (interactive)
  (-nomis/tree/command
      nil
    (nomis/tree/visibility-span/set-min--aux (-nomis/tree/mode))))

(defun nomis/tree/visibility-span/set-max ()
  (interactive)
  (-nomis/tree/command
      nil
    (nomis/tree/visibility-span/set-max--aux (-nomis/tree/mode))))

;;;;; nomis/tree/show-tree-only and nomis/tree/max-lineage

(cl-defgeneric nomis/tree/show-tree-only--aux (k))
(cl-defgeneric nomis/tree/max-lineage--aux (k))

(defun nomis/tree/show-tree-only ()
  (interactive)
  (-nomis/tree/command
      nil
    (nomis/tree/show-tree-only--aux (-nomis/tree/mode))))

(defun nomis/tree/max-lineage ()
  (interactive)
  (-nomis/tree/command
      nil
    (nomis/tree/max-lineage--aux (-nomis/tree/mode))))

;;;;; nomis/tree/set-step-n-levels-to-show

(cl-defgeneric nomis/tree/set-step-n-levels-to-show--aux (k n))

(defun nomis/tree/set-step-n-levels-to-show (n)
  (interactive "P")
  (-nomis/tree/command
      nil
    (nomis/tree/set-step-n-levels-to-show--aux (-nomis/tree/mode) n)))

;;;;; Expand/collapse from point

(cl-defgeneric nomis/tree/show-children-from-point/incremental/less--aux (k n))
(cl-defgeneric nomis/tree/show-children-from-point/incremental/more--aux (k n))
(cl-defgeneric nomis/tree/show-children-from-point/set-min--aux (k))
(cl-defgeneric nomis/tree/show-children-from-point/fully-expand--aux (k))

(defun nomis/tree/show-children-from-point/incremental/less (n)
  "Incrementally collapse the current heading by 1 level.
With a numeric prefix `N`, set the number of visible levels to exactly `N`.
When in a body, \"current heading\" means the current body's parent heading."
  (interactive "P")
  (-nomis/tree/command
      nil
    (nomis/tree/show-children-from-point/incremental/less--aux (-nomis/tree/mode)
                                                               n)))

(defun nomis/tree/show-children-from-point/incremental/more (n)
  "Incrementally expand the current heading by 1 level.
With a numeric prefix `N`, set the number of visible levels to exactly `N`.
When in a body, \"current heading\" means the current body's parent heading."
  (interactive "P")
  (-nomis/tree/command
      nil
    (nomis/tree/show-children-from-point/incremental/more--aux (-nomis/tree/mode)
                                                               n)))

(defun nomis/tree/show-children-from-point/set-min ()
  "Fully collapse the current heading.
When in a body, \"current heading\" means the current body's parent heading."
  (interactive)
  (-nomis/tree/command
      nil
    (nomis/tree/show-children-from-point/set-min--aux (-nomis/tree/mode))))

(defun nomis/tree/show-children-from-point/fully-expand ()
  "Fully expand the current heading.
When in a body, \"current heading\" means the current body's parent heading."
  (interactive)
  (-nomis/tree/command
      nil
    (nomis/tree/show-children-from-point/fully-expand--aux (-nomis/tree/mode))))

;;;;; Expand/collapse from parent

(cl-defgeneric nomis/tree/show-children-from-parent/incremental/less--aux (k n))
(cl-defgeneric nomis/tree/show-children-from-parent/incremental/more--aux (k n))
(cl-defgeneric nomis/tree/show-children-from-parent/set-min--aux (k))
(cl-defgeneric nomis/tree/show-children-from-parent/fully-expand--aux (k))

(defun nomis/tree/show-children-from-parent/incremental/less (n)
  "Like `nomis/tree/show-children-from-point/incremental/less`, but from
the current entry's parent and with the parent always expanded at least
one level."
  (interactive "P")
  (-nomis/tree/command
      nil
    (nomis/tree/show-children-from-parent/incremental/less--aux (-nomis/tree/mode) n)))

(defun nomis/tree/show-children-from-parent/incremental/more (n)
  "Like `nomis/tree/show-children-from-point/incremental/more`, but from
the current entry's parent."
  (interactive "P")
  (-nomis/tree/command
      nil
    (nomis/tree/show-children-from-parent/incremental/more--aux (-nomis/tree/mode) n)))

(defun nomis/tree/show-children-from-parent/set-min ()
  "Like `nomis/tree/show-children-from-point/set-min`, but from the
current entry's parent and showing one level."
  (interactive)
  (-nomis/tree/command
      nil
    (nomis/tree/show-children-from-parent/set-min--aux (-nomis/tree/mode))))

(defun nomis/tree/show-children-from-parent/fully-expand ()
  "Like `nomis/tree/show-children-from-point/fully-expand`, but from
the current entry's parent."
  (interactive)
  (-nomis/tree/command
      nil
    (nomis/tree/show-children-from-parent/fully-expand--aux (-nomis/tree/mode))))

;;;;; Expand/collapse from root -- to current level, and from all roots -- to current level

(cl-defgeneric nomis/tree/show-children-from-root/to-current-level--aux (k))
(cl-defgeneric nomis/tree/show-children-from-all-roots/to-current-level--aux (k))

(defun nomis/tree/show-children-from-root/to-current-level ()
  (interactive)
  (-nomis/tree/command
      nil
    (nomis/tree/show-children-from-root/to-current-level--aux (-nomis/tree/mode))))

(defun nomis/tree/show-children-from-all-roots/to-current-level ()
  (interactive)
  (-nomis/tree/command
      nil
    (nomis/tree/show-children-from-all-roots/to-current-level--aux (-nomis/tree/mode))))

;;;;; Expand/collapse from all roots

(cl-defgeneric nomis/tree/show-children-from-all-roots/incremental/less--aux (k n))
(cl-defgeneric nomis/tree/show-children-from-all-roots/incremental/more--aux (k n))
(cl-defgeneric nomis/tree/show-children-from-all-roots/set-min--aux (k))
(cl-defgeneric nomis/tree/show-children-from-all-roots/fully-expand--aux (k))

(defun nomis/tree/show-children-from-all-roots/incremental/less (n)
  "Incrementally collapse all roots by 1 level.
With a numeric prefix `N`, set the number of visible levels to exactly `N`."
  (interactive "P")
  (-nomis/tree/command
      nil
    (nomis/tree/show-children-from-all-roots/incremental/less--aux (-nomis/tree/mode) n)))

(defun nomis/tree/show-children-from-all-roots/incremental/more (n)
  "Incrementally expand all roots by 1 level.
With a numeric prefix `N`, set the number of visible levels to exactly `N`."
  (interactive "P")
  (-nomis/tree/command
      nil
    (nomis/tree/show-children-from-all-roots/incremental/more--aux (-nomis/tree/mode) n)))

(defun nomis/tree/show-children-from-all-roots/set-min ()
  (interactive)
  (-nomis/tree/command
      nil
    (nomis/tree/show-children-from-all-roots/set-min--aux (-nomis/tree/mode))))

(defun nomis/tree/show-children-from-all-roots/fully-expand ()
  (interactive)
  (-nomis/tree/command
      nil
    (nomis/tree/show-children-from-all-roots/fully-expand--aux (-nomis/tree/mode))))

;;;;; Expand/collapse from root

(cl-defgeneric nomis/tree/show-children-from-root/incremental/less--aux (k n))
(cl-defgeneric nomis/tree/show-children-from-root/incremental/more--aux (k n))
(cl-defgeneric nomis/tree/show-children-from-root/set-min--aux (k))
(cl-defgeneric nomis/tree/show-children-from-root/fully-expand--aux (k))

(defun nomis/tree/show-children-from-root/incremental/less (n)
  "Incrementally collapse the current root by 1 level.
With a numeric prefix `N`, set the number of visible levels to exactly `N`."
  (interactive "P")
  (-nomis/tree/command
      nil
    (nomis/tree/show-children-from-root/incremental/less--aux (-nomis/tree/mode) n)))

(defun nomis/tree/show-children-from-root/incremental/more (n)
  "Incrementally expand the current root by 1 level.
With a numeric prefix `N`, set the number of visible levels to exactly `N`."
  (interactive "P")
  (-nomis/tree/command
      nil
    (nomis/tree/show-children-from-root/incremental/more--aux (-nomis/tree/mode) n)))

(defun nomis/tree/show-children-from-root/set-min ()
  (interactive)
  (-nomis/tree/command
      nil
    (nomis/tree/show-children-from-root/set-min--aux (-nomis/tree/mode))))

(defun nomis/tree/show-children-from-root/fully-expand ()
  (interactive)
  (-nomis/tree/command
      nil
    (nomis/tree/show-children-from-root/fully-expand--aux (-nomis/tree/mode))))

;;;;; Movement

(cl-defgeneric nomis/tree/previous-sibling--aux (k))
(cl-defgeneric nomis/tree/next-sibling--aux (k))
(cl-defgeneric nomis/tree/previous-peer--aux (k))
(cl-defgeneric nomis/tree/next-peer--aux (k))

(defun nomis/tree/previous-sibling ()
  "Move backward one heading at the same level as this one."
  (interactive)
  (-nomis/tree/command
      nil
    (nomis/tree/previous-sibling--aux (-nomis/tree/mode))))

(defun nomis/tree/next-sibling ()
  "Move forward one heading at the same level as this one."
  (interactive)
  (-nomis/tree/command
      nil
    (nomis/tree/next-sibling--aux (-nomis/tree/mode))))

(defun nomis/tree/previous-peer ()
  "Move backward one heading at the same level, crossing parent boundaries."
  (interactive)
  (-nomis/tree/command
      nil
    (nomis/tree/previous-peer--aux (-nomis/tree/mode))))

(defun nomis/tree/next-peer ()
  "Move forward one heading at the same level, crossing parent boundaries."
  (interactive)
  (-nomis/tree/command
      nil
    (nomis/tree/next-peer--aux (-nomis/tree/mode))))

;;;;; Movement + expand/collapse

;; TODO: The prefix arg here has different meanings for `:outline` and `:org`.
;;
;;       For the former it's the number of headings to move, and we convert
;;       `nil` to 1. See uses of `(or n 1)` in `noutline`.
;;
;;       For the latter it's `n-levels-to-show-or-nil`, and we convert `nil` to
;;       `norg/step-n-levels-to-show`.

(cl-defgeneric nomis/tree/step-backward-sibling--aux (k n))
(cl-defgeneric nomis/tree/step-forward-sibling--aux (k n))
(cl-defgeneric nomis/tree/step-backward-peer--aux (k n))
(cl-defgeneric nomis/tree/step-forward-peer--aux (k n))
(cl-defgeneric nomis/tree/previous-heading--aux (k n))
(cl-defgeneric nomis/tree/next-heading--aux (k n))
(cl-defgeneric nomis/tree/previous-heading/set-tree+body--aux (k))
(cl-defgeneric nomis/tree/next-heading/set-tree+body--aux (k))

(defun nomis/tree/step-backward-sibling (n)
  (interactive "P")
  (-nomis/tree/command
      nil
    (nomis/tree/step-backward-sibling--aux (-nomis/tree/mode) n)))

(defun nomis/tree/step-forward-sibling (n)
  (interactive "P")
  (-nomis/tree/command
      nil
    (nomis/tree/step-forward-sibling--aux (-nomis/tree/mode) n)))

(defun nomis/tree/step-backward-peer (n)
  (interactive "P")
  (-nomis/tree/command
      nil
    (nomis/tree/step-backward-peer--aux (-nomis/tree/mode) n)))

(defun nomis/tree/step-forward-peer (n)
  (interactive "P")
  (-nomis/tree/command
      nil
    (nomis/tree/step-forward-peer--aux (-nomis/tree/mode) n)))

(defun nomis/tree/previous-heading (n)
  (interactive "p")
  (-nomis/tree/command
      nil
    (nomis/tree/previous-heading--aux (-nomis/tree/mode) n)))

(defun nomis/tree/next-heading (n)
  (interactive "p")
  (-nomis/tree/command
      nil
    (nomis/tree/next-heading--aux (-nomis/tree/mode) n)))

(defun nomis/tree/previous-heading/set-tree+body ()
  (interactive)
  (-nomis/tree/command
      nil
    (nomis/tree/previous-heading/set-tree+body--aux (-nomis/tree/mode))))

(defun nomis/tree/next-heading/set-tree+body ()
  (interactive)
  (-nomis/tree/command
      nil
    (nomis/tree/next-heading/set-tree+body--aux (-nomis/tree/mode))))

;;; End

(provide 'nomis-tree)
