;;; nomis-tree-org.el --- A layer on top of Org mode  -*- lexical-binding: t; -*-

;;; Code:

;;;; `nomis/tree` methods

;;;;; Search heading text

(cl-defmethod nomis/tree/search-heading-text--aux ((k (eql :org)))
  (nomis/org-search-heading-text))

(cl-defmethod nomis/tree/search-heading-text-again--aux ((k (eql :org))) ()
  (nomis/org-search-heading-text-again))

;;;;; Visibility span

(cl-defmethod nomis/tree/visibility-span/less--aux ((k (eql :org)))
  (nomis/org-visibility-span/less))

(cl-defmethod nomis/tree/visibility-span/more--aux ((k (eql :org)))
  (nomis/org-visibility-span/more))

(cl-defmethod nomis/tree/visibility-span/set-min--aux ((k (eql :org)))
  (nomis/org-visibility-span/set-min))

(cl-defmethod nomis/tree/visibility-span/set-max--aux ((k (eql :org)))
  (nomis/org-visibility-span/set-max))

;;;;; nomis/tree/show-tree-only and nomis/tree/max-lineage

(cl-defmethod nomis/tree/show-tree-only--aux ((k (eql :org)))
  (norg/show-tree-only))

(cl-defmethod nomis/tree/max-lineage--aux ((k (eql :org)))
  (nomis/tree/unimplemented-method k))

;;;;; nomis/tree/set-step-n-levels-to-show

(cl-defmethod nomis/tree/set-step-n-levels-to-show--aux ((k (eql :org))
                                                         n)
  (norg/set-step-n-levels-to-show n))

;;;;; Expand/collapse from point

(cl-defmethod nomis/tree/show-children-from-point/incremental/less--aux
  ((k (eql :org)) arg)
  (norg/show-children-from-point/incremental/less arg))

(cl-defmethod nomis/tree/show-children-from-point/incremental/more--aux
  ((k (eql :org)) arg)
  (norg/show-children-from-point/incremental/more arg))

(cl-defmethod nomis/tree/show-children-from-point/set-min--aux
  ((k (eql :org)))
  (norg/show-children-from-point/set-min))

(cl-defmethod nomis/tree/show-children-from-point/fully-expand--aux
  ((k (eql :org)))
  (norg/show-children-from-point/fully-expand))

;;;;; Expand/collapse from parent

(cl-defmethod nomis/tree/show-children-from-parent/incremental/less--aux
  ((k (eql :org)) arg)
  (norg/show-children-from-parent/incremental/less arg))

(cl-defmethod nomis/tree/show-children-from-parent/incremental/more--aux
  ((k (eql :org)) arg)
  (norg/show-children-from-parent/incremental/more arg))

(cl-defmethod nomis/tree/show-children-from-parent/set-min--aux
  ((k (eql :org)))
  (norg/show-children-from-parent/set-min))

(cl-defmethod nomis/tree/show-children-from-parent/fully-expand--aux
  ((k (eql :org)))
  (norg/show-children-from-parent/fully-expand))

;;;;; Expand/collapse from root -- to current level, and from all roots -- to current level

(cl-defmethod nomis/tree/show-children-from-root/to-current-level--aux
  ((k (eql :org)))
  (norg/show-children-from-root/to-current-level))

(cl-defmethod nomis/tree/show-children-from-all-roots/to-current-level--aux
  ((k (eql :org)))
  (norg/show-children-from-all-roots/to-current-level))

;;;;; Expand/collapse from all roots

(cl-defmethod nomis/tree/show-children-from-all-roots/incremental/less--aux
  ((k (eql :org)) arg)
  (norg/show-children-from-all-roots/incremental/less arg))

(cl-defmethod nomis/tree/show-children-from-all-roots/incremental/more--aux
  ((k (eql :org)) arg)
  (norg/show-children-from-all-roots/incremental/more arg))

(cl-defmethod nomis/tree/show-children-from-all-roots/set-min--aux
  ((k (eql :org)))
  (norg/show-children-from-all-roots/set-min))

(cl-defmethod nomis/tree/show-children-from-all-roots/fully-expand--aux
  ((k (eql :org)))
  (norg/show-children-from-all-roots/fully-expand))

;;;;; Expand/collapse from root

(cl-defmethod nomis/tree/show-children-from-root/incremental/less--aux
  ((k (eql :org)) arg)
  (norg/show-children-from-root/incremental/less arg))

(cl-defmethod nomis/tree/show-children-from-root/incremental/more--aux
  ((k (eql :org)) arg)
  (norg/show-children-from-root/incremental/more arg))

(cl-defmethod nomis/tree/show-children-from-root/set-min--aux
  ((k (eql :org)))
  (norg/show-children-from-root/set-min))

(cl-defmethod nomis/tree/show-children-from-root/fully-expand--aux
  ((k (eql :org)))
  (norg/show-children-from-root/fully-expand))

;;;;; Tab and shifttab

(cl-defmethod nomis/tree/tab--aux ((k (eql :org)) arg)
  (norg/tab arg))

(cl-defmethod nomis/tree/shifttab--aux ((k (eql :org)) arg)
  (norg/shifttab arg))

;;;;; Movement

(cl-defmethod nomis/tree/previous-sibling--aux ((k (eql :org)))
  (norg/previous-sibling))

(cl-defmethod nomis/tree/next-sibling--aux ((k (eql :org)))
  (norg/next-sibling))

(cl-defmethod nomis/tree/previous-sibling/allow-cross-parent--aux
  ((k (eql :org)))
  (norg/previous-sibling/allow-cross-parent))

(cl-defmethod nomis/tree/next-sibling/allow-cross-parent--aux ((k (eql :org)))
  (norg/next-sibling/allow-cross-parent))

;;;;; Movement + expand/collapse

(cl-defmethod nomis/tree/step-backward--aux ((k (eql :org)) n-levels-to-show-or-nil)
  (norg/step-backward n-levels-to-show-or-nil))

(cl-defmethod nomis/tree/step-forward--aux ((k (eql :org)) n-levels-to-show-or-nil)
  (norg/step-forward n-levels-to-show-or-nil))

(cl-defmethod nomis/tree/step-backward/allow-cross-parent--aux ((k (eql :org)) n-levels-to-show-or-nil)
  (norg/step-backward/allow-cross-parent n-levels-to-show-or-nil))

(cl-defmethod nomis/tree/step-forward/allow-cross-parent--aux ((k (eql :org)) n-levels-to-show-or-nil)
  (norg/step-forward/allow-cross-parent n-levels-to-show-or-nil))

(cl-defmethod nomis/tree/previous-heading--aux ((k (eql :org)) n)
  ;; TODO: We are ignoring `n`.
  (norg/previous-heading))

(cl-defmethod nomis/tree/next-heading--aux ((k (eql :org)) n)
  ;; TODO: We are ignoring `n`.
  (norg/next-heading))

(cl-defmethod nomis/tree/previous-heading/set-tree+body--aux ((k (eql :org)))
  (norg/previous-heading/set-tree+body))

(cl-defmethod nomis/tree/next-heading/set-tree+body--aux ((k (eql :org)))
  (norg/next-heading/set-tree+body))

;;; End

(provide 'nomis-tree-org)
