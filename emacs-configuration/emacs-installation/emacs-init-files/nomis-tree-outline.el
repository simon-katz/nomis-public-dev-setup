;;; nomis-tree-outline.el ---  -*- lexical-binding: t; -*-

;;; Code:

;;;; Requires

(require 'cl-lib)
(require 'nomis-outline-uber)
(require 'nomis-tree)

;;;; `nomis/tree` methods

;;;;; Search heading text

(cl-defmethod nomis/tree/search-heading-text--aux ((k (eql :outline)))
  (nomis/tree/unimplemented-method k))

(cl-defmethod nomis/tree/search-heading-text-again--aux ((k (eql :outline)))
  (nomis/tree/unimplemented-method k))

;;;;; Visibility span

(cl-defmethod nomis/tree/visibility-span/less--aux ((k (eql :outline)))
  (nomis/tree/unimplemented-method k))

(cl-defmethod nomis/tree/visibility-span/more--aux ((k (eql :outline)))
  (nomis/tree/unimplemented-method k))

(cl-defmethod nomis/tree/visibility-span/set-min--aux ((k (eql :outline)))
  (nomis/tree/unimplemented-method k))

(cl-defmethod nomis/tree/visibility-span/set-max--aux ((_k (eql :outline)))
  (nomis/outline/visibility-span/set-max))

;;;;; nomis/tree/show-tree-only and nomis/tree/max-lineage

(cl-defmethod nomis/tree/show-tree-only--aux ((_k (eql :outline)))
  (nomis/outline/show-tree-only))

(cl-defmethod nomis/tree/max-lineage--aux ((_k (eql :outline)))
  (nomis/outline/show-max-lineage))

;;;;; nomis/tree/set-step-n-levels-to-show

(cl-defmethod nomis/tree/set-step-n-levels-to-show--aux ((k (eql :outline)) _n)
  (nomis/tree/unimplemented-method k))

;;;;; Expand/collapse from point

(cl-defmethod nomis/tree/show-children-from-point/incremental/less--aux
  ((k (eql :outline)) _n)
  (nomis/outline/dec-children))

(cl-defmethod nomis/tree/show-children-from-point/incremental/more--aux
  ((k (eql :outline)) _n)
  (nomis/outline/inc-children))

(cl-defmethod nomis/tree/show-children-from-point/set-min--aux
  ((k (eql :outline)))
  (nomis/tree/unimplemented-method k))

(cl-defmethod nomis/tree/show-children-from-point/fully-expand--aux
  ((k (eql :outline)))
  (nomis/tree/unimplemented-method k))

;;;;; Expand/collapse from parent

(cl-defmethod nomis/tree/show-children-from-parent/incremental/less--aux
  ((k (eql :outline)) _n)
  (nomis/tree/unimplemented-method k))

(cl-defmethod nomis/tree/show-children-from-parent/incremental/more--aux
  ((k (eql :outline)) _n)
  (nomis/tree/unimplemented-method k))

(cl-defmethod nomis/tree/show-children-from-parent/set-min--aux
  ((k (eql :outline)))
  (nomis/tree/unimplemented-method k))

(cl-defmethod nomis/tree/show-children-from-parent/fully-expand--aux
  ((k (eql :outline)))
  (nomis/tree/unimplemented-method k))

;;;;; Expand/collapse from root -- to current level, and from all roots -- to current level

(cl-defmethod nomis/tree/show-children-from-root/to-current-level--aux
  ((k (eql :outline)))
  (nomis/tree/unimplemented-method k))

(cl-defmethod nomis/tree/show-children-from-all-roots/to-current-level--aux
  ((k (eql :outline)))
  (nomis/tree/unimplemented-method k))

;;;;; Expand/collapse from all roots

(cl-defmethod nomis/tree/show-children-from-all-roots/incremental/less--aux
  ((k (eql :outline)) _n)
  (nomis/tree/unimplemented-method k))

(cl-defmethod nomis/tree/show-children-from-all-roots/incremental/more--aux
  ((k (eql :outline)) _n)
  (nomis/tree/unimplemented-method k))

(cl-defmethod nomis/tree/show-children-from-all-roots/set-min--aux
  ((k (eql :outline)))
  (nomis/tree/unimplemented-method k))

(cl-defmethod nomis/tree/show-children-from-all-roots/fully-expand--aux
  ((k (eql :outline)))
  (nomis/tree/unimplemented-method k))

;;;;; Expand/collapse from root

(cl-defmethod nomis/tree/show-children-from-root/incremental/less--aux
  ((k (eql :outline)) _n)
  (nomis/tree/unimplemented-method k))

(cl-defmethod nomis/tree/show-children-from-root/incremental/more--aux
  ((k (eql :outline)) _n)
  (nomis/tree/unimplemented-method k))

(cl-defmethod nomis/tree/show-children-from-root/set-min--aux
  ((k (eql :outline)))
  (nomis/tree/unimplemented-method k))

(cl-defmethod nomis/tree/show-children-from-root/fully-expand--aux
  ((k (eql :outline)))
  (nomis/tree/unimplemented-method k))

;;;;; Tab and shifttab

(cl-defmethod nomis/tree/tab--aux ((_k (eql :outline)) arg)
  (nomis/outline/tab arg))

(cl-defmethod nomis/tree/shifttab--aux ((_k (eql :outline)) _arg)
  (nomis/outline/dec-children))

;;;;; Movement

(cl-defmethod nomis/tree/previous-sibling--aux ((_k (eql :outline)))
  (nomis/outline/previous-sibling 1))

(cl-defmethod nomis/tree/next-sibling--aux ((_k (eql :outline)))
  (nomis/outline/next-sibling 1))

(cl-defmethod nomis/tree/previous-peer--aux ((_k (eql :outline)))
  (nomis/outline/previous-peer 1))

(cl-defmethod nomis/tree/next-peer--aux ((_k (eql :outline)))
  (nomis/outline/next-peer 1))

;;;;; Movement + expand/collapse

(cl-defmethod nomis/tree/step-backward-sibling--aux ((_k (eql :outline)) n)
  (nomis/outline/step-backward-sibling n))

(cl-defmethod nomis/tree/step-forward-sibling--aux ((_k (eql :outline)) n)
  (nomis/outline/step-forward-sibling n))

(cl-defmethod nomis/tree/step-backward-peer--aux ((_k (eql :outline)) n)
  (nomis/outline/step-backward-peer n))

(cl-defmethod nomis/tree/step-forward-peer--aux ((_k (eql :outline)) n)
  (nomis/outline/step-forward-peer n))

(cl-defmethod nomis/tree/previous-heading--aux ((_k (eql :outline)) n)
  (nomis/outline/previous-heading n))

(cl-defmethod nomis/tree/next-heading--aux ((_k (eql :outline)) n)
  (nomis/outline/next-heading n))

(cl-defmethod nomis/tree/previous-heading/set-tree+body--aux ((k (eql :outline)))
  (nomis/tree/unimplemented-method k))

(cl-defmethod nomis/tree/next-heading/set-tree+body--aux ((k (eql :outline)))
  (nomis/tree/unimplemented-method k))

;;; End

(provide 'nomis-tree-outline)
