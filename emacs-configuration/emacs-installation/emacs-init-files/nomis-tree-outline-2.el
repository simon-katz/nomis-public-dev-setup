;;; nomis-tree-outline-2.el ---  -*- lexical-binding: t; -*-

;; TODO: The names in this file should be `nomis/tree-outline-2/xxxx`.

;; TODO: Maybe move all this to `nomis-tree-outline`. And similarly maybe move
;;       contents of `norg` to `nomis-tree-org`.

;;; Code:

;;;; Requires

(require 'a)
(require 'cl-lib)
(require 'dash)
(require 'nomis-msg)
(require 'nomis-outline-common)
(require 'nomis-popup)
(require 'nomis-tree-lineage-specs)
(require 'outline)

;;;; Utilities

;;;;; Previous/next helpers

(defun -nomis/tree/outline/prev-or-next-heading-and-show-lineage (lineage-spec
                                                                  n
                                                                  direction
                                                                  kind)
  (when (nomis/outline/c/prev-or-next-heading n direction kind)
    (nomis/tree/ls/show-lineage lineage-spec)))

;;;; API

;;;;; nomis/tree/outline/show-all

;; TODO: Temporary, until we have
;;       `nomis/tree/show-children-from-all-roots/fully-expand`.
(defun nomis/tree/outline/show-all ()
  (interactive)
  (outline-show-all)
  (nomis/msg/pulse-buffer))

;;;;; nomis/tree/outline/show-children-from-point/xxxx

(defvar -nomis/tree/outline/increments-children-approach)

(defun -nomis/tree/outline/increments-children-approach/get ()
  ;; TODO: At some point change this to look at the actual text rather than
  ;;       relying on `-nomis/tree/outline/increments-children-approach`.
  (when (member (nomis/outline/c/last-command)
                '(nomis/tree/show-children-from-point/incremental/more
                  nomis/tree/show-children-from-point/incremental/less))
    -nomis/tree/outline/increments-children-approach))

(defun -nomis/tree/outline/inc-dec-message (approach clamped?)
  (nomis/popup/message (concat (cl-ecase approach
                                 (0 "Folded")
                                 (1 "Body")
                                 (2 "Children")
                                 (3 "Branches")
                                 (4 "Subtree"))
                               (when clamped? " (clamped)"))))

(defun -nomis/tree/outline/increments-children-approach/set (approach clamped?)
  (setq -nomis/tree/outline/increments-children-approach approach)
  (nomis/tree/ls/show-lineage (nomis/tree/ls/spec/show-children approach))
  (-nomis/tree/outline/inc-dec-message approach clamped?))

(defun -nomis/tree/outline/set-n-children-from-point (n)
  (let* ((new-approach (max 0
                            (min n
                                 nomis/tree/ls/children-approach-max)))
         (clamped? (/= n new-approach)))
    (-nomis/tree/outline/increments-children-approach/set new-approach
                                                          clamped?)))

(defun nomis/tree/outline/show-children-from-point/incremental/less (n)
  (if n
      (-nomis/tree/outline/set-n-children-from-point n)
    (let* ((current-approach (-nomis/tree/outline/increments-children-approach/get)))
      (if (eql current-approach 0)
          (nomis/popup/error-message "Already fully collapsed")
        (let* ((new-approach (if current-approach
                                 (1- current-approach)
                               nomis/tree/ls/children-approach-max)))
          (-nomis/tree/outline/increments-children-approach/set new-approach
                                                                nil))))))

(defun nomis/tree/outline/show-children-from-point/incremental/more (n)
  (if n
      (-nomis/tree/outline/set-n-children-from-point n)
    (let* ((current-approach (-nomis/tree/outline/increments-children-approach/get)))
      (if (eql current-approach nomis/tree/ls/children-approach-max)
          (nomis/popup/error-message "Already fully expanded")
        (let* ((new-approach (if current-approach
                                 (1+ current-approach)
                               0)))
          (-nomis/tree/outline/increments-children-approach/set new-approach
                                                                nil))))))

;;;;; nomis/tree/outline/lineage

(defun nomis/tree/outline/lineage/set-max ()
  (nomis/tree/ls/show-lineage nomis/tree/ls/spec/fat-parents-one-child))

;;;;; nomis/tree/outline/show-max-lineage

(defun nomis/tree/outline/show-max-lineage ()
  (nomis/tree/ls/show-lineage nomis/tree/ls/spec/fat-parents-all-children))

;;;;; nomis/tree/outline/show-tree-only

(defun nomis/tree/outline/show-tree-only ()
  (nomis/tree/ls/show-lineage nomis/tree/ls/spec/fat-parents))

;;;;; Previous

(defun nomis/tree/outline/previous-heading (n)
  (-nomis/tree/outline/prev-or-next-heading-and-show-lineage nomis/tree/ls/spec/navigation
                                                             n
                                                             :backward
                                                             :any-level))

(defun nomis/tree/outline/previous-sibling (n)
  "Move backward to the N'th heading at same level as this one.
Stop at the first and last headings of a superior heading."
  (interactive "p")
  (-nomis/tree/outline/prev-or-next-heading-and-show-lineage nomis/tree/ls/spec/navigation
                                                             n
                                                             :backward
                                                             :sibling))

(defun nomis/tree/outline/previous-peer (n)
  "Move backward to the N'th heading at same level as this one.
Can pass by a superior heading."
  (interactive "p")
  (-nomis/tree/outline/prev-or-next-heading-and-show-lineage nomis/tree/ls/spec/navigation
                                                             n
                                                             :backward
                                                             :peer))

(defun nomis/tree/outline/step-backward-sibling (n)
  "Move backward to the N'th heading at same level as this one, then show
fat parents and all children.
Stop at the first and last headings of a superior heading."
  (-nomis/tree/outline/prev-or-next-heading-and-show-lineage nomis/tree/ls/spec/step
                                                             (or n 1)
                                                             :backward
                                                             :sibling))

(defun nomis/tree/outline/step-backward-peer (n)
  "Move backward to the N'th heading at same level as this one, then show
fat parents and all children.
Can pass by a superior heading."
  (-nomis/tree/outline/prev-or-next-heading-and-show-lineage nomis/tree/ls/spec/step
                                                             (or n 1)
                                                             :backward
                                                             :peer))

;;;;; Next

(defun nomis/tree/outline/next-heading (n)
  (-nomis/tree/outline/prev-or-next-heading-and-show-lineage nomis/tree/ls/spec/navigation
                                                             n
                                                             :forward
                                                             :any-level))

(defun nomis/tree/outline/next-sibling (n)
  "Move forward to the N'th heading at same level as this one.
Stop at the first and last headings of a superior heading."
  (interactive "p")
  (-nomis/tree/outline/prev-or-next-heading-and-show-lineage nomis/tree/ls/spec/navigation
                                                             n
                                                             :forward
                                                             :sibling))

(defun nomis/tree/outline/next-peer (n)
  "Move forward to the N'th heading at same level as this one.
Can pass by a superior heading."
  (interactive "p")
  (-nomis/tree/outline/prev-or-next-heading-and-show-lineage nomis/tree/ls/spec/navigation
                                                             n
                                                             :forward
                                                             :peer))

(defun nomis/tree/outline/step-forward-sibling (n)
  "Move forward to the N'th heading at same level as this one, then show
fat parents and all children.
Stop at the first and last headings of a superior heading."
  (-nomis/tree/outline/prev-or-next-heading-and-show-lineage nomis/tree/ls/spec/step
                                                             (or n 1)
                                                             :forward
                                                             :sibling))

(defun nomis/tree/outline/step-forward-peer (n)
  "Move forward to the N'th heading at same level as this one, then show
fat parents and all children.
Can pass by a superior heading."
  (-nomis/tree/outline/prev-or-next-heading-and-show-lineage nomis/tree/ls/spec/step
                                                             (or n 1)
                                                             :forward
                                                             :peer))

;;; End

(provide 'nomis-tree-outline-2)
