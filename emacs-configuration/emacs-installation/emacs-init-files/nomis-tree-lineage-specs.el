;;; nomis-tree-lineage-specs.el --- Lineage specs for tree display  -*- lexical-binding: t; -*-

;;; Code:

;;;; Requires

(require 'a)
(require 'nomis-outline-common)

;;;; Lineage spec

;; A lineage-spec controls how lineages are displayed and has the following
;; entries (with permitted values nested):
;;
;; - `:spec/pre-hide-all?`
;;   - boolean
;;
;; - `:spec/pre-hide-children?`
;;   - boolean
;;
;; - `:spec/parents-approach` (doesn't hide anything, but can show things)
;;   - `nil`
;;     - Do nothing.
;;   - `:parents/thin`
;;     - Show parents.
;;   - `:parents/fat`
;;     - Show parents, siblings of parents, and siblings.
;;
;; - `:spec/children-approach` (doesn't hide anything, but can show things)
;;   - `nil` or `0`
;;     - Do nothing.
;;   - `1` / `2` / `3` / `4`
;;     - Show body/children/branches/subtree.

;; TODO: None of this shows bodies. Should it?

;; TODO: See `org-show-context-detail` for ideas for more lineage specs.

(defconst nomis/tree/ls/children-approach-max 4)

(defconst nomis/tree/ls/spec/min-lineage
  (a-hash-table :spec/pre-hide-all? t))

(defconst nomis/tree/ls/spec/thin-parents-lineage
  (a-hash-table :spec/pre-hide-all? t
                :spec/parents-approach :parents/thin))

(defconst nomis/tree/ls/spec/fat-parents-lineage
  (a-hash-table :spec/pre-hide-all? t
                :spec/parents-approach :parents/fat))

(defconst nomis/tree/ls/spec/max-visibility-span-lineage
  (a-hash-table :spec/pre-hide-all? t
                :spec/parents-approach :parents/fat
                :spec/children-approach 1))

(defconst nomis/tree/ls/spec/fat-parents-immediate-children-lineage
  (a-hash-table :spec/pre-hide-all? t
                :spec/parents-approach :parents/fat
                :spec/children-approach 2))

(defconst nomis/tree/ls/spec/max-lineage
  (a-hash-table :spec/pre-hide-all? t
                :spec/parents-approach :parents/fat
                :spec/children-approach nomis/tree/ls/children-approach-max))

(defconst nomis/tree/ls/spec/step-lineage
  (a-hash-table :spec/pre-hide-all? t
                :spec/parents-approach :parents/fat
                :spec/children-approach nomis/tree/ls/children-approach-max))

(defconst nomis/tree/ls/spec/navigation-lineage
  (a-hash-table :spec/parents-approach :parents/thin))

(defun nomis/tree/ls/spec/show-children-lineage (children-approach)
  (a-hash-table :spec/pre-hide-children? t
                :spec/children-approach children-approach
                :spec/pulse-max-children? t))

;;;; Hide/show lineage

(defun -nomis/tree/ls/hsl-hide (lineage-spec)
  (let* ((pre-hide-all? (a-get lineage-spec :spec/pre-hide-all?))
         (parents-approach (a-get lineage-spec :spec/parents-approach)))
    (when pre-hide-all?
      (let* ((top-level-level (-nomis/outline/c/top-level-level))
             (hide-level (cl-ecase parents-approach
                           ((nil :parents/thin) (1- top-level-level))
                           (:parents/fat top-level-level))))
        (outline-hide-sublevels (max 1 ; avoid error when < 1
                                     hide-level))))))

(defun -nomis/tree/ls/hsl-show-parents (lineage-spec)
  (let* ((parents-approach (a-get lineage-spec :spec/parents-approach)))
    (when parents-approach
      (let* ((parent-points
              (let* ((ps '()))
                (save-excursion
                  (while (and (nomis/outline/c/on-heading?)
                              (not (-nomis/outline/c/on-top-level-heading?)))
                    (nomis/outline/c/up-heading 1)
                    (push (point) ps)))
                ps)))
        (save-excursion
          (cl-loop
           for p in parent-points
           do (progn
                (goto-char p)
                (nomis/outline/c/ensure-heading-shown)
                (cl-ecase parents-approach
                  (:parents/thin nil)
                  (:parents/fat (nomis/outline/c/show-children 1))))))))))

(defun -nomis/tree/ls/hsl-show-children (lineage-spec)
  (when (a-get lineage-spec :spec/pre-hide-children?)
    (outline-hide-subtree))
  (cl-ecase (a-get lineage-spec :spec/children-approach)
    ((nil) nil)
    (0 nil)
    (1 (outline-show-entry))
    (2 (outline-show-entry)
       (nomis/outline/c/show-children 1))
    (3 (outline-show-entry)
       (outline-show-branches))
    (4 (outline-show-subtree))))

(defun nomis/tree/ls/show-lineage (lineage-spec)
  (-nomis/tree/ls/hsl-hide lineage-spec)
  (-nomis/tree/ls/hsl-show-parents lineage-spec)
  (nomis/outline/c/ensure-heading-shown)
  (-nomis/tree/ls/hsl-show-children lineage-spec)
  (when (and (a-get lineage-spec :spec/pulse-max-children?)
             (= (a-get lineage-spec :spec/children-approach)
                nomis/tree/ls/children-approach-max))
    (nomis/outline/c/pulse-current-section)))

;;; End

(provide 'nomis-tree-lineage-specs)
