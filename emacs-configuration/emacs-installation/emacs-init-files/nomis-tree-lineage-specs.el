;;; nomis-tree-lineage-specs.el --- Lineage specs for tree display  -*- lexical-binding: t; -*-

;;; Code:

;;;; Requires

(require 'a)
(require 'nomis-msg)
(require 'nomis-outline-wrappers)

;;;; Lineage specs

;;;;; Preamble

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

;;;;; Lineage spec sequence

(defconst nomis/tree/ls/children-approach-max 4)

(defconst nomis/tree/ls/spec/min
  (a-hash-table :spec/pre-hide-all? t))

(defconst nomis/tree/ls/spec/thin-parents
  (a-hash-table :spec/pre-hide-all? t
                :spec/parents-approach :parents/thin))

(defconst nomis/tree/ls/spec/fat-parents
  (a-hash-table :spec/pre-hide-all? t
                :spec/parents-approach :parents/fat))

(defconst nomis/tree/ls/spec/fat-parents-immediate-children
  (a-hash-table :spec/pre-hide-all? t
                :spec/parents-approach :parents/fat
                :spec/children-approach 2))

(defconst nomis/tree/ls/spec/fat-parents-all-children
  (a-hash-table :spec/pre-hide-all? t
                :spec/parents-approach :parents/fat
                :spec/children-approach nomis/tree/ls/children-approach-max))

(defconst -nomis/tree/ls/spec-sequence
  ;; TODO: Change this so that the third item (`show?`) is part of a lineage
  ;;       spec. So we'll need to replace
  ;;       `nomis/tree/ls/spec/fat-parents-all-children` with two
  ;;       new ones.
  `((:minimal   "Minimal"          nil ,nomis/tree/ls/spec/min)
    (:ancestors "Ancestors"        nil ,nomis/tree/ls/spec/thin-parents)
    (:lineage   "Lineage"          nil ,nomis/tree/ls/spec/fat-parents)
    (:tree      "Tree"             nil ,nomis/tree/ls/spec/fat-parents-immediate-children)
    (:canonical "Canonical"        nil ,nomis/tree/ls/spec/fat-parents-all-children)
    (:canonical "Canonical + body" t   ,nomis/tree/ls/spec/fat-parents-all-children)))

(defconst -nomis/tree/ls/spec-sequence-min-spec
  (cl-first -nomis/tree/ls/spec-sequence))

(defconst -nomis/tree/ls/spec-sequence-max-spec
  (-> -nomis/tree/ls/spec-sequence last cl-first))

(defconst nomis/tree/ls/spec-sequence-max-value
  (1- (length -nomis/tree/ls/spec-sequence)))

(defconst -nomis/tree/ls/initial-numeric-value
  (or (cl-position :ancestors -nomis/tree/ls/spec-sequence :key #'cl-first)
      (error "Didn't find :ancestors entry in -nomis/tree/ls/spec-sequence")))

(defconst -nomis/tree/ls/tree+body-value
  (or (cl-position :tree -nomis/tree/ls/spec-sequence :key #'cl-first)
      (error "Didn't find :tree entry in -nomis/tree/ls/spec-sequence")))

;;;;; Other lineage specs

(defconst nomis/tree/ls/spec/no-hide-fat-parents ; TODO: => we want to rename others to say "hide"
  (a-hash-table :spec/parents-approach :parents/fat))

(defconst nomis/tree/ls/spec/fat-parents-one-child
  (a-hash-table :spec/pre-hide-all? t
                :spec/parents-approach :parents/fat
                :spec/children-approach 1))

(defconst nomis/tree/ls/spec/step
  (a-hash-table :spec/pre-hide-all? t
                :spec/parents-approach :parents/fat
                :spec/children-approach nomis/tree/ls/children-approach-max))

(defconst nomis/tree/ls/spec/navigation
  (a-hash-table :spec/parents-approach :parents/thin))

(defun nomis/tree/ls/spec/show-children (children-approach)
  (a-hash-table :spec/pre-hide-children? t
                :spec/children-approach children-approach
                :spec/pulse-max-children? t))

;;;; Hide/show lineage

(defun -nomis/tree/ls/hsl-hide (lineage-spec)
  (let* ((pre-hide-all? (a-get lineage-spec :spec/pre-hide-all?))
         (parents-approach (a-get lineage-spec :spec/parents-approach)))
    (when pre-hide-all?
      (let* ((top-level-level (nomis/outline/w/top-level-level))
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
                  (beginning-of-line)
                  (while (nomis/outline/w/up-heading 1 t t)
                    (push (point) ps)))
                ps)))
        (save-excursion
          (cl-loop
           for p in parent-points
           do (progn
                (goto-char p)
                (nomis/outline/w/ensure-heading-shown)
                (cl-ecase parents-approach
                  (:parents/thin nil)
                  (:parents/fat (nomis/outline/w/show-children 1))))))))))

(defun -nomis/tree/ls/hsl-show-children (lineage-spec)
  (when (a-get lineage-spec :spec/pre-hide-children?)
    (outline-hide-subtree))
  (cl-ecase (a-get lineage-spec :spec/children-approach)
    ((nil) nil)
    (0 nil)
    (1 (outline-show-entry))
    (2 (outline-show-entry)
       (nomis/outline/w/show-children 1))
    (3 (outline-show-entry)
       (outline-show-branches))
    (4 (outline-show-subtree))))

(defun nomis/tree/ls/show-lineage (lineage-spec)
  (-nomis/tree/ls/hsl-hide lineage-spec)
  (-nomis/tree/ls/hsl-show-parents lineage-spec)
  (nomis/outline/w/ensure-heading-shown)
  (-nomis/tree/ls/hsl-show-children lineage-spec)
  (when (and (a-get lineage-spec :spec/pulse-max-children?)
             (= (a-get lineage-spec :spec/children-approach)
                nomis/tree/ls/children-approach-max))
    (nomis/outline/w/pulse-current-section)))

;;;; Functionality moved from `nomis-tree-impl` -- for integration here

;; TODO: Integrate this functionality that was moved from `nomis-tree-impl`.

;;;;; Lineage

(defconst -nomis/tree/ls/lineage/commands
  '(nomis/tree/lineage/less
    nomis/tree/lineage/more
    nomis/tree/lineage/set-min
    nomis/tree/lineage/set-max))

(defvar -nomis/tree/ls/lineage/prev-action-index -1)

(defun -nomis/tree/ls/lineage/set-level/numeric (n delta?
                                          &optional no-message?)
  (let* ((prev-command-was-not-lineage?
          (not (member (nomis/outline/w/last-command)
                       -nomis/tree/ls/lineage/commands)))
         (prev-action-index -nomis/tree/ls/lineage/prev-action-index)
         (action-index (cond
                        ((not delta?)
                         n)
                        (prev-command-was-not-lineage?
                         -nomis/tree/ls/initial-numeric-value)
                        (t
                         (+ n prev-action-index))))
         (ok? (if delta?
                  (<= 0
                      action-index
                      nomis/tree/ls/spec-sequence-max-value)
                (or prev-command-was-not-lineage?
                    (not (= n prev-action-index)))))
         (new-pos-or-nil (if ok?
                             (progn
                               (setq -nomis/tree/ls/lineage/prev-action-index
                                     action-index)
                               action-index)
                           nil)))
    (if (null new-pos-or-nil)
        (let* ((msg (cl-second
                     (if (if delta? (< n 0) (= n 0))
                         -nomis/tree/ls/spec-sequence-min-spec
                       -nomis/tree/ls/spec-sequence-max-spec))))
          (nomis/popup/error-message "%s" msg))
      (cl-destructuring-bind (_ msg show? lineage-spec)
          (nth new-pos-or-nil -nomis/tree/ls/spec-sequence)
        (nomis/tree/ls/show-lineage lineage-spec)
        (if show? (nomis/outline/w/show-entry) (nomis/outline/w/hide-entry))
        (unless no-message?
          (nomis/popup/message "%s" msg))))))

(defun nomis/tree/ls/lineage/more ()
  (-nomis/tree/ls/lineage/set-level/numeric 1 t))

(defun nomis/tree/ls/lineage/less ()
  (-nomis/tree/ls/lineage/set-level/numeric -1 t))

(defun nomis/tree/ls/lineage/set-min ()
  (-nomis/tree/ls/lineage/set-level/numeric 0 nil))

(defun nomis/tree/ls/lineage/set-max ()
  (let* ((v nomis/tree/ls/spec-sequence-max-value))
    (-nomis/tree/ls/lineage/set-level/numeric v nil)))

;;; End

(provide 'nomis-tree-lineage-specs)
