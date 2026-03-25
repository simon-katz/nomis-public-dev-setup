;;; nomis-tree-lineage-specs.el --- Lineage specs for tree display  -*- lexical-binding: t; -*-

;;; Code:

;;;; Requires

(require 'a)
(require 'nomis-msg)
(require 'nomis-outline-wrappers)

;;;; Lineage specs

;;;;; Preamble

;; A lineage spec controls how lineages are displayed. They are inspired by
;; `org`s visibility spans.

;; We could see `org-show-context-detail` for ideas for more lineage specs.

;; A lineages spec has the following entries (with permitted values nested):
;;
;; - `:spec/pre-hide-all?`
;;   - boolean
;;
;; - `:spec/pre-hide-children?`
;;   - boolean
;;
;; - `:spec/ancestors-approach` (doesn't hide anything, but can show things)
;;   - `nil`
;;     - Do nothing.
;;   - `:ancestors/thin`
;;     - Show ancestors.
;;   - `:ancestors/fat`
;;     - Show ancestors, siblings of ancestors, and siblings.
;;
;; - `:spec/children-approach` (doesn't hide anything, but can show things)
;;   - `nil` or `0`
;;     - Do nothing.
;;   - `1` / `2` / `3` / `4`
;;     - Show body/children/branches/subtree.
;;
;; - `:spec/show-body?`
;;   - boolean

;;;;; The sequence of lineage specs for less/more

(defconst nomis/tree/ls/children-approach-max 4)

(defconst nomis/tree/ls/spec/hide-all--no-ancestors--no-children
  (a-hash-table :spec/pre-hide-all? t))

(defconst nomis/tree/ls/spec/hide-all--thin-ancestors--no-children
  (a-hash-table :spec/pre-hide-all? t
                :spec/ancestors-approach :ancestors/thin))

(defconst nomis/tree/ls/spec/hide-all--fat-ancestors--no-children
  (a-hash-table :spec/pre-hide-all? t
                :spec/ancestors-approach :ancestors/fat))

(defconst nomis/tree/ls/spec/hide-all--fat-ancestors--immediate-children
  (a-hash-table :spec/pre-hide-all? t
                :spec/ancestors-approach :ancestors/fat
                :spec/children-approach 2))

(defconst nomis/tree/ls/spec/hide-all--fat-ancestors--all-children
  (a-hash-table :spec/pre-hide-all? t
                :spec/ancestors-approach :ancestors/fat
                :spec/children-approach nomis/tree/ls/children-approach-max))

(defconst nomis/tree/ls/spec/hide-all--fat-ancestors--all-children--show-body
  (a-hash-table :spec/pre-hide-all? t
                :spec/ancestors-approach :ancestors/fat
                :spec/children-approach nomis/tree/ls/children-approach-max
                :spec/show-body?        t))

(defconst -nomis/tree/ls/spec-sequence
  `((:minimal   "Minimal"          ,nomis/tree/ls/spec/hide-all--no-ancestors--no-children)
    (:ancestors "Ancestors"        ,nomis/tree/ls/spec/hide-all--thin-ancestors--no-children)
    (:lineage   "Fat ancestors"      ,nomis/tree/ls/spec/hide-all--fat-ancestors--no-children)
    (:tree      "Fat ancestors + immediate children" ,nomis/tree/ls/spec/hide-all--fat-ancestors--immediate-children)
    (:canonical "Fat ancestors + all children" ,nomis/tree/ls/spec/hide-all--fat-ancestors--all-children)
    (:canonical+body "Fat ancestors + all children + body" ,nomis/tree/ls/spec/hide-all--fat-ancestors--all-children--show-body)))

(defconst -nomis/tree/ls/spec-sequence-min-spec
  (cl-first -nomis/tree/ls/spec-sequence))

(defconst -nomis/tree/ls/spec-sequence-max-spec
  (-> -nomis/tree/ls/spec-sequence last cl-first))

(defconst nomis/tree/ls/spec-sequence-max-value
  (1- (length -nomis/tree/ls/spec-sequence)))

(defconst -nomis/tree/ls/initial-numeric-value
  (or (cl-position :ancestors -nomis/tree/ls/spec-sequence :key #'cl-first)
      (error "Didn't find :ancestors entry in -nomis/tree/ls/spec-sequence")))

;;;;; Other lineage specs

(defconst nomis/tree/ls/spec/no-hide--fat-ancestors--all-children
  (a-hash-table :spec/ancestors-approach :ancestors/fat
                :spec/children-approach nomis/tree/ls/children-approach-max))

;;;; Hide/show lineage

(defun -nomis/tree/ls/hsl-hide (lineage-spec)
  (let* ((pre-hide-all? (a-get lineage-spec :spec/pre-hide-all?))
         (ancestors-approach (a-get lineage-spec :spec/ancestors-approach)))
    (when pre-hide-all?
      (let* ((top-level-level (nomis/outline/w/top-level-level))
             (hide-level (cl-ecase ancestors-approach
                           ((nil :ancestors/thin) (1- top-level-level))
                           (:ancestors/fat top-level-level))))
        (outline-hide-sublevels (max 1 ; avoid error when < 1
                                     hide-level))))))

(defun -nomis/tree/ls/hsl-show-ancestors (lineage-spec)
  (let* ((ancestors-approach (a-get lineage-spec :spec/ancestors-approach)))
    (when ancestors-approach
      (let* ((ancestor-points
              (let* ((ps '()))
                (save-excursion
                  (beginning-of-line)
                  (while (nomis/outline/w/up-heading* 1 t t)
                    (push (point) ps)))
                ps)))
        (save-excursion
          (cl-loop
           for p in ancestor-points
           do (progn
                (goto-char p)
                (nomis/outline/w/ensure-heading-shown)
                (cl-ecase ancestors-approach
                  (:ancestors/thin nil)
                  (:ancestors/fat (nomis/outline/w/show-children 1))))))))))

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
  (save-excursion
    (nomis/outline/w/back-to-heading) ; not sure we need this, but it can't harm
    (-nomis/tree/ls/hsl-hide lineage-spec)
    (-nomis/tree/ls/hsl-show-ancestors lineage-spec)
    (nomis/outline/w/ensure-heading-shown)
    (-nomis/tree/ls/hsl-show-children lineage-spec)
    (when (eql lineage-spec
               nomis/tree/ls/spec/hide-all--fat-ancestors--all-children--show-body)
      (nomis/outline/w/pulse-current-section))))

(defconst -nomis/tree/ls/lineage/commands
  '(nomis/tree/lineage/less
    nomis/tree/lineage/more
    nomis/tree/lineage/set-min
    nomis/tree/lineage/set-max))

(defvar -nomis/tree/ls/lineage/prev-action-index -1)

(defun -nomis/tree/ls/lineage/set-level/numeric (n delta?)
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
      (cl-destructuring-bind (_ msg lineage-spec)
          (nth new-pos-or-nil -nomis/tree/ls/spec-sequence)
        (nomis/tree/ls/show-lineage lineage-spec)
        (if (a-get lineage-spec :spec/show-body?)
            (nomis/outline/w/show-entry)
          (nomis/outline/w/hide-entry))
        (nomis/popup/message "%s" msg)))))

(defun nomis/tree/ls/lineage/more ()
  (-nomis/tree/ls/lineage/set-level/numeric 1 t))

(defun nomis/tree/ls/lineage/less ()
  (-nomis/tree/ls/lineage/set-level/numeric -1 t))

(defun nomis/tree/ls/lineage/set-min ()
  (-nomis/tree/ls/lineage/set-level/numeric 0 nil))

(defun nomis/tree/ls/lineage/set-max ()
  (let* ((v nomis/tree/ls/spec-sequence-max-value))
    (-nomis/tree/ls/lineage/set-level/numeric v nil)))

;;;; nomis/tree/ls/show-after-find

(defun nomis/tree/ls/show-after-find ()
  (unless (nomis/outline/w/before-first-heading?)
    (nomis/tree/ls/show-lineage
     nomis/tree/ls/spec/no-hide--fat-ancestors--all-children)))

;;; End

(provide 'nomis-tree-lineage-specs)
