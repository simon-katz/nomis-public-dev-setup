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
(require 'outline)

;;;; Utilities

;;;;; nomis/tree/outline/pulse-current-section

(defun nomis/tree/outline/pulse-current-section ()
  (let ((start (point)))
    (cl-flet ((next-same-level-heading ()
                (save-excursion (ignore-errors
                                  (outline-forward-same-level 1)
                                  (point))))
              (next-up-one-level-heading ()
                (save-excursion (ignore-errors
                                  (outline-up-heading 1)
                                  (outline-forward-same-level 1)
                                  (unless (= (point) start)
                                    ;; We have this guard because
                                    ;; `outline-up-heading` is broken when
                                    ;; there's no up-one-level heading.
                                    (point))))))
      (let* ((end (or (next-same-level-heading)
                      (next-up-one-level-heading)
                      (point-max))))
        (pulse-momentary-highlight-region start end)))))

;;;;; Lineage spec

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

;; TODO: See `org-show-context-detail` for ideas for more lineage specs.

(defconst -nomis/tree/outline/children-approach-max 4)

(defconst fat-parents-lineage-spec
  (a-hash-table :spec/pre-hide-all? t
                :spec/parents-approach :parents/fat))

(defconst max-visibility-span-lineage-spec
  (a-hash-table :spec/pre-hide-all? t
                :spec/parents-approach :parents/fat
                :spec/children-approach 1))

(defconst max-lineage-spec
  (a-hash-table :spec/pre-hide-all? t
                :spec/parents-approach :parents/fat
                :spec/children-approach -nomis/tree/outline/children-approach-max))

(defconst step-lineage-spec
  (a-hash-table :spec/pre-hide-all? t
                :spec/parents-approach :parents/fat
                :spec/children-approach -nomis/tree/outline/children-approach-max))

(defconst navigation-lineage-spec
  (a-hash-table :spec/parents-approach :parents/thin))

(defun show-children-lineage-spec (children-approach)
  (a-hash-table :spec/pre-hide-children? t
                :spec/children-approach children-approach
                :spec/pulse-max-children? t))

;;;;; Hide/show lineage

(defun -nomis/tree/outline/hsl-hide (lineage-spec)
  (let* ((pre-hide-all? (a-get lineage-spec :spec/pre-hide-all?))
         (parents-approach (a-get lineage-spec :spec/parents-approach)))
    (when pre-hide-all?
      (let* ((top-level-level (-nomis/outline/c/top-level-level))
             (hide-level (cl-ecase parents-approach
                           ((nil :parents/thin) (1- top-level-level))
                           (:parents/fat top-level-level))))
        (outline-hide-sublevels (max 1 ; avoid error when < 1
                                     hide-level))))))

(defun -nomis/tree/outline/hsl-show-parents (lineage-spec)
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

(defun -nomis/tree/outline/hsl-show-children (lineage-spec)
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

(defun -nomis/tree/outline/show-lineage (lineage-spec)
  (-nomis/tree/outline/hsl-hide lineage-spec)
  (-nomis/tree/outline/hsl-show-parents lineage-spec)
  (nomis/outline/c/ensure-heading-shown)
  (-nomis/tree/outline/hsl-show-children lineage-spec)
  (when (and (a-get lineage-spec :spec/pulse-max-children?)
             (= (a-get lineage-spec :spec/children-approach)
                -nomis/tree/outline/children-approach-max))
    (nomis/tree/outline/pulse-current-section)))

;;;;; Previous/next helpers

(defun -nomis/tree/outline/prev-or-next-heading-and-show-lineage (lineage-spec
                                                                  n
                                                                  direction
                                                                  kind)
  (when (nomis/outline/c/prev-or-next-heading n direction kind)
    (-nomis/tree/outline/show-lineage lineage-spec)))

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
  (-nomis/tree/outline/show-lineage (show-children-lineage-spec approach))
  (-nomis/tree/outline/inc-dec-message approach clamped?))

(defun -nomis/tree/outline/set-n-children-from-point (n)
  (let* ((new-approach (max 0
                            (min n
                                 -nomis/tree/outline/children-approach-max)))
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
                               -nomis/tree/outline/children-approach-max)))
          (-nomis/tree/outline/increments-children-approach/set new-approach
                                                                nil))))))

(defun nomis/tree/outline/show-children-from-point/incremental/more (n)
  (if n
      (-nomis/tree/outline/set-n-children-from-point n)
    (let* ((current-approach (-nomis/tree/outline/increments-children-approach/get)))
      (if (eql current-approach -nomis/tree/outline/children-approach-max)
          (nomis/popup/error-message "Already fully expanded")
        (let* ((new-approach (if current-approach
                                 (1+ current-approach)
                               0)))
          (-nomis/tree/outline/increments-children-approach/set new-approach
                                                                nil))))))

;;;;; nomis/tree/outline/visibility-span

(defun nomis/tree/outline/visibility-span/set-max ()
  (-nomis/tree/outline/show-lineage max-visibility-span-lineage-spec))

;;;;; nomis/tree/outline/show-max-lineage

(defun nomis/tree/outline/show-max-lineage ()
  (-nomis/tree/outline/show-lineage max-lineage-spec))

;;;;; nomis/tree/outline/show-tree-only

(defun nomis/tree/outline/show-tree-only ()
  (-nomis/tree/outline/show-lineage fat-parents-lineage-spec))

;;;;; Previous

(defun nomis/tree/outline/previous-heading (n)
  (-nomis/tree/outline/prev-or-next-heading-and-show-lineage navigation-lineage-spec
                                                             n
                                                             :backward
                                                             :any-level))

(defun nomis/tree/outline/previous-sibling (n)
  "Move backward to the N'th heading at same level as this one.
Stop at the first and last headings of a superior heading."
  (interactive "p")
  (-nomis/tree/outline/prev-or-next-heading-and-show-lineage navigation-lineage-spec
                                                             n
                                                             :backward
                                                             :sibling))

(defun nomis/tree/outline/previous-peer (n)
  "Move backward to the N'th heading at same level as this one.
Can pass by a superior heading."
  (interactive "p")
  (-nomis/tree/outline/prev-or-next-heading-and-show-lineage navigation-lineage-spec
                                                             n
                                                             :backward
                                                             :peer))

(defun nomis/tree/outline/step-backward-sibling (n)
  "Move backward to the N'th heading at same level as this one, then show
fat parents and all children.
Stop at the first and last headings of a superior heading."
  (-nomis/tree/outline/prev-or-next-heading-and-show-lineage step-lineage-spec
                                                             (or n 1)
                                                             :backward
                                                             :sibling))

(defun nomis/tree/outline/step-backward-peer (n)
  "Move backward to the N'th heading at same level as this one, then show
fat parents and all children.
Can pass by a superior heading."
  (-nomis/tree/outline/prev-or-next-heading-and-show-lineage step-lineage-spec
                                                             (or n 1)
                                                             :backward
                                                             :peer))

;;;;; Next

(defun nomis/tree/outline/next-heading (n)
  (-nomis/tree/outline/prev-or-next-heading-and-show-lineage navigation-lineage-spec
                                                             n
                                                             :forward
                                                             :any-level))

(defun nomis/tree/outline/next-sibling (n)
  "Move forward to the N'th heading at same level as this one.
Stop at the first and last headings of a superior heading."
  (interactive "p")
  (-nomis/tree/outline/prev-or-next-heading-and-show-lineage navigation-lineage-spec
                                                             n
                                                             :forward
                                                             :sibling))

(defun nomis/tree/outline/next-peer (n)
  "Move forward to the N'th heading at same level as this one.
Can pass by a superior heading."
  (interactive "p")
  (-nomis/tree/outline/prev-or-next-heading-and-show-lineage navigation-lineage-spec
                                                             n
                                                             :forward
                                                             :peer))

(defun nomis/tree/outline/step-forward-sibling (n)
  "Move forward to the N'th heading at same level as this one, then show
fat parents and all children.
Stop at the first and last headings of a superior heading."
  (-nomis/tree/outline/prev-or-next-heading-and-show-lineage step-lineage-spec
                                                             (or n 1)
                                                             :forward
                                                             :sibling))

(defun nomis/tree/outline/step-forward-peer (n)
  "Move forward to the N'th heading at same level as this one, then show
fat parents and all children.
Can pass by a superior heading."
  (-nomis/tree/outline/prev-or-next-heading-and-show-lineage step-lineage-spec
                                                             (or n 1)
                                                             :forward
                                                             :peer))

;;; End

(provide 'nomis-tree-outline-2)
