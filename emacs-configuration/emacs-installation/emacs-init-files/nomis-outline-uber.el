;;; nomis-outline-uber.el ---  -*- lexical-binding: t; -*-

;;; Code:

;;;; Requires

(require 'a)
(require 'cl-format)
(require 'cl-lib)
(require 'company)
(require 'dash)
(require 'nomis-outline)
(require 'nomis-popup)
(require 'outline)

;;;; Utilities

;;;;; Misc

(defun -nomis/outline/last-command ()
  (or (bound-and-true-p *nomis/smex/last-command*)
      last-command))

(defun -nomis/outline/ordinal (n)
  (cl-format nil "~a~a"
             n
             (let ((x (cl-format nil "~:r" n)))
               (cl-subseq x (- (length x) 2)))))

;;;;; nomis/outline/pulse-current-section

(defun nomis/outline/pulse-current-section ()
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

;;;;; Simple outline wrappers

(defun nomis/outline/on-heading? ()
  (outline-on-heading-p t))

(defun -nomis/outline/on-visible-heading? ()
  (outline-on-heading-p))

(defun nomis/outline/back-to-heading? ()
  (outline-back-to-heading t))

(defun -nomis/outline/back-to-visible-heading? ()
  (outline-back-to-heading))

(defun -nomis/outline/up-heading (n)
  (outline-up-heading n t))

(defun -nomis/outline/up-visible-heading (n)
  (outline-up-heading n))

(defun -nomis/outline/at-beginning-of-heading? ()
  (and (bolp)
       (nomis/outline/on-heading?)))

(defun -nomis/outline/on-top-level-heading? ()
  "Are we on a top-level heading?"
  ;; `(outline-level)` and `(funcall outline-level)` return weird numbers in
  ;; some modes. This, we hope, is bulletproof.
  (save-excursion
    (when (nomis/outline/on-heading?)
      (let* ((olevel (funcall outline-level)))
        (ignore-errors
          ;; `ignore-errors` is needed when before first heading.
          (-nomis/outline/up-heading 1))
        (or (not (nomis/outline/on-heading?)) ; blank lines at top of file?
            (= olevel (funcall outline-level)))))))

(defun -nomis/outline/top-level-level ()
  (cl-assert (nomis/outline/on-heading?))
  (save-excursion
    (goto-char (point-min))
    (unless (nomis/outline/on-heading?) (outline-next-heading))
    (funcall outline-level)))

(defun -nomis/outline/ensure-heading-shown ()
  (when (outline-invisible-p)
    ;; Is there a simpler way to show the heading but not the body?
    (outline-show-entry)
    (outline-hide-entry)))

(defun -nomis/show-children ()
  ;; The `1` is important; otherwise we get bodies of children.
  (outline-show-children 1))

(defun -nomis/outline/prev-or-next (direction)
  (cl-ecase direction
    (:backward (outline-previous-heading))
    (:forward (outline-next-heading))))

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

(defconst -nomis/outline/children-approach-max 4)

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
                :spec/children-approach -nomis/outline/children-approach-max))

(defconst step-lineage-spec
  (a-hash-table :spec/pre-hide-all? t
                :spec/parents-approach :parents/fat
                :spec/children-approach -nomis/outline/children-approach-max))

(defconst navigation-lineage-spec
  (a-hash-table :spec/parents-approach :parents/thin))

(defun show-children-lineage-spec (children-approach)
  (a-hash-table :spec/pre-hide-children? t
                :spec/children-approach children-approach
                :spec/pulse-max-children? t))

;;;;; Hide/show lineage

(defun -nomis/outline/hsl-hide (lineage-spec)
  (let* ((pre-hide-all? (a-get lineage-spec :spec/pre-hide-all?))
         (parents-approach (a-get lineage-spec :spec/parents-approach)))
    (when pre-hide-all?
      (let* ((top-level-level (-nomis/outline/top-level-level))
             (hide-level (cl-ecase parents-approach
                           ((nil :parents/thin) (1- top-level-level))
                           (:parents/fat top-level-level))))
        (outline-hide-sublevels (max 1 ; avoid error when < 1
                                     hide-level))))))

(defun -nomis/outline/hsl-show-parents (lineage-spec)
  (let* ((parents-approach (a-get lineage-spec :spec/parents-approach)))
    (when parents-approach
      (let* ((parent-points
              (let* ((ps '()))
                (save-excursion
                  (while (and (nomis/outline/on-heading?)
                              (not (-nomis/outline/on-top-level-heading?)))
                    (-nomis/outline/up-heading 1)
                    (push (point) ps)))
                ps)))
        (save-excursion
          (cl-loop
           for p in parent-points
           do (progn
                (goto-char p)
                (-nomis/outline/ensure-heading-shown)
                (cl-ecase parents-approach
                  (:parents/thin nil)
                  (:parents/fat (-nomis/show-children))))))))))

(defun -nomis/outline/hsl-show-children (lineage-spec)
  (when (a-get lineage-spec :spec/pre-hide-children?)
    (outline-hide-subtree))
  (cl-ecase (a-get lineage-spec :spec/children-approach)
    ((nil) nil)
    (0 nil)
    (1 (outline-show-entry))
    (2 (outline-show-entry)
       (-nomis/show-children))
    (3 (outline-show-entry)
       (outline-show-branches))
    (4 (outline-show-subtree))))

(defun -nomis/outline/show-lineage (lineage-spec)
  (-nomis/outline/hsl-hide lineage-spec)
  (-nomis/outline/hsl-show-parents lineage-spec)
  (-nomis/outline/ensure-heading-shown)
  (-nomis/outline/hsl-show-children lineage-spec)
  (when (and (a-get lineage-spec :spec/pulse-max-children?)
             (= (a-get lineage-spec :spec/children-approach)
                -nomis/outline/children-approach-max))
    (nomis/outline/pulse-current-section)))

;;;;; Previous/next helpers

(defun -nomis/outline/prev-next-same-level (direction sibling-or-peer)
  (let* ((opoint (point))
         (level (funcall outline-level))
         (npoint  (save-excursion
                    ;; The logic here is a copy-and-edit of
                    ;; `outline-get-last-sibling` and
                    ;; `outline-get-next-sibling`.
                    (-nomis/outline/prev-or-next direction)
                    (when (cl-ecase direction
                            (:backward (and (/= (point) opoint)
                                            (outline-on-heading-p t)))
                            (:forward t))
                      (while (and (cl-ecase direction
                                    (:backward t)
                                    (:forward (not (eobp))))
                                  (funcall (cl-ecase sibling-or-peer
                                             (:sibling #'>)
                                             (:peer #'/=))
                                           (funcall outline-level)
                                           level)
                                  (cl-ecase direction
                                    (:backward (not (bobp)))
                                    (:forward t)))
                        (-nomis/outline/prev-or-next direction))
                      (if (or (cl-ecase direction
                                (:backward nil)
                                (:forward (eobp)))
                              (< (funcall outline-level) level))
                          nil
                        (cl-assert (= level (funcall outline-level)))
                        (point))))))
    (when npoint
      (goto-char npoint))))

(defun -nomis/outline/prev-or-next-heading-pos (start
                                                direction
                                                kind)
  (when start
    (save-excursion
      (goto-char start)
      (let* ((boh? (-nomis/outline/at-beginning-of-heading?)))
        (if (and (eq direction :backward)
                 (not boh?))
            (progn
              (nomis/outline/back-to-heading?)
              (point))
          (when (and (eq direction :forward)
                     (not boh?))
            (nomis/outline/back-to-heading?))
          (cl-ecase kind
            (:any-level
             (-nomis/outline/prev-or-next direction))
            (:sibling
             (-nomis/outline/prev-next-same-level direction :sibling))
            (:peer
             (-nomis/outline/prev-next-same-level direction :peer)))
          (when (and (/= (point) start)
                     (nomis/outline/on-heading?))
            ;; ^^ Check of `(nomis/outline/on-heading?)` needed because
            ;;    `-nomis/outline/prev-or-next` goes to BOF or EOF when there's
            ;;    no prev/next heading.
            (point)))))))

(defun -nomis/outline/prev-or-next-heading (lineage-spec
                                            n
                                            direction
                                            kind)
  (let* ((pos (->> (-iterate (lambda (start)
                               (-nomis/outline/prev-or-next-heading-pos
                                start
                                direction
                                kind))
                             (point)
                             (1+ n))
                   cl-rest
                   (-drop (1- n))
                   cl-first)))
    (if pos
        (progn
          (goto-char pos)
          (-nomis/outline/show-lineage lineage-spec))
      (let* ((direction-word (cl-ecase direction
                               (:backward "previous")
                               (:forward "next")))
             (kind-word (cl-ecase kind
                          (:any-level "heading")
                          (:sibling "sibling")
                          (:peer "same-level"))))
        (nomis/popup/error-message
         "No %s%s %s"
         (if (= n 1) "" (concat (-nomis/outline/ordinal n)
                                "-"))
         direction-word
         kind-word)))))

;;;; API

;;;;; nomis/outline/show-all

;; TODO: Temporary, until we have
;;       `nomis/tree/show-children-from-all-roots/fully-expand`.
(defun nomis/outline/show-all ()
  (interactive)
  (outline-show-all)
  (nomis/msg/pulse-buffer))

;;;;; nomis/outline/show-children-from-point/xxxx

(defvar -nomis/outline/increments-children-approach)

(defun -nomis/outline/increments-children-approach/get ()
  ;; TODO: At some point change this to look at the actual text rather than
  ;;       relying on `-nomis/outline/increments-children-approach`.
  (when (member (-nomis/outline/last-command)
                '(nomis/tree/show-children-from-point/incremental/more
                  nomis/tree/show-children-from-point/incremental/less))
    -nomis/outline/increments-children-approach))

(defun -nomis/outline/inc-dec-message (approach clamped?)
  (nomis/popup/message (concat (cl-ecase approach
                                 (0 "Folded")
                                 (1 "Body")
                                 (2 "Children")
                                 (3 "Branches")
                                 (4 "Subtree"))
                               (when clamped? " (clamped)"))))

(defun -nomis/outline/increments-children-approach/set (approach clamped?)
  (setq -nomis/outline/increments-children-approach approach)
  (-nomis/outline/show-lineage (show-children-lineage-spec approach))
  (-nomis/outline/inc-dec-message approach clamped?))

(defun -nomis/outline/set-n-children-from-point (n)
  (let* ((new-approach (max 0
                            (min n
                                 -nomis/outline/children-approach-max)))
         (clamped? (/= n new-approach)))
    (-nomis/outline/increments-children-approach/set new-approach
                                                     clamped?)))

(defun nomis/outline/show-children-from-point/incremental/less (n)
  (if n
      (-nomis/outline/set-n-children-from-point n)
    (let* ((current-approach (-nomis/outline/increments-children-approach/get)))
      (if (eql current-approach 0)
          (nomis/popup/error-message "Already fully collapsed")
        (let* ((new-approach (if current-approach
                                 (1- current-approach)
                               -nomis/outline/children-approach-max)))
          (-nomis/outline/increments-children-approach/set new-approach
                                                           nil))))))

(defun nomis/outline/show-children-from-point/incremental/more (n)
  (if n
      (-nomis/outline/set-n-children-from-point n)
    (let* ((current-approach (-nomis/outline/increments-children-approach/get)))
      (if (eql current-approach -nomis/outline/children-approach-max)
          (nomis/popup/error-message "Already fully expanded")
        (let* ((new-approach (if current-approach
                                 (1+ current-approach)
                               0)))
          (-nomis/outline/increments-children-approach/set new-approach
                                                           nil))))))

;;;;; Search heading text

;;;;; nomis/outline/visibility-span

(defun nomis/outline/visibility-span/set-max ()
  (-nomis/outline/show-lineage max-visibility-span-lineage-spec))

;;;;; nomis/outline/show-max-lineage

(defun nomis/outline/show-max-lineage ()
  (-nomis/outline/show-lineage max-lineage-spec))

;;;;; nomis/outline/show-tree-only

(defun nomis/outline/show-tree-only ()
  (-nomis/outline/show-lineage fat-parents-lineage-spec))

;;;;; Expand/collapse

;;;;; Movement

;;;;; Previous

(defun nomis/outline/previous-heading (n)
  (-nomis/outline/prev-or-next-heading navigation-lineage-spec
                                       n
                                       :backward
                                       :any-level))

(defun nomis/outline/previous-sibling (n)
  "Move backward to the N'th heading at same level as this one.
Stop at the first and last headings of a superior heading."
  (interactive "p")
  (-nomis/outline/prev-or-next-heading navigation-lineage-spec
                                       n
                                       :backward
                                       :sibling))

(defun nomis/outline/previous-peer (n)
  "Move backward to the N'th heading at same level as this one.
Can pass by a superior heading."
  (interactive "p")
  (-nomis/outline/prev-or-next-heading navigation-lineage-spec
                                       n
                                       :backward
                                       :peer))

(defun nomis/outline/step-backward-sibling (n)
  "Move backward to the N'th heading at same level as this one, then show
fat parents and all children.
Stop at the first and last headings of a superior heading."
  (-nomis/outline/prev-or-next-heading step-lineage-spec
                                       (or n 1)
                                       :backward
                                       :sibling))

(defun nomis/outline/step-backward-peer (n)
  "Move backward to the N'th heading at same level as this one, then show
fat parents and all children.
Can pass by a superior heading."
  (-nomis/outline/prev-or-next-heading step-lineage-spec
                                       (or n 1)
                                       :backward
                                       :peer))

;;;;; Next

(defun nomis/outline/next-heading (n)
  (-nomis/outline/prev-or-next-heading navigation-lineage-spec
                                       n
                                       :forward
                                       :any-level))

(defun nomis/outline/next-sibling (n)
  "Move forward to the N'th heading at same level as this one.
Stop at the first and last headings of a superior heading."
  (interactive "p")
  (-nomis/outline/prev-or-next-heading navigation-lineage-spec
                                       n
                                       :forward
                                       :sibling))

(defun nomis/outline/next-peer (n)
  "Move forward to the N'th heading at same level as this one.
Can pass by a superior heading."
  (interactive "p")
  (-nomis/outline/prev-or-next-heading navigation-lineage-spec
                                       n
                                       :forward
                                       :peer))

(defun nomis/outline/step-forward-sibling (n)
  "Move forward to the N'th heading at same level as this one, then show
fat parents and all children.
Stop at the first and last headings of a superior heading."
  (-nomis/outline/prev-or-next-heading step-lineage-spec
                                       (or n 1)
                                       :forward
                                       :sibling))

(defun nomis/outline/step-forward-peer (n)
  "Move forward to the N'th heading at same level as this one, then show
fat parents and all children.
Can pass by a superior heading."
  (-nomis/outline/prev-or-next-heading step-lineage-spec
                                       (or n 1)
                                       :forward
                                       :peer))

;;; End

(provide 'nomis-outline-uber)
