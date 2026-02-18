;;; nomis-outline-uber -- -*- lexical-binding: t -*-

(require 'a)
(require 'cl-lib)
(require 'dash)
(require 'nomis-popup)
(require 'nomis-scrolling)

;;; Utilities

;;;; Misc

(defun -nomis/outline-last-command ()
  (or (bound-and-true-p *nomis/smex/last-command*)
      last-command))

(defun -nomis/outline-ordinal (n)
  (cl-format nil "~a~a"
             n
             (let ((x (cl-format nil "~:r" n)))
               (subseq x (- (length x) 2)))))

;;;; Simple outline wrappers

(defun -nomis/outline-on-heading? ()
  (outline-on-heading-p t))

(defun -nomis/outline-on-visible-heading? () ; TODO: Unused.
  (outline-on-heading-p))

(defun -nomis/outline-back-to-heading? ()
  (outline-back-to-heading t))

(defun -nomis/outline-back-to-visible-heading? () ; TODO: Unused.
  (outline-back-to-heading))

(defun -nomis/outline-up-heading (n)
  (outline-up-heading n t))

(defun -nomis/outline-up-visible-heading (n) ; TODO: Unused.
  (outline-up-heading n))

(defun -nomis/outline-at-beginning-of-heading? ()
  (and (bolp)
       (-nomis/outline-on-heading?)))

(defun -nomis/outline-on-top-level-heading? ()
  "Are we on a top-level heading?"
  ;; `(outline-level)` and `(funcall outline-level)` return weird numbers in
  ;; some modes. This, we hope, is bulletproof.
  (save-excursion
    (when (-nomis/outline-on-heading?)
      (let* ((opoint (point))
             (olevel (funcall outline-level)))
        (ignore-errors
          ;; `ignore-errors` is needed when before first heading.
          (-nomis/outline-up-heading 1))
        (or (not (-nomis/outline-on-heading?)) ; blank lines at top of file?
            (= olevel (funcall outline-level)))))))

(defun -nomis/outline-top-level-level ()
  (assert (-nomis/outline-on-heading?))
  (save-excursion
    (beginning-of-buffer)
    (unless (-nomis/outline-on-heading?) (outline-next-heading))
    (funcall outline-level)))

(defun -nomis/outline-ensure-heading-shown ()
  (when (outline-invisible-p)
    ;; Is there a simpler way to show the heading but not the body?
    (outline-show-entry)
    (outline-hide-entry)))

(defun -nomis/show-children ()
  ;; The `1` is important; otherwise we get bodies of children.
  (outline-show-children 1))

(defun -nomis/outline-prev-or-next (direction)
  (cl-ecase direction
    (:backward (outline-previous-heading))
    (:forward (outline-next-heading))))

;;;; Lineage spec

;; A lineage-spec controls how lineages are displayed and has the following
;; entries (with permitted values nested):
;;
;; - `:spec/pre-hide-all?`
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
;;   - `1` / `2` / `3`
;;     - Show children/branches/subtree.

(defconst max-lineage-spec
  (a-hash-table :spec/parents-approach :parents/fat
                :spec/children-approach 3))

(defconst step-lineage-spec
  (a-hash-table :spec/pre-hide-all? t
                :spec/parents-approach :parents/fat
                :spec/children-approach 3))

(defconst ensure-visible-lineage-spec
  (a-hash-table :spec/parents-approach :parents/thin))

(defun lineage-with-incs-or-decs-lineage-spec (children-approach)
  (a-hash-table :spec/pre-hide-all? t
                :spec/parents-approach :parents/fat
                :spec/children-approach children-approach
                :spec/pulse-max-children? t))

;;;; Hide/show lineage

(defun -nomis/outline-hsl-hide (lineage-spec)
  (let* ((pre-hide-all? (a-get lineage-spec :spec/pre-hide-all?))
         (parents-approach (a-get lineage-spec :spec/parents-approach)))
    (when pre-hide-all?
      (let* ((top-level-level (-nomis/outline-top-level-level))
             (hide-level (cl-ecase parents-approach
                           ((nil :parents/thin) (1- top-level-level))
                           (:parents/fat top-level-level))))
        (outline-hide-sublevels (max 1 ; avoid error when < 1
                                     hide-level))))))

(defun -nomis/outline-hsl-show-parents (lineage-spec)
  (let* ((parents-approach (a-get lineage-spec :spec/parents-approach)))
    (when parents-approach
      (let* ((parent-points
              (let* ((ps '()))
                (save-excursion
                  (while (and (-nomis/outline-on-heading?)
                              (not (-nomis/outline-on-top-level-heading?)))
                    (-nomis/outline-up-heading 1)
                    (push (point) ps)))
                ps)))
        (save-excursion
          (cl-loop
           for p in parent-points
           do (progn
                (goto-char p)
                (-nomis/outline-ensure-heading-shown)
                (cl-ecase parents-approach
                  (:parents/thin nil)
                  (:parents/fat (-nomis/show-children))))))))))

(defun -nomis/outline-hsl-show-children (lineage-spec)
  (cl-ecase (a-get lineage-spec :spec/children-approach)
    ((nil) nil)
    (0 nil)
    (1 (-nomis/show-children))
    (2 (outline-show-branches))
    (3 (outline-show-subtree))))

(defun -nomis/outline-show-lineage (lineage-spec)
  (-nomis/outline-hsl-hide lineage-spec)
  (-nomis/outline-hsl-show-parents lineage-spec)
  (-nomis/outline-ensure-heading-shown)
  (-nomis/outline-hsl-show-children lineage-spec)
  (when (and (a-get lineage-spec :spec/pulse-max-children?)
             (= (a-get lineage-spec :spec/children-approach) 3))
    (-nomis/outline-pulse-current-section)))

;;;; Previous/next helpers

(defun -nomis/outline-prev-next-same-level (direction allow-cross-parent?)
  (let* ((opoint (point))
         (level (funcall outline-level))
         (npoint  (save-excursion
                    ;; The logic here is a copy-and-edit of
                    ;; `outline-get-last-sibling` and
                    ;; `outline-get-next-sibling`.
                    (-nomis/outline-prev-or-next direction)
                    (when (cl-ecase direction
                            (:backward (and (/= (point) opoint)
                                            (outline-on-heading-p t)))
                            (:forward t))
                      (while (and (cl-ecase direction
                                    (:backward t)
                                    (:forward (not (eobp))))
                                  (funcall (if allow-cross-parent? #'/= #'>)
                                           (funcall outline-level)
                                           level)
                                  (cl-ecase direction
                                    (:backward (not (bobp)))
                                    (:forward t)))
                        (-nomis/outline-prev-or-next direction))
                      (if (or (cl-ecase direction
                                (:backward nil)
                                (:forward (eobp)))
                              (< (funcall outline-level) level))
                          nil
                        (cl-assert (= level (funcall outline-level)))
                        (point))))))
    (when npoint
      (goto-char npoint))))

(defun -nomis/outline-prev-or-next-heading-pos (lineage-spec
                                                start
                                                direction
                                                kind)
  (when start
    (save-excursion
      (goto-char start)
      (let* ((boh? (-nomis/outline-at-beginning-of-heading?)))
        (if (and (eq direction :backward)
                 (not boh?))
            (progn
              (-nomis/outline-back-to-heading?)
              (point))
          (when (and (eq direction :forward)
                     (not boh?))
            (-nomis/outline-back-to-heading?))
          (cl-ecase kind
            (:any-level
             (-nomis/outline-prev-or-next direction))
            (:sibling
             (-nomis/outline-prev-next-same-level direction nil))
            (:same-level-allow-cross-parent
             (-nomis/outline-prev-next-same-level direction t)))
          (when (and (/= (point) start)
                     (-nomis/outline-on-heading?))
            ;; ^^ Check of `(-nomis/outline-on-heading?)` needed because
            ;;    `-nomis/outline-prev-or-next` goes to BOF or EOF when there's
            ;;    no prev/next heading.
            (point)))))))

(defun -nomis/outline-prev-or-next-heading (lineage-spec
                                            n
                                            direction
                                            kind)
  (let* ((pos (->> (-iterate (lambda (start)
                               (-nomis/outline-prev-or-next-heading-pos
                                lineage-spec
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
          (-nomis/outline-show-lineage lineage-spec))
      (let* ((direction-word (cl-ecase direction
                               (:backward "previous")
                               (:forward "next")))
             (kind-word (cl-ecase kind
                          (:any-level "heading")
                          (:sibling "sibling")
                          (:same-level-allow-cross-parent "same-level"))))
        (error (cl-format nil
                          "No ~a~a ~a"
                          (if (= n 1) "" (concat (-nomis/outline-ordinal n)
                                                 "-"))
                          direction-word
                          kind-word))))))

;;;; -nomis/outline-command

(defun -nomis/outline-command* (f)
  (push-mark)
  (nomis/scrolling/with-maybe-maintain-line-no-in-window
    (funcall f)))

(cl-defmacro -nomis/outline-command (_opts &body body)
  (declare (indent 1))
  `(-nomis/outline-command* (lambda () ,@body) ))

;;; API

;;;; nomis/outline-show-lineage-with-increments

(defvar -nomis/outline-increments-children-approach)

(defun nomis/outline-show-lineage-with-incs-or-decs (one-or-minus-one)
  (let* ((approach (if (not (member (-nomis/outline-last-command)
                                    '(nomis/outline-show-lineage-with-increments
                                      nomis/outline-show-lineage-with-decrements)))
                       0
                     (mod (+ -nomis/outline-increments-children-approach
                             one-or-minus-one)
                          4))))
    (setq -nomis/outline-increments-children-approach approach)
    (-nomis/outline-show-lineage (lineage-with-incs-or-decs-lineage-spec
                                  approach))
    (cl-ecase approach
      (0 (nomis/popup/message "Folded"))
      (1 (nomis/popup/message "Children"))
      (2 (nomis/popup/message "Branches"))
      (3 (nomis/popup/message "Subtree")))))

(defun nomis/outline-show-lineage-with-increments ()
  (interactive)
  ;; Repeated invocations cycle amount of child stuff.
  (nomis/outline-show-lineage-with-incs-or-decs 1))

(defun nomis/outline-show-lineage-with-decrements ()
  (interactive)
  ;; Repeated invocations cycle amount of child stuff backwards.
  (nomis/outline-show-lineage-with-incs-or-decs -1))

;;;; nomis/outline-show-max-lineage

(defun nomis/outline-show-max-lineage ()
  (interactive)
  (-nomis/outline-show-lineage max-lineage-spec))

;;;; nomis/outline-cycle-or-indent-or-complete

(defun nomis/outline-cycle-or-indent-or-complete (arg)
  (interactive "P")
  (if (and (bolp)
           (looking-at-p outline-regexp))
      (bicycle-cycle arg)
    ;; Maybe we could find what Tab would be bound to if `outline-minor-mode`
    ;; were not enabled. I've tried but it's non-trivial. So I'm not bothering,
    ;; at least for now.
    (company-indent-or-complete-common arg)))

;;;; Previous

(defun nomis/outline-previous-heading (n)
  (interactive "p")
  (-nomis/outline-command
      nil
    (-nomis/outline-prev-or-next-heading ensure-visible-lineage-spec
                                         n
                                         :backward
                                         :any-level)))

(defun nomis/outline-previous-sibling (n)
  "Move backward to the N'th heading at same level as this one.
Stop at the first and last headings of a superior heading."
  (interactive "p")
  (-nomis/outline-command
      nil
    (-nomis/outline-prev-or-next-heading ensure-visible-lineage-spec
                                         n
                                         :backward
                                         :sibling)))

(defun nomis/outline-previous-sibling/allow-cross-parent (n)
  "Move backward to the N'th heading at same level as this one.
Can pass by a superior heading."
  (interactive "p")
  (-nomis/outline-command
      nil
    (-nomis/outline-prev-or-next-heading ensure-visible-lineage-spec
                                         n
                                         :backward
                                         :same-level-allow-cross-parent)))

(defun nomis/outline-step-backward (n)
  "Move backward to the N'th heading at same level as this one, then show
fat parents and all children.
Stop at the first and last headings of a superior heading."
  (interactive "p")
  (-nomis/outline-command
      nil
    (-nomis/outline-prev-or-next-heading step-lineage-spec
                                         n
                                         :backward
                                         :sibling)))

(defun nomis/outline-step-backward/allow-cross-parent (n)
  "Move backward to the N'th heading at same level as this one, then show
fat parents and all children.
Can pass by a superior heading."
  (interactive "p")
  (-nomis/outline-command
      nil
    (-nomis/outline-prev-or-next-heading step-lineage-spec
                                         n
                                         :backward
                                         :same-level-allow-cross-parent)))

;;;; Next

(defun nomis/outline-next-heading (n)
  (interactive "p")
  (-nomis/outline-command
      nil
    (-nomis/outline-prev-or-next-heading ensure-visible-lineage-spec
                                         n
                                         :forward
                                         :any-level)))

(defun nomis/outline-next-sibling (n)
  "Move forward to the N'th heading at same level as this one.
Stop at the first and last headings of a superior heading."
  (interactive "p")
  (-nomis/outline-command
      nil
    (-nomis/outline-prev-or-next-heading ensure-visible-lineage-spec
                                         n
                                         :forward
                                         :sibling)))

(defun nomis/outline-next-sibling/allow-cross-parent (n)
  "Move forward to the N'th heading at same level as this one.
Can pass by a superior heading."
  (interactive "p")
  (-nomis/outline-command
      nil
    (-nomis/outline-prev-or-next-heading ensure-visible-lineage-spec
                                         n
                                         :forward
                                         :same-level-allow-cross-parent)))

(defun nomis/outline-step-forward (n)
  "Move forward to the N'th heading at same level as this one, then show
fat parents and all children.
Stop at the first and last headings of a superior heading."
  (interactive "p")
  (-nomis/outline-command
      nil
    (-nomis/outline-prev-or-next-heading step-lineage-spec
                                         n
                                         :forward
                                         :sibling)))

(defun nomis/outline-step-forward/allow-cross-parent (n)
  "Move forward to the N'th heading at same level as this one, then show
fat parents and all children.
Can pass by a superior heading."
  (interactive "p")
  (-nomis/outline-command
      nil
    (-nomis/outline-prev-or-next-heading step-lineage-spec
                                         n
                                         :forward
                                         :same-level-allow-cross-parent)))

;;; End

(provide 'nomis-outline-uber)
