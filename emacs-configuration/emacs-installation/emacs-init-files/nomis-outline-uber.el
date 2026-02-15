;;; nomis-outline-uber -- -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'dash)

;;; To dos

;; TODO: For things that can navigate to invisible headings, implement wrappers
;;       that make curent heading visible. (Is `outline-show-entry` the best
;;       thing for this? Can you show all parents of current heading without
;;       changing anything else?)

;; TODO: Sibling navigation when not on a heading is broken.

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

(defun -nomis/outline-on-heading? ()
  (outline-on-heading-p t))

(defun -nomis/outline-on-visible-heading? () ; TODO: Unused.
  (outline-on-heading-p))

(defun -nomis/outline-back-to-heading? ()
  (outline-back-to-heading t))

(defun -nomis/outline-back-to-visible-heading? () ; TODO: Unused.
  (outline-back-to-heading))

(defun -nomis/outline-on-top-level-heading? ()
  "Are we on a top-level heading?"
  ;; `(outline-level)` and `(funcall outline-level)` return weird numbers in
  ;; some modes. This, we hope, is bulletproof.
  (save-excursion
    (when (-nomis/outline-on-heading?)
      (let* ((opoint (point))
             (olevel (funcall outline-level)))
        (ignore-errors (outline-up-heading 1 t))
        (or (not (-nomis/outline-on-heading?)) ; blank lines at top of file?
            (= olevel (funcall outline-level)))))))

(defun -nomis/outline-top-level-level ()
  (assert (-nomis/outline-on-heading?))
  (save-excursion
    (beginning-of-buffer)
    (unless (-nomis/outline-on-heading?) (outline-next-heading))
    (funcall outline-level)))

(defun -nomis/show-children ()
  ;; The `1` is important; otherwise we get bodies of children.
  (outline-show-children 1))

(defun -nomis/outline-show-tree* (level &optional no-pulse?)
  (let* ((parent-points
          (let* ((ps '()))
            (save-excursion
              (while (and (-nomis/outline-on-heading?)
                          (not (-nomis/outline-on-top-level-heading?)))
                (outline-up-heading 1)
                (push (point) ps)))
            ps)))
    (save-excursion
      (cl-case 2
        (1 (outline-hide-other))
        (2 (outline-hide-sublevels (-nomis/outline-top-level-level))))
      (save-excursion
        (cl-loop for p in parent-points
                 do (progn (goto-char p)
                           (outline-show-entry)
                           (outline-hide-body)
                           (-nomis/show-children))))))
  (recenter-top-bottom -1)
  (cl-ecase level
    (0 nil)
    (1 (-nomis/show-children))
    (2 (outline-show-branches))
    (3 (outline-show-subtree)
       (unless no-pulse?
         (-nomis/outline-pulse-current-section)))))

(defvar *-nomis/outline-show-tree-n-child-levels*)

(defun nomis/outline-show-tree-and-increments ()
  (interactive)
  ;; Repeated invocations cycle amount of child stuff.
  (let* ((level (if (not (eq this-command (-nomis/outline-last-command)))
                    0
                  (mod (1+ *-nomis/outline-show-tree-n-child-levels*)
                       4))))
    (setq *-nomis/outline-show-tree-n-child-levels* level)
    (-nomis/outline-show-tree* level)
    (cl-ecase level
      (0 (message "FOLDED"))
      (1 (message "CHILDREN"))
      (2 (message "BRANCHES"))
      (3 (message "SUBTREE")))))

(defun nomis/outline-show-tree-and-subtree ()
  (interactive)
  (-nomis/outline-show-tree* 3 t))

(defun -nomis/outline-show-context (show-context-approach)
  (cl-ecase show-context-approach
    (:show-entry (outline-show-entry))
    (:show-fat-parents-and-subtree (-nomis/outline-show-tree* 3 t))))

;;;; Previous helpers

(defun -nomis/outline-previous-same-level (allow-cross-parent?)
  (let* ((opoint (point))
         (level (funcall outline-level))
         (npoint (save-excursion
                   ;; The logic here is a copy-and-edit of
                   ;; `outline-get-last-sibling`.
                   (outline-previous-heading)
                   (when (and (/= (point) opoint) (outline-on-heading-p t))
                     (while (and (funcall (if allow-cross-parent? #'/= #'>)
                                          (funcall outline-level)
                                          level)
                                 (not (bobp)))
                       (outline-previous-heading))
                     (if (< (funcall outline-level) level)
                         nil
                       (cl-assert (= level (funcall outline-level)))
                       (point))))))
    (when npoint
      (goto-char npoint))))

(defun -nomis/outline-previous-heading-pos (start kind)
  (when start
    (save-excursion
      (goto-char start)
      (if (not (and (-nomis/outline-on-heading?)
                    (bolp)))
          (progn
            (-nomis/outline-back-to-heading?)
            (point))
        (cl-ecase kind
          (:any-level
           (outline-previous-heading))
          (:sibling
           (-nomis/outline-previous-same-level nil))
          (:same-level-allow-cross-parent
           (-nomis/outline-previous-same-level t)))
        (when (and (/= (point) start)
                   (-nomis/outline-on-heading?))
          ;; ^^ Check of `(-nomis/outline-on-heading?)` needed because
          ;;    `outline-previous-heading` goes to BOF when there's no
          ;;    previous heading.
          (point))))))

(defun -nomis/outline-previous-heading* (n kind show-context-approach)
  (when-let ((pos (->> (-iterate (lambda (start)
                                   (-nomis/outline-previous-heading-pos
                                    start
                                    kind))
                                 (point)
                                 (1+ n))
                       cl-rest
                       (-drop (1- n))
                       cl-first)))
    (goto-char pos)
    (-nomis/outline-show-context show-context-approach)
    pos))

;;;; Next helpers

(defun -nomis/outline-next-same-level (allow-cross-parent?)
  (let* ((level (funcall outline-level))
         (npoint  (save-excursion
                    ;; The logic here is a copy-and-edit of
                    ;; `outline-get-next-sibling`.
                    (outline-next-heading)
                    (while (and (not (eobp))
                                (funcall (if allow-cross-parent? #'/= #'>)
                                         (funcall outline-level)
                                         level))
                      (outline-next-heading))
                    (if (or (eobp) (< (funcall outline-level) level))
                        nil
                      (cl-assert (= level (funcall outline-level)))
                      (point)))))
    (when npoint
      (goto-char npoint))))

(defun -nomis/outline-next-heading-pos (start kind)
  (when start
    (save-excursion
      (goto-char start)
      (unless (and (-nomis/outline-on-heading?)
                   (bolp))
        (-nomis/outline-back-to-heading?)
        (point))
      (cl-ecase kind
        (:any-level
         (outline-next-heading))
        (:sibling
         (-nomis/outline-next-same-level nil))
        (:same-level-allow-cross-parent
         (-nomis/outline-next-same-level t)))
      (when (and (/= (point) start)
                 (-nomis/outline-on-heading?))
        ;; ^^ Check of `(-nomis/outline-on-heading?)` needed because
        ;;    `outline-next-heading` goes to EOF when there's no next heading.
        (point)))))

(defun -nomis/outline-next-heading* (n kind show-context-approach)
  (when-let ((pos (->> (-iterate (lambda (start)
                                   (-nomis/outline-next-heading-pos
                                    start
                                    kind))
                                 (point)
                                 (1+ n))
                       cl-rest
                       (-drop (1- n))
                       cl-first)))
    (goto-char pos)
    (-nomis/outline-show-context show-context-approach)
    pos))

;;; API

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
  (or (-nomis/outline-previous-heading* n
                                        :any-level
                                        :show-entry)
      (error (if (= n 1)
                 "No previous heading"
               (concat "No " (-nomis/outline-ordinal n) "-previous heading")))))

(defun nomis/outline-previous-sibling (n)
  "Move backward to the N'th heading at same level as this one.
Stop at the first and last headings of a superior heading."
  (interactive "p")
  (or (-nomis/outline-previous-heading* n
                                        :sibling
                                        :show-entry)
      (error (if (= n 1)
                 "No previous sibling"
               (concat "No " (-nomis/outline-ordinal n) "-previous sibling")))))

(defun nomis/outline-previous-sibling/allow-cross-parent (n)
  "Move backward to the N'th heading at same level as this one.
Can pass by a superior heading."
  (interactive "p")
  (or (-nomis/outline-previous-heading* n
                                        :same-level-allow-cross-parent
                                        :show-entry)
      (error (if (= n 1)
                 "No previous same-level"
               (concat "No " (-nomis/outline-ordinal n) "-previous same-level")))))

(defun nomis/outline-step-backward (n)
  "Move backward to the N'th heading at same level as this one and run
`outline-hide-other`.
Stop at the first and last headings of a superior heading."
  (interactive "p")
  (or (-nomis/outline-previous-heading* n
                                        :sibling
                                        :show-fat-parents-and-subtree)
      (error (if (= n 1)
                 "No previous sibling"
               (concat "No " (-nomis/outline-ordinal n) "-previous sibling")))))

(defun nomis/outline-step-backward/allow-cross-parent (n)
  "Move backward to the N'th heading at same level as this one and run
`outline-hide-other`.
Can pass by a superior heading."
  (interactive "p")
  (or (-nomis/outline-previous-heading* n
                                        :same-level-allow-cross-parent
                                        :show-fat-parents-and-subtree)
      (error (if (= n 1)
                 "No previous same-level"
               (concat "No " (-nomis/outline-ordinal n) "-previous same-level")))))

;;;; Next

(defun nomis/outline-next-heading (n)
  (interactive "p")
  (or (-nomis/outline-next-heading* n
                                    :any-level
                                    :show-entry)
      (error (if (= n 1)
                 "No next heading"
               (concat "No " (-nomis/outline-ordinal n) "-next heading")))))

(defun nomis/outline-next-sibling (n)
  "Move forward to the N'th heading at same level as this one.
Stop at the first and last headings of a superior heading."
  (interactive "p")
  (or (-nomis/outline-next-heading* n
                                    :sibling
                                    :show-entry)
      (error (if (= n 1)
                 "No next sibling"
               (concat "No " (-nomis/outline-ordinal n) "-next sibling")))))

(defun nomis/outline-next-sibling/allow-cross-parent (n)
  "Move forward to the N'th heading at same level as this one.
Can pass by a superior heading."
  (interactive "p")
  (or (-nomis/outline-next-heading* n
                                    :same-level-allow-cross-parent
                                    :show-entry)
      (error (if (= n 1)
                 "No next same-level"
               (concat "No " (-nomis/outline-ordinal n) "-next same-level")))))

(defun nomis/outline-step-forward (n)
  "Move forward to the N'th heading at same level as this one and run
`outline-hide-other`.
Stop at the first and last headings of a superior heading."
  (interactive "p")
  (or (-nomis/outline-next-heading* n
                                    :sibling
                                    :show-fat-parents-and-subtree)
      (error (if (= n 1)
                 "No next sibling"
               (concat "No " (-nomis/outline-ordinal n) "-next sibling")))))

(defun nomis/outline-step-forward/allow-cross-parent (n)
  "Move forward to the N'th heading at same level as this one and run
`outline-hide-other`.
Can pass by a superior heading."
  (interactive "p")
  (or (-nomis/outline-next-heading* n
                                    :same-level-allow-cross-parent
                                    :show-fat-parents-and-subtree)
      (error (if (= n 1)
                 "No next same-level"
               (concat "No " (-nomis/outline-ordinal n) "-next same-level")))))

;;; End

(provide 'nomis-outline-uber)
