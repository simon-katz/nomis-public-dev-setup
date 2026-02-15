;;; nomis-outline-uber -- -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'dash)
(require 'nomis-popup)

;;; To dos

;; TODO: For things that can navigate to invisible headings, implement wrappers
;;       that make curent heading visible. (Is `outline-show-entry` the best
;;       thing for this? Can you show all parents of current heading without
;;       changing anything else?)

;; TODO: Bug: Sibling navigation when not on a heading is broken.

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

(defun -nomis/outline-at-beginning-of-heading? ()
  (and (bolp)
       (-nomis/outline-on-heading?)))

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

(defun -nomis/outline-prev-or-next (direction)
  (cl-ecase direction
    (:backward (outline-previous-heading))
    (:forward (outline-next-heading))))

;;;; More substantial things

(defun -nomis/outline-show-fat-tree** (n-child-levels no-pulse?)
  (let* ((parent-points
          (let* ((ps '()))
            (save-excursion
              (while (and (-nomis/outline-on-heading?)
                          (not (-nomis/outline-on-top-level-heading?)))
                (outline-up-heading 1)
                (push (point) ps)))
            ps)))
    (save-excursion
      (outline-hide-sublevels (-nomis/outline-top-level-level))
      (save-excursion
        (cl-loop for p in parent-points
                 do (progn (goto-char p)
                           (outline-show-entry)
                           (outline-hide-body)
                           (-nomis/show-children))))))
  (recenter-top-bottom -1)
  (cl-ecase n-child-levels
    (0 nil)
    (1 (-nomis/show-children))
    (2 (outline-show-branches))
    (3 (outline-show-subtree)
       (unless no-pulse?
         (-nomis/outline-pulse-current-section)))))

(defun -nomis/outline-show-fat-tree* (n-child-levels no-pulse?)
  (cl-ecase 2
    (1
     ;; After cross-parent stepping, this expands things more than it should
     ;; -- all siblings or the current heading are expanded too. I don't
     ;; understand why. So we don't do this.
     (-nomis/outline-show-fat-tree**))
    (2
     ;; Hackily get around the above problem...
     ;;
     ;; Ensure point is visible, otherwise point is in a different place
     ;; when we run `-nomis/outline-show-fat-tree**`.
     (outline-show-entry)
     ;; Do the thing we want to do.
     (run-at-time 0 nil #'(lambda ()
                            (-nomis/outline-show-fat-tree** n-child-levels
                                                            no-pulse?))))))

(defun -nomis/outline-show-context (show-context-approach)
  (cl-ecase show-context-approach
    (:show-entry (outline-show-entry))
    (:show-fat-parents-and-subtree (-nomis/outline-show-fat-tree* 3 t))))

(defun -nomis/outline-command* (f)
  (push-mark)
  (funcall f))

(cl-defmacro -nomis/outline-command (_opts &body body)
  (declare (indent 1))
  `(-nomis/outline-command* (lambda () ,@body) ))

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

(defun -nomis/outline-prev-or-next-heading-pos (start direction kind)
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

(defun -nomis/outline-prev-or-next-heading (n
                                            direction
                                            kind
                                            show-context-approach)
  (let* ((pos (->> (-iterate (lambda (start)
                               (-nomis/outline-prev-or-next-heading-pos
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
          (-nomis/outline-show-context show-context-approach))
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

;;; API

;;;; nomis/outline-show-fat-tree-and-increments

(defvar *-nomis/outline-show-fat-tree-n-child-levels*)

(defun nomis/outline-show-fat-tree-and-increments ()
  (interactive)
  ;; Repeated invocations cycle amount of child stuff.
  (let* ((level (if (not (eq this-command (-nomis/outline-last-command)))
                    0
                  (mod (1+ *-nomis/outline-show-fat-tree-n-child-levels*)
                       4))))
    (setq *-nomis/outline-show-fat-tree-n-child-levels* level)
    (-nomis/outline-show-fat-tree* level nil)
    (cl-ecase level
      (0 (nomis/popup/message "Folded"))
      (1 (nomis/popup/message "Children"))
      (2 (nomis/popup/message "Branches"))
      (3 (nomis/popup/message "Subtree")))))

;;;; nomis/outline-show-fat-tree-and-subtree

(defun nomis/outline-show-fat-tree-and-subtree ()
  (interactive)
  (-nomis/outline-show-fat-tree* 3 t))

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
    (-nomis/outline-prev-or-next-heading n
                                         :backward
                                         :any-level
                                         :show-entry)))

(defun nomis/outline-previous-sibling (n)
  "Move backward to the N'th heading at same level as this one.
Stop at the first and last headings of a superior heading."
  (interactive "p")
  (-nomis/outline-command
      nil
    (-nomis/outline-prev-or-next-heading n
                                         :backward
                                         :sibling
                                         :show-entry)))

(defun nomis/outline-previous-sibling/allow-cross-parent (n)
  "Move backward to the N'th heading at same level as this one.
Can pass by a superior heading."
  (interactive "p")
  (-nomis/outline-command
      nil
    (-nomis/outline-prev-or-next-heading n
                                         :backward
                                         :same-level-allow-cross-parent
                                         :show-entry)))

(defun nomis/outline-step-backward (n)
  "Move backward to the N'th heading at same level as this one and run
`outline-hide-other`.
Stop at the first and last headings of a superior heading."
  (interactive "p")
  (-nomis/outline-command
      nil
    (-nomis/outline-prev-or-next-heading n
                                         :backward
                                         :sibling
                                         :show-fat-parents-and-subtree)))

(defun nomis/outline-step-backward/allow-cross-parent (n)
  "Move backward to the N'th heading at same level as this one and run
`outline-hide-other`.
Can pass by a superior heading."
  (interactive "p")
  (-nomis/outline-command
      nil
    (-nomis/outline-prev-or-next-heading n
                                         :backward
                                         :same-level-allow-cross-parent
                                         :show-fat-parents-and-subtree)))

;;;; Next

(defun nomis/outline-next-heading (n)
  (interactive "p")
  (-nomis/outline-command
      nil
    (-nomis/outline-prev-or-next-heading n
                                         :forward
                                         :any-level
                                         :show-entry)))

(defun nomis/outline-next-sibling (n)
  "Move forward to the N'th heading at same level as this one.
Stop at the first and last headings of a superior heading."
  (interactive "p")
  (-nomis/outline-command
      nil
    (-nomis/outline-prev-or-next-heading n
                                         :forward
                                         :sibling
                                         :show-entry)))

(defun nomis/outline-next-sibling/allow-cross-parent (n)
  "Move forward to the N'th heading at same level as this one.
Can pass by a superior heading."
  (interactive "p")
  (-nomis/outline-command
      nil
    (-nomis/outline-prev-or-next-heading n
                                         :forward
                                         :same-level-allow-cross-parent
                                         :show-entry)))

(defun nomis/outline-step-forward (n)
  "Move forward to the N'th heading at same level as this one and run
`outline-hide-other`.
Stop at the first and last headings of a superior heading."
  (interactive "p")
  (-nomis/outline-command
      nil
    (-nomis/outline-prev-or-next-heading n
                                         :forward
                                         :sibling
                                         :show-fat-parents-and-subtree)))

(defun nomis/outline-step-forward/allow-cross-parent (n)
  "Move forward to the N'th heading at same level as this one and run
`outline-hide-other`.
Can pass by a superior heading."
  (interactive "p")
  (-nomis/outline-command
      nil
    (-nomis/outline-prev-or-next-heading n
                                         :forward
                                         :same-level-allow-cross-parent
                                         :show-fat-parents-and-subtree)))

;;; End

(provide 'nomis-outline-uber)
