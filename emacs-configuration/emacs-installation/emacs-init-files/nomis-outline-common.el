;;; nomis-outline-common.el --- Outline wrappers we can use for outline and org -*- lexical-binding: t; -*-

;;; Code:

;;;; Requires

(require 'cl-lib)
(require 'cl-format)
(require 'dash)
(require 'nomis-popup)
(require 'org)
(require 'outline)

;;;; nomis/outline/c/mode

(defun -nomis/outline/c/org-mode? ()
  (derived-mode-p 'org-mode))

(defun -nomis/outline/c/outline-mode? ()
  (and (not (-nomis/outline/c/org-mode?))
       (or (derived-mode-p 'outline-mode)
           outline-minor-mode)))

(defun nomis/outline/c/mode ()
  (cond ((-nomis/outline/c/outline-mode?)
         :outline)
        ((-nomis/outline/c/org-mode?)
         :org)
        (t
         (error "Unexpected: None of outline-mode, outline-minor-mode or org-mode is active"))))

;;;; Misc

(defun nomis/outline/c/last-command ()
  (or (bound-and-true-p *nomis/smex/last-command*)
      last-command))

(defun -nomis/outline/c/ordinal (n)
  (cl-format nil "~a~a"
             n
             (let ((x (cl-format nil "~:r" n)))
               (cl-subseq x (- (length x) 2)))))

;;;; Infinity

(defconst nomis/outline/c/plus-infinity   1.0e+INF)
(defconst nomis/outline/c/minus-infinity -1.0e+INF)

;;;; Simple outline and org wrappers

;; TODO: When ready, check that the only uses of `org-xxxx` and `outline-xxxx`
;;       are here.

(defalias 'nomis/outline/c/next-heading     'outline-next-heading)
(defalias 'nomis/outline/c/end-of-heading   'outline-end-of-heading)
(defalias 'nomis/outline/c/next-preface     'outline-next-preface)
(defalias 'nomis/outline/c/previous-heading 'outline-previous-heading)
(defalias 'nomis/outline/c/show-entry       'outline-show-entry)
(defalias 'nomis/outline/c/hide-entry       'outline-hide-entry)

(defun nomis/outline/c/collapse ()
  (outline-hide-subtree))

(defun nomis/outline/c/invisible? (&optional pos)
  (let* ((pos (or pos (point))))
    (cl-ecase (nomis/outline/c/mode)
      (:outline (outline-invisible-p pos))
      (:org     (org-invisible-p pos t) ; TODO: Is this `folding-only` arg right?
                ))))

(defun nomis/outline/c/visible? (&optional pos)
  (not (nomis/outline/c/invisible? pos)))

(defun nomis/outline/c/on-heading? ()
  (outline-on-heading-p t))

(defun -nomis/outline/c/on-visible-heading? ()
  (outline-on-heading-p))

(defun nomis/outline/c/back-to-heading ()
  (outline-back-to-heading t))

(defun -nomis/outline/c/back-to-visible-heading? ()
  (outline-back-to-heading))

(defun nomis/outline/c/before-first-heading? ()
  (save-excursion
    (condition-case nil
        (progn (nomis/outline/c/back-to-heading)
               nil)
      (error t))))

(defun -nomis/outline/c/at-beginning-of-heading? ()
  (and (bolp)
       (nomis/outline/c/on-heading?)))

(defun nomis/outline/c/end-of-line ()
  (cl-ecase (nomis/outline/c/mode)
    (:outline (end-of-line))
    (:org     (org-end-of-line))))

(defun nomis/outline/c/level (&optional inc-if-in-body?)
  (let* ((v (save-excursion (nomis/outline/c/back-to-heading)
                            (funcall outline-level))))
    (if (and inc-if-in-body?
             (not (nomis/outline/c/on-heading?)))
        (1+ v)
      v)))

(defun nomis/outline/c/up-heading (n
                                   &optional
                                   no-error-if-before-first-heading?
                                   fewer-ok?)
  ;; `outline-up-heading` behaves differently in `outline` and `org`.
  ;; This gives us the same behaviour in both.
  "Move to the heading line N levels above the present line.

If point is not at the beginning of a heading, moving to the beginning of
the heading counts as 1 of the N.

Return new point if we moved, nil otherwise.

Signal an error if point is before the first heading, unless
NO-ERROR-IF-BEFORE-FIRST-HEADING? is truthy.

Signal an error if there are not N levels of parent, unless
FEWER-OK? is truthy."
  (cl-assert (>= n 0))
  (if (nomis/outline/c/before-first-heading?)
      (unless no-error-if-before-first-heading?
        (error "Before first heading"))
    (let* ((opoint (point))
           (n (if (-nomis/outline/c/at-beginning-of-heading?) n (1- n)))
           (npoint
            (save-excursion
              (nomis/outline/c/back-to-heading)
              (cl-loop
               for i from 1 to n
               for opoint2 = (point)
               for olevel = (nomis/outline/c/level)
               do (ignore-errors (outline-up-heading 1 t))
               for no-can-do? = (or (not (nomis/outline/c/on-heading?))
                                    (= olevel (nomis/outline/c/level)))
               when (and no-can-do? fewer-ok?)
               return opoint2
               when no-can-do?
               do (if (= i 1)
                      (error "Already at top level")
                    (error "There are only %s parent levels, but needed %s"
                           (1- i)
                           n))
               finally return (point)))))
      (if (= opoint npoint)
          nil
        (goto-char npoint)
        npoint))))

(defun -nomis/outline/c/on-top-level-heading? ()
  "Are we on a top-level heading?"
  (save-excursion
    (when (nomis/outline/c/on-heading?)
      (beginning-of-line)
      (not (nomis/outline/c/up-heading 1 t t)))))

(defun nomis/outline/c/top-level-level ()
  (save-excursion
    (goto-char (point-min))
    (unless (nomis/outline/c/on-heading?) (outline-next-heading))
    (nomis/outline/c/level)))

(defun nomis/outline/c/ensure-heading-shown ()
  (interactive)
  (when (nomis/outline/c/invisible?)
    ;; Is there a simpler way to show the heading but not the body?
    (outline-show-entry)
    (outline-hide-entry)))

(defun nomis/outline/c/show-children (n)
  (outline-show-children n))

(defun -nomis/outline/c/prev-or-next (direction)
  (cl-ecase direction
    (:backward (outline-previous-heading))
    (:forward (outline-next-heading))))

(defun nomis/outline/c/map-tree (fun)
  ;; Copy-and-edit of `org-map-tree`.
  "Call FUN for the current heading and all headings underneath it."
  (nomis/outline/c/back-to-heading)
  (let ((level (nomis/outline/c/level)))
    (save-excursion
      (funcall fun)
      (while (and (progn
		    (nomis/outline/c/next-heading)
		    (> (nomis/outline/c/level) level))
		  (not (eobp)))
	(funcall fun)))))

;;;; nomis/outline/c/pulse-current-section

(defun nomis/outline/c/pulse-current-section ()
  (save-excursion
    (if (nomis/outline/c/before-first-heading?)
        (let* ((end (nomis/outline/c/next-heading)))
          (pulse-momentary-highlight-region (point-min) end))
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
            (pulse-momentary-highlight-region start end)))))))

;;;; Previous/next helpers

(defun -nomis/outline/c/prev-next-same-level (direction sibling-or-peer)
  (let* ((opoint (point))
         (level (nomis/outline/c/level))
         (npoint  (save-excursion
                    ;; The logic here is a copy-and-edit of
                    ;; `outline-get-last-sibling` and
                    ;; `outline-get-next-sibling`.
                    (-nomis/outline/c/prev-or-next direction)
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
                                           (nomis/outline/c/level)
                                           level)
                                  (cl-ecase direction
                                    (:backward (not (bobp)))
                                    (:forward t)))
                        (-nomis/outline/c/prev-or-next direction))
                      (if (or (cl-ecase direction
                                (:backward nil)
                                (:forward (eobp)))
                              (< (nomis/outline/c/level) level))
                          nil
                        (cl-assert (= level (nomis/outline/c/level)))
                        (point))))))
    (when npoint
      (goto-char npoint))))

(defun -nomis/outline/c/prev-or-next-heading-pos (start
                                                  direction
                                                  kind)
  (when start
    (save-excursion
      (goto-char start)
      (let* ((boh? (-nomis/outline/c/at-beginning-of-heading?)))
        (if (and (eq direction :backward)
                 (not boh?))
            (progn
              (nomis/outline/c/back-to-heading)
              (point))
          (when (and (eq direction :forward)
                     (not boh?))
            (nomis/outline/c/back-to-heading))
          (cl-ecase kind
            (:any-level
             (-nomis/outline/c/prev-or-next direction))
            (:sibling
             (-nomis/outline/c/prev-next-same-level direction :sibling))
            (:peer
             (-nomis/outline/c/prev-next-same-level direction :peer)))
          (when (and (/= (point) start)
                     (nomis/outline/c/on-heading?))
            ;; ^^ Check of `(nomis/outline/c/on-heading?)` needed because
            ;;    `-nomis/outline/c/prev-or-next` goes to BOF or EOF when there's
            ;;    no prev/next heading.
            (point)))))))

(defun nomis/outline/c/prev-or-next-heading (n
                                             direction
                                             kind
                                             &optional
                                             ;; TODO: I don't like this.
                                             ;;       Refactor and make outline
                                             ;;       and org callers consistent.
                                             no-msg?)
  "Go to the N'th-next heading of kind KIND in direction DIRECTION.
If such a heading exists, return `t`.
If no such heading exists, return `nil', leave point unchanged and
display a popup message.
KIND is one of `:sibling`, `:peer` and `:any-level`.
DIRECTION is one or `:forward` and `:backward`."
  (let* ((pos (->> (-iterate (lambda (start)
                               (-nomis/outline/c/prev-or-next-heading-pos
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
          t)
      (unless no-msg?
        (let* ((direction-word (cl-ecase direction
                                 (:backward "previous")
                                 (:forward "next")))
               (kind-word (cl-ecase kind
                            (:any-level "heading")
                            (:sibling "sibling")
                            (:peer "same-level"))))
          (nomis/popup/error-message
           "No %s%s %s"
           (if (= n 1) "" (concat (-nomis/outline/c/ordinal n)
                                  "-"))
           direction-word
           kind-word)))
      nil)))

;;; End

(provide 'nomis-outline-common)
