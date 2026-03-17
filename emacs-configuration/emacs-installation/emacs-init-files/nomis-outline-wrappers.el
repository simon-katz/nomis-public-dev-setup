;;; nomis-outline-wrappers.el --- Outline wrappers we can use for outline and org -*- lexical-binding: t; -*-

;;; Code:

;;;; Requires

(require 'cl-lib)
(require 'cl-format)
(require 'dash)
(require 'nomis-popup)
(require 'org)
(require 'outline)

;;;; nomis/outline/w/mode

(defun -nomis/outline/w/org-mode? ()
  (derived-mode-p 'org-mode))

(defun -nomis/outline/w/outline-mode? ()
  (and (not (-nomis/outline/w/org-mode?))
       (or (derived-mode-p 'outline-mode)
           outline-minor-mode)))

(defun nomis/outline/w/mode ()
  (cond ((-nomis/outline/w/outline-mode?)
         :outline)
        ((-nomis/outline/w/org-mode?)
         :org)
        (t
         (error "Unexpected: None of outline-mode, outline-minor-mode or org-mode is active"))))

;;;; Misc

(defun nomis/outline/w/last-command ()
  (or (bound-and-true-p *nomis/smex/last-command*)
      last-command))

(defun -nomis/outline/w/ordinal (n)
  (cl-format nil "~a~a"
             n
             (let ((x (cl-format nil "~:r" n)))
               (cl-subseq x (- (length x) 2)))))

;;;; Infinity

(defconst nomis/outline/w/plus-infinity   1.0e+INF)
(defconst nomis/outline/w/minus-infinity -1.0e+INF)

;;;; Simple outline and org wrappers

(defalias 'nomis/outline/w/next-heading     'outline-next-heading)
(defalias 'nomis/outline/w/end-of-heading   'outline-end-of-heading)
(defalias 'nomis/outline/w/next-preface     'outline-next-preface)
(defalias 'nomis/outline/w/previous-heading 'outline-previous-heading)
(defalias 'nomis/outline/w/show-entry       'outline-show-entry)
(defalias 'nomis/outline/w/hide-entry       'outline-hide-entry)

(defun nomis/outline/w/show-heading ()
  (save-excursion
    (outline-show-heading)))

(defun nomis/outline/w/collapse ()
  (outline-hide-subtree))

(defun nomis/outline/w/invisible? (&optional pos)
  (let* ((pos (or pos (point))))
    (cl-ecase (nomis/outline/w/mode)
      (:outline (outline-invisible-p pos))
      (:org     (org-invisible-p pos t)))))

(defun nomis/outline/w/on-heading? ()
  (outline-on-heading-p t))

(defun -nomis/outline/w/on-visible-heading? ()
  (outline-on-heading-p))

(defun nomis/outline/w/back-to-heading ()
  (outline-back-to-heading t))

(defun -nomis/outline/w/has-body?/boh ()
  (save-excursion
    (/= (progn (nomis/outline/w/end-of-heading)
               (point))
        (progn (nomis/outline/w/next-preface)
               (point)))))

(defun -nomis/outline/w/back-to-visible-heading? ()
  (outline-back-to-heading))

(defun nomis/outline/w/before-first-heading? ()
  (save-excursion
    (condition-case nil
        (progn (nomis/outline/w/back-to-heading)
               nil)
      (error t))))

(defun nomis/outline/w/at-beginning-of-heading? ()
  (and (bolp)
       (nomis/outline/w/on-heading?)))

(defun nomis/outline/w/end-of-line ()
  (cl-ecase (nomis/outline/w/mode)
    (:outline (end-of-line))
    (:org     (org-end-of-line))))

(defun nomis/outline/w/level/boh ()
  (cl-assert (nomis/outline/w/at-beginning-of-heading?))
  (funcall outline-level))

(defun nomis/outline/w/level/no-inc-if-in-body ()
  (save-excursion (nomis/outline/w/back-to-heading)
                  (nomis/outline/w/level/boh)))

(defun nomis/outline/w/level/inc-if-in-body ()
  (let* ((v (nomis/outline/w/level/no-inc-if-in-body)))
    (if (not (nomis/outline/w/on-heading?))
        (1+ v)
      v)))

(defun nomis/outline/w/up-heading* (n
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
  (if (nomis/outline/w/before-first-heading?)
      (unless no-error-if-before-first-heading?
        (error "Before first heading"))
    (let* ((opoint (point))
           (n (if (nomis/outline/w/at-beginning-of-heading?) n (1- n)))
           (npoint
            (save-excursion
              (nomis/outline/w/back-to-heading)
              (cl-loop
               for i from 1 to n
               for opoint2 = (point)
               for olevel = (nomis/outline/w/level/boh)
               do (ignore-errors (outline-up-heading 1 t))
               for no-can-do? = (or (not (nomis/outline/w/on-heading?))
                                    (= olevel (nomis/outline/w/level/boh)))
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

(defun nomis/outline/w/up-heading (n)
  (nomis/outline/w/up-heading* n))

(defun -nomis/outline/w/on-top-level-heading? ()
  "Are we on a top-level heading?"
  (save-excursion
    (when (nomis/outline/w/on-heading?)
      (beginning-of-line)
      (not (nomis/outline/w/up-heading* 1 t t)))))

(defun nomis/outline/w/top-level-level ()
  (save-excursion
    (goto-char (point-min))
    (unless (nomis/outline/w/on-heading?) (outline-next-heading))
    (nomis/outline/w/level/boh)))

(defun nomis/outline/w/ensure-heading-shown ()
  (interactive)
  (when (nomis/outline/w/invisible?)
    ;; Is there a simpler way to show the heading but not the body?
    (outline-show-entry)
    (outline-hide-entry)))

(defun nomis/outline/w/hide-bodies ()
  "Hide the bodies of all headings."
  (outline-hide-body) ; misnomer -- applies to all headings
  )

(defun nomis/outline/w/show-bodies ()
  "Show the bodies of all currently-visible headings."
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (when (-nomis/outline/w/on-visible-heading?)
        (nomis/outline/w/show-entry))
      (nomis/outline/w/next-heading))))

(defun nomis/outline/w/show-children (n)
  (outline-show-children n))

(defun -nomis/outline/w/prev-or-next (direction)
  (cl-ecase direction
    (:backward (outline-previous-heading))
    (:forward (outline-next-heading))))

(defun nomis/outline/w/map-tree (fun)
  ;; Copy-and-edit of `org-map-tree`.
  "Call FUN for the current heading and all headings underneath it."
  (nomis/outline/w/back-to-heading)
  (let ((level (nomis/outline/w/level/boh)))
    (save-excursion
      (funcall fun)
      (while (and (progn
                    (nomis/outline/w/next-heading)
                    (and (nomis/outline/w/on-heading?) ; might be after last heading
                         (> (nomis/outline/w/level/boh) level)))
                  (not (eobp)))
        (funcall fun)))))

(defun nomis/outline/w/do-to-self-and-ancestors (f)
  "Call the function `f' on current heading, then parent heading, etc."
  (save-excursion
    (funcall f)
    (while (nomis/outline/w/up-heading* 1 t t)
      (funcall f))))

;;;; nomis/outline/w/pulse-current-section

(defun nomis/outline/w/pulse-current-section ()
  (save-excursion
    (if (nomis/outline/w/before-first-heading?)
        (let* ((end (nomis/outline/w/next-heading)))
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

(defun -nomis/outline/w/prev-next-same-level (direction sibling-or-peer)
  (let* ((opoint (point))
         (level (nomis/outline/w/level/boh))
         (npoint  (save-excursion
                    ;; The logic here is a copy-and-edit of
                    ;; `outline-get-last-sibling` and
                    ;; `outline-get-next-sibling`.
                    (-nomis/outline/w/prev-or-next direction)
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
                                           (nomis/outline/w/level/boh)
                                           level)
                                  (cl-ecase direction
                                    (:backward (not (bobp)))
                                    (:forward t)))
                        (-nomis/outline/w/prev-or-next direction))
                      (if (or (cl-ecase direction
                                (:backward nil)
                                (:forward (eobp)))
                              (< (nomis/outline/w/level/boh) level))
                          nil
                        (cl-assert (= level (nomis/outline/w/level/boh)))
                        (point))))))
    (when npoint
      (goto-char npoint))))

(defun -nomis/outline/w/prev-or-next-heading-pos (start
                                                  direction
                                                  kind)
  (when start
    (save-excursion
      (goto-char start)
      (let* ((boh? (nomis/outline/w/at-beginning-of-heading?)))
        (if (and (eq direction :backward)
                 (not boh?))
            (progn
              (nomis/outline/w/back-to-heading)
              (point))
          (when (and (eq direction :forward)
                     (not boh?))
            (nomis/outline/w/back-to-heading))
          (cl-ecase kind
            (:any-level
             (-nomis/outline/w/prev-or-next direction))
            (:sibling
             (-nomis/outline/w/prev-next-same-level direction :sibling))
            (:peer
             (-nomis/outline/w/prev-next-same-level direction :peer)))
          (when (and (/= (point) start)
                     (nomis/outline/w/on-heading?))
            ;; ^^ Check of `(nomis/outline/w/on-heading?)` needed because
            ;;    `-nomis/outline/w/prev-or-next` goes to BOF or EOF when there's
            ;;    no prev/next heading.
            (point)))))))

(defun nomis/outline/w/prev-or-next-heading (n
                                             direction
                                             kind
                                             &optional
                                             no-msg?)
  "Go to the N'th-next heading of kind KIND in direction DIRECTION.
If such a heading exists, return `t`.
If no such heading exists, return `nil', leave point unchanged and.
when NO-MSG? is nil, display a popup message.
KIND is one of `:sibling`, `:peer` and `:any-level`.
DIRECTION is one or `:forward` and `:backward`."
  (let* ((pos (->> (-iterate (lambda (start)
                               (-nomis/outline/w/prev-or-next-heading-pos
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
                            (:peer "peer"))))
          (nomis/popup/error-message
           "No %s%s %s"
           (if (= n 1) "" (concat (-nomis/outline/w/ordinal n)
                                  "-"))
           direction-word
           kind-word)))
      nil)))

;;; End

(provide 'nomis-outline-wrappers)
