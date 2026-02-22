;;; norg.el --- A layer on top of Org mode -*- lexical-binding: t -*-

;; Copyright (C) 2019-2021 Simon Katz.

;; Author: Simon Katz
;; Version: 0.0.1-SNAPSHOT
;; TODO: Add a contact details (or, probably, a GitHub URL).
;; TODO: Add Keywords
;; TODO: Add Package-Requires

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; TODO: When we make a repo for this, include the GNU General Public License
;;       mentioned above.

;;; Commentary:

;; A layer on top of org mode providing additional functionality for
;; navigating and for expanding & collapsing.

;; TODO: Write webby documentation and add a URL here.

;;; Code:

(progn) ; this stops `hs-hide-all` from hiding the next comment

;;;; ___________________________________________________________________________
;;;; ____ * Some rejected (at least for now) ideas

;; XXXX (Too hard! Park this for now.)
;;      Sometimes things take a long time and a busy pointer would be useful.
;;      Why don't you have one?
;;      Emacs is supposed to do this for you.
;;      Some investigation:
;;      - Tested with -Q option:
;;          /Applications/Emacs-26-1-2.app/Contents/MacOS/Emacs -Q
;;      - Same behaviour on two Macs.
;;      - There seem to be several problems:
;;        - On OS X, the pointer disappears when typing. You have to move
;;          the mouse to get it back. So even if you fixed this problem,
;;          you wouldn't have a busy cursor when busy unless you moved the
;;          pointer.
;;        - Busy pointer is an I-beam, not an hourglass.
;;        - Busy pointer doesn't appear as soon as it should.
;;        - Busy pointer sometimes sticks until you move the mouse.
;;      - Google not helping -- can't find any mention of this.

;; XXXX Idea of treating level -1 as show only parents, and not siblings.
;;      But first:
;;      - The visibility-span stuff is global, but you have things that work
;;        from point and from root. So you might have to roll your own to o
;;        this.
;;      - Can you detect a parents-not-siblings state? (Remember: you are
;;        no longer storing state for positions. (But you could in this
;;        special case, I guess. But that's getting complicated. It was nice
;;        to get rid of the state.))
;;      - Consider whether there's anything else you might want to do with
;;        visibility.
;;      - Sort out visibiity issues that are mentioned above:
;;        - Bodies not being expanded.
;;        - (Anything else?)

;; XXXX Bug: When a M-x is used to invoke a command (even something not
;;      org-related (such as `version` or `what-cursor-position)`, if the
;;      point is hidden it gets changed to be a visible point.
;;      To reproduce:
;;      - M-x what-cursor-position
;;      - Do something that hides point.
;;      - M-x what-cursor-position
;;        - Observe that point has not changed.
;;      - M-x what-cursor-position
;;        - Observe that point has changed.
;;      So, it seems that after running a M-x command, point gets changed.

;; XXXX When getting to 0 or max, first flash then cycle.
;;      REJECTED
;;      This would give a modal UI. An invisibly-modal UI.

;;;; ___________________________________________________________________________
;;;; ____ * Require things

(require 'org)
(require 'cl-lib)
(require 'dash)

;;;; Things that you might want to make into packages if you make norg into a
;;;; package.

(require 'nomis-popup)
(require 'nomis-scrolling)

;;;; ___________________________________________________________________________
;;;; ____ * Tailor other functionality

;;;; ____ ** last-command

(defun norg/last-command ()
  (or (bound-and-true-p *nomis/smex/last-command*)
      last-command))

;;;; ____ ** what-cursor-position
;;;; Add org level to the output of `what-cursor-position`.

(defvar norg/add-info-to-what-cursor-position?
  t
  "Control whether we add additional info to the message produced by
`what-cursor-position`. This is here just in case someone might be parsing the
message and in case adding org level messes things up.")

(defvar *norg/-in-what-cursor-position?* nil)

(advice-add 'what-cursor-position
            :around
            (lambda (orig-fun &rest args)
              (let* ((*norg/-in-what-cursor-position?* t))
                (apply orig-fun args)))
            '((name . norg/add-level-info)))

(advice-add 'message
            :around
            (lambda (orig-fun &rest args)
              (if (and norg/add-info-to-what-cursor-position?
                       (eq major-mode 'org-mode)
                       *norg/-in-what-cursor-position?*)
                  (let* ((format-string
                          (concat "org-level=%s  " (first args)))
                         (format-args
                          (append
                           (list (norg/current-level-or-error-string t))
                           (rest args)))
                         (s (apply #'format format-string format-args)))
                    (funcall orig-fun "%s" s)
                    (nomis/popup/message "%s" s))
                (apply orig-fun args)))
            '((name . norg/add-level-info)))

;;;; ___________________________________________________________________________
;;;; ____ * Infinity

(defconst norg/-plus-infinity   1.0e+INF)
(defconst norg/-minus-infinity -1.0e+INF)

;;;; ___________________________________________________________________________
;;;; ____ * Wrappers for `outline` and `org`

;;;; I'm not clear about the public API of `outline` and `org`, so let's
;;;; be safe.
;;;; Besides, it's useful to isolate how we use `outline` and `org`.

(defalias 'norg/w/end-of-line 'org-end-of-line)
(defalias 'norg/w/beginning-of-line 'org-beginning-of-line)

(defalias 'norg/w/at-heading-p 'org-at-heading-p)

(defun norg/w/level/must-be-at-boh ()
  "Point must be at the beginning of a headline.
Return the nesting depth of the headline in the outline."
  (funcall outline-level))

(defalias 'norg/w/next-heading 'outline-next-heading)
(defalias 'norg/w/end-of-heading 'outline-end-of-heading)
(defalias 'norg/w/next-preface 'outline-next-preface)
(defalias 'norg/w/forward-heading-same-level 'org-forward-heading-same-level)

(defalias 'norg/w/backward-heading-same-level 'org-backward-heading-same-level)
(defalias 'norg/w/back-to-heading 'org-back-to-heading)
(defalias 'norg/w/up-heading 'outline-up-heading)
(defalias 'norg/w/previous-heading 'outline-previous-heading)

(defalias 'norg/w/show-entry 'outline-show-entry)
(defalias 'norg/w/hide-entry 'outline-hide-entry)
(defalias 'norg/w/show-children 'outline-show-children) ; Not `org-show-children`, because that shows first level when n is 0
(defalias 'norg/w/cycle 'org-cycle)
(defalias 'norg/w/overview 'org-overview)
(defalias 'norg/w/show-set-visibility 'org-show-set-visibility)

(defalias 'norg/w/map-tree 'org-map-tree)

(defalias 'norg/w/invisible-p 'org-invisible-p)
(defalias 'norg/w/flag-subtree 'org-flag-subtree)

(defalias 'norg/w/check-before-invisible-edit 'org-check-before-invisible-edit)

(defvaralias 'norg/w/catch-invisible-edits 'org-catch-invisible-edits)

;;;; ___________________________________________________________________________
;;;; ____ * Whether to show bodies

(defvar norg/show-bodies? t)

(defun norg/toggle-show-bodies ()
  (interactive)
  (setq norg/show-bodies? (not norg/show-bodies?))
  (message "norg/show-bodies? set to %s" norg/show-bodies?))

;;;; ___________________________________________________________________________
;;;; ____ * Some wrappers for org functionality

;;;; Basic stuff

(defun norg/point-is-visible? ()
  (not (norg/w/invisible-p)))

(defun norg/-has-body?/must-be-at-boh/leaving-cursor-at-end-of-heading ()
  (let* ((_ (norg/w/end-of-heading))
         (end-of-heading-position (point))
         (_ (norg/w/next-preface))
         (end-of-preface-position (point))
         (has-body? (not (= end-of-heading-position
                            end-of-preface-position))))
    (prog1
        has-body?
      ;; Go to end of heading rather than end of preface.
      ;; Without this, if we have a link at the end of the body (and if links
      ;; are being displayed in the usual way so that the actual link text is
      ;; invisible), we don't know whether the body is being displayed.
      ;; This newline char being visible or not tells us what we want to know.
      (goto-char end-of-heading-position))))

(defun norg/-has-body?/must-be-at-boh ()
  (save-excursion
    (norg/-has-body?/must-be-at-boh/leaving-cursor-at-end-of-heading)))

(defun norg/-body-info ()
  (save-excursion
    (norg/w/back-to-heading t)
    (let* ((has-body?
            (norg/-has-body?/must-be-at-boh/leaving-cursor-at-end-of-heading))
           (has-visible-body? (and has-body?
                                   (not
                                    (norg/w/invisible-p (point)))))
           (has-invisible-body? (and has-body?
                                     (not has-visible-body?))))
      (list has-body?
            has-visible-body?
            has-invisible-body?))))

(defun norg/level-incl-body/must-be-at-boh ()
  (let* ((heading-level (norg/w/level/must-be-at-boh)))
    (+ heading-level
       (if (and norg/show-bodies?
                (norg/-has-body?/must-be-at-boh))
           1
         0))))

(defun norg/-in-body? ()
  (> (point)
     (save-excursion
       (norg/w/back-to-heading t)
       (norg/w/end-of-heading)
       (point))))

(defun norg/current-level (&optional inc-if-in-body?)
  "If in a heading or if `inc-if-in-body?` is falsey, return the nesting depth
of the current heading in the outline. Otherwise return one more than that
value."
  (+ (save-excursion
       (norg/w/back-to-heading t)
       (norg/w/level/must-be-at-boh))
     (if (and norg/show-bodies?
              inc-if-in-body?
              (norg/-in-body?))
         1
       0)))

(defun norg/current-level-or-error-string (&optional inc-if-in-body?)
  (condition-case err
      (norg/current-level inc-if-in-body?)
    (error (cdr err))))

(defun norg/goto-root ()
  (interactive)
  (while (ignore-errors (norg/w/up-heading 1))))

(cl-defmacro norg/save-excursion-to-root (&body body)
  (declare (indent 0))
  `(save-excursion
     (norg/goto-root)
     ,@body))

(defun norg/show-point ()
  (interactive)
  (cl-case 1
    (1
     (unless (norg/point-is-visible?)
       ;; Make point visible and leave subtree collapsed
       (dotimes (_ 3) (norg/w/cycle))))
    (2
     ;; This makes lots of stuff visible, but seems to be the "official" way.
     ;; Leave this here as a point of interest.
     (let ((norg/w/catch-invisible-edits 'show))
       (norg/w/check-before-invisible-edit 'insert)))))

(cl-defmacro norg/save-excursion-to-parent (&body body)
  (declare (indent 0))
  `(save-excursion
     (norg/w/up-heading 1)
     ,@body))

;;;; Support for do-ing and mapping

(defun norg/-mapc-headlines-satisfying (pred-of-no-args fun)
  (save-excursion
    (cl-flet ((call-fun-when-pred-is-satisfied
               ()
               (when (funcall pred-of-no-args)
                 (funcall fun))))
      (beginning-of-buffer)
      (unless (norg/w/at-heading-p)
        (norg/w/next-heading))
      (when (norg/w/at-heading-p)
        (call-fun-when-pred-is-satisfied)
        (while (progn
                 (norg/w/next-heading)
                 (not (eobp)))
          (call-fun-when-pred-is-satisfied)))))
  nil)

(defun norg/-convert-mapc-fun-to-map-fun (do-fun)
  (lambda (fun)
    (let* ((acc '()))
      (funcall do-fun (lambda ()
                        (push (funcall fun)
                              acc)))
      (nreverse acc))))

;;;; Do-ing

(defun norg/mapc-entries-from-point (fun)
  "Call FUN for the current headline and for each headline below the current
headline.
When in a body, \"current headline\" means the current body's parent headline."
  (save-excursion
    (norg/w/map-tree fun))
  nil)

(defun norg/mapc-entries-from-root (fun)
  (norg/save-excursion-to-root
    (norg/mapc-entries-from-point fun)))

(defun norg/mapc-roots (fun)
  (norg/-mapc-headlines-satisfying (lambda ()
                                     (= (norg/w/level/must-be-at-boh)
                                        1))
                                   fun))

(defun norg/mapc-entries-from-all-roots (fun)
  (norg/-mapc-headlines-satisfying (lambda () t)
                                   fun)
  nil)

;;;; Mapping

(defun norg/map-entries-from-point (fun)
  (funcall (norg/-convert-mapc-fun-to-map-fun #'norg/mapc-entries-from-point)
           fun))

(defun norg/map-entries-from-root (fun)
  (funcall (norg/-convert-mapc-fun-to-map-fun #'norg/mapc-entries-from-root)
           fun))

(defun norg/map-roots (fun)
  (funcall (norg/-convert-mapc-fun-to-map-fun #'norg/mapc-roots)
           fun))

(defun norg/map-entries-from-all-roots (fun)
  (funcall (norg/-convert-mapc-fun-to-map-fun #'norg/mapc-entries-from-all-roots)
           fun))

;;;; Reducing (add more here if and when needed)

(cl-defun norg/reduce-entries-from-point (initial-value
                                          value-fun
                                          reducing-function
                                          &optional
                                          (pred-of-no-args (lambda () t)))
  (let* ((sofar initial-value))
    (norg/mapc-entries-from-point (lambda ()
                                    (when (funcall pred-of-no-args)
                                      (let* ((v (funcall value-fun)))
                                        (setq sofar
                                              (funcall reducing-function
                                                       sofar
                                                       v))))))
    sofar))

;;;; Expanding and collapsing

(defun norg/collapse ()
  (cl-case 2
    (1
     ;; This hides too much stuff.
     (norg/w/overview)
     (norg/w/show-set-visibility 'canonical))
    (2
     ;; This hides just the subtree under the headline at point.
     ;; Idea from http://christiantietze.de/posts/2019/06/org-fold-heading/.
     ;; But what does `org-flag-subtree` do, is it part of the org public API,
     ;; and why can't I find any useful info by googling?
     (norg/w/flag-subtree t))))

(defun norg/expand (n &optional collapse-first?)
  "Expand N levels below the current headline. If COLLAPSE-FIRST? is non-nil,
collapse the tree first so that only N levels are shown. When in
a body, \"current headline\" means the current body's parent
headline."
  (when collapse-first? (norg/collapse))
  (norg/w/show-children n)
  (when norg/show-bodies?
    (let* ((level (norg/current-level)))
      (norg/mapc-entries-from-point #'(lambda ()
                                        (when (< (- (norg/w/level/must-be-at-boh)
                                                    level)
                                                 n)
                                          (norg/w/show-entry)))))))

(defun norg/expand-fully ()
  (norg/expand 1000) ; TODO magic number
  )

;;;; ___________________________________________________________________________
;;;; ____ * Visibility span

(defun norg/collapse-all-and-set-visibility-span (detail)
  (cl-flet ((collapse
             ()
             (cl-case 1
               (1 (org-overview))
               (2 (save-excursion
                    (norg/goto-root)
                    (norg/collapse))))))
    (collapse)
    (org-show-set-visibility detail)))

(defun norg/show-tree-only ()
  (norg/collapse-all-and-set-visibility-span 'tree))

(cl-defmethod nomis/tree/show-tree-only--aux ((k (eql :org)))
  (norg/show-tree-only))

;;;; ___________________________________________________________________________
;;;; ---- * Max lineage

(cl-defmethod nomis/tree/max-lineage--aux ((k (eql :org)))
  (error "Not supported: %s %s" k this-command))

;;;; ___________________________________________________________________________
;;;; ____ * Navigation

;;;; ____ ** Forward and backward at same level

(defun norg/-heading-same-level-helper (move-fun
                                        error-message)
  (nomis/scrolling/with-maybe-maintain-line-no-in-window
    (norg/w/back-to-heading t)
    (let* ((starting-point (point)))
      (funcall move-fun 1 t)
      (norg/show-point)
      (when (= (point) starting-point)
        (nomis/popup/error-message "%s" error-message)))))

(defun norg/next-sibling ()
  "Move forward one subheading at same level as this one.
Like `org-forward-heading-same-level` but:
- when the target is invisible, make it visible
- if this is the first subheading within its parent, display a popup message."
  (norg/-heading-same-level-helper #'norg/w/forward-heading-same-level
                                   "No next heading at this level"))

(cl-defmethod nomis/tree/next-sibling--aux ((k (eql :org)))
  (norg/next-sibling))

(defun norg/previous-sibling ()
  "Move backward one subheading at same level as this one.
Like `org-backward-heading-same-level` but:
- when the target is invisible, make it visible
- if this is the first subheading within its parent, display a popup message."
  (norg/-heading-same-level-helper #'norg/w/backward-heading-same-level
                                   "No previous heading at this level"))

(cl-defmethod nomis/tree/previous-sibling--aux ((k (eql :org)))
  (norg/previous-sibling))

;;;; ____ ** Forward and backward at same level, allow cross-parent

(defun norg/-heading-same-level/allow-cross-parent/helper (direction)
  (nomis/scrolling/with-maybe-maintain-line-no-in-window
    (let ((start-position-fun (cl-case direction
                                (:forward 'norg/w/end-of-line)
                                (:backward 'norg/w/beginning-of-line)))
          (re-search-function (cl-case direction
                                (:forward 're-search-forward)
                                (:backward 're-search-backward)))
          (post-search-adjust-function (cl-case direction
                                         (:forward 'norg/w/beginning-of-line)
                                         (:backward #'(lambda ())))))
      (let* ((text-to-look-for (save-excursion
                                 (norg/w/beginning-of-line)
                                 (concat (thing-at-point 'symbol)
                                         " "))))
        (funcall start-position-fun)
        (let ((found-p (condition-case nil
                           (progn
                             (funcall re-search-function
                                      (concat "^" (regexp-quote text-to-look-for))
                                      nil
                                      nil ;t
                                      )
                             t)
                         (error nil))))
          (if found-p
              (progn
                (norg/show-point)
                (funcall post-search-adjust-function))
            (progn
              (norg/w/beginning-of-line)
              (let* ((msg (cl-case direction
                            (:forward
                             "No next heading at this level, even across parents")
                            (:backward
                             "No previous heading at this level, even across parents"
                             ;; but maybe there is -- I've seen a bug here
                             ))))
                (nomis/popup/error-message "%s" msg)))))))))

(defun norg/next-sibling/allow-cross-parent ()
  "Move forward one subheading at same level as this one.
Like `org-forward-heading-same-level` but:
- when the target is invisible, make it visible
- if this is the first subheading within its parent, move to the first
  subheading at this level in the next parent."
  (norg/-heading-same-level/allow-cross-parent/helper :forward))

(cl-defmethod nomis/tree/next-sibling/allow-cross-parent--aux ((k (eql :org)))
  (norg/next-sibling/allow-cross-parent))

(defun norg/previous-sibling/allow-cross-parent ()
  "Move backward one subheading at same level as this one.
Like `org-backward-heading-same-level` but:
- when the target is invisible, make it visible
- if this is the first subheading within its parent, move to the last
subheading at this level in the previous parent."
  (norg/-heading-same-level/allow-cross-parent/helper :backward))

(cl-defmethod nomis/tree/previous-sibling/allow-cross-parent--aux
  ((k (eql :org)))
  (norg/previous-sibling/allow-cross-parent))

;;;; ____ ** Forward and backward at any level

(defvar norg/-heading-any-level-show-entry?
  t
  "Truthy if `norg/next-heading` should show bodies (and so match
`norg/next-heading/set-tree+body)`.
Same for the `backward` commands.")

(defun norg/next-heading ()
  (nomis/scrolling/with-maybe-maintain-line-no-in-window
    (norg/w/next-heading)
    (if norg/-heading-any-level-show-entry?
        (norg/w/show-entry)
      (norg/show-point))))

(cl-defmethod nomis/tree/next-heading--aux ((k (eql :org)) n)
  ;; TODO: We are ignoring `n`.
  (norg/next-heading))

(defun norg/previous-heading ()
  (nomis/scrolling/with-maybe-maintain-line-no-in-window
    (norg/w/previous-heading)
    (if norg/-heading-any-level-show-entry?
        (norg/w/show-entry)
      (norg/show-point))))

(cl-defmethod nomis/tree/previous-heading--aux ((k (eql :org)) n)
  ;; TODO: We are ignoring `n`.
  (norg/previous-heading))

(defun norg/next-heading/set-tree+body ()
  (norg/next-heading)
  (nomis/org-visibility-span/set-tree+body))

(cl-defmethod nomis/tree/next-heading/set-tree+body--aux ((k (eql :org)))
  (norg/next-heading/set-tree+body))

(defun norg/previous-heading/set-tree+body ()
  (norg/previous-heading)
  (nomis/org-visibility-span/set-tree+body))

(cl-defmethod nomis/tree/previous-heading/set-tree+body--aux ((k (eql :org)))
  (norg/previous-heading/set-tree+body))

;;;; ___________________________________________________________________________
;;;; ____ * Info that relies on our navigation stuff

(defun norg/on-first-child?/must-be-at-boh ()
  (save-excursion
    (let ((starting-point (point)))
      (norg/w/backward-heading-same-level 1 t)
      (= (point) starting-point))))

(defun norg/on-last-child?/must-be-at-boh ()
  (save-excursion
    (let ((starting-point (point)))
      (norg/w/forward-heading-same-level 1 t)
      (= (point) starting-point))))

;;;; ___________________________________________________________________________
;;;; ____ * Stepping TODO This uses `norg/fully-expanded?`, and so belongs later in the file

(defvar norg/step-n-levels-to-show nil)

(defun norg/set-step-n-levels-to-show (n)
  (when (null n)
    (setq n
          (let* ((s (read-string
                     (cl-format nil
                                "Number of levels to show ~
                                 (empty string for all children) ~
                                 (currently ~s): "
                                norg/step-n-levels-to-show))))
            (if (member (s-trim s) '("" "nil"))
                nil
              (string-to-number s)))))
  (setq norg/step-n-levels-to-show (if (null n) n (max 1 (floor n))))
  (message "n-levels-to-show set to %s" norg/step-n-levels-to-show))

(cl-defmethod nomis/tree/set-step-n-levels-to-show--aux ((k (eql :org))
                                                         n)
  (norg/set-step-n-levels-to-show n))

(defun norg/-step-then-step-cross-parent? ()
  (let ((cmds (list (norg/last-command)
                    this-command)))
    (member cmds
            '((nomis/tree/step-forward
               nomis/tree/step-forward/allow-cross-parent)
              (nomis/tree/step-backward
               nomis/tree/step-backward/allow-cross-parent)))))

(defun norg/-doing-one-of-the-step-forward-commands? ()
  (member this-command
          '(nomis/tree/step-forward
            nomis/tree/step-forward/allow-cross-parent)))

(defun norg/-doing-one-of-the-step-backward-commands? ()
  (member this-command
          '(nomis/tree/step-backward
            nomis/tree/step-backward/allow-cross-parent)))

(defun norg/-stepping-forward-on-last-but-not-first-child/must-be-at-boh ()
  (and (norg/-doing-one-of-the-step-forward-commands?)
       (norg/on-last-child?/must-be-at-boh)
       (not (norg/on-first-child?/must-be-at-boh))))

(defun norg/-stepping-backward-on-first-but-not-last-child/must-be-at-boh ()
  (and (norg/-doing-one-of-the-step-backward-commands?)
       (norg/on-first-child?/must-be-at-boh)
       (not (norg/on-last-child?/must-be-at-boh))))

(defvar norg/-most-recent-step-time -9999)

(defvar norg/step-quick-repeat-delay
  (if (boundp '*nomis/popup/duration*)
      *nomis/popup/duration*
    1))

(defun norg/-small-time-gap-since-prev-step-command? ()
  (< (float-time)
     (+ norg/-most-recent-step-time
        norg/step-quick-repeat-delay)))

(defun norg/-step-then-step-cross-parent-with-small-time-gap? ()
  (and (norg/-small-time-gap-since-prev-step-command?)
       (norg/-step-then-step-cross-parent?)))

(defun norg/-step/impl (n allow-cross-parent? n-levels-to-show-or-nil)
  (let* ((n-levels-or-nil (or n-levels-to-show-or-nil
                              norg/step-n-levels-to-show)))
    (nomis/scrolling/with-maybe-maintain-line-no-in-window
      (cl-flet ((expanded-to-desired-level?
                 ()
                 (if (null n-levels-or-nil)
                     (norg/fully-expanded?)
                   (let* ((n-levels-being-shown-or-infinity
                           (1- (norg/smallest-invisible-level-below-or-infinity))))
                     (if (= n-levels-being-shown-or-infinity
                            norg/-plus-infinity)
                         ;; The tree is fully expanded at point. This is truthy if
                         ;; the number of levels below is less than or equal to
                         ;; the desired number of levels to show.
                         (<= (norg/n-levels-below)
                             n-levels-or-nil)
                       ;; The tree is not fully expanded at point. This is truthy
                       ;; if the number of levels being shown is the same as the
                       ;; desired number of levels.
                       (= n-levels-being-shown-or-infinity
                          n-levels-or-nil)))))
                (try-to-move
                 ()
                 (if allow-cross-parent?
                     (norg/-heading-same-level/allow-cross-parent/helper
                      (cl-case n
                        (1 :forward)
                        (-1 :backward)))
                   (norg/w/forward-heading-same-level n t)))
                (tried-to-go-too-far
                 ()
                 (let* ((msg (concat (if (< n 0)
                                         "No previous heading at this level"
                                       "No next heading at this level")
                                     (if allow-cross-parent?
                                         ", even across parents"
                                       ""))))
                   (nomis/popup/error-message msg)))
                (expand
                 ()
                 (if (null n-levels-or-nil)
                     (norg/expand-fully)
                   (norg/expand n-levels-or-nil t))))
        (norg/w/back-to-heading t)
        (if (not (or (norg/-stepping-forward-on-last-but-not-first-child/must-be-at-boh)
                     (norg/-stepping-backward-on-first-but-not-last-child/must-be-at-boh)
                     ;; If we very recently did a `norg/step-xxxx` which tried to
                     ;; go too far and which so collapsed the current heading, and
                     ;; if now we're doing a `norg/step-xxxx/allow-cross-parent`,
                     ;; we're happy to do a step across the parent.
                     (norg/-step-then-step-cross-parent-with-small-time-gap?)
                     (expanded-to-desired-level?)))
            (expand)
          (let* ((starting-point (point))
                 (start-on-first-or-last-child?
                  (if (< n 0)
                      (norg/on-first-child?/must-be-at-boh)
                    (norg/on-last-child?/must-be-at-boh))))
            (try-to-move)
            (let* ((moved? (not (= (point) starting-point))))
              (if moved?
                  (progn
                    ;; Collapse the headline where we started.
                    (save-excursion
                      (goto-char starting-point)
                      (when start-on-first-or-last-child?
                        ;; We've moved across a parent, so collapse that.
                        (norg/w/up-heading 1))
                      (norg/collapse))
                    ;; Expand.
                    (expand))
                (progn
                  (norg/collapse)
                  (tried-to-go-too-far))))))))
    (setq norg/-most-recent-step-time (float-time))
    (message "n-levels = %s" (or n-levels-or-nil "all"))))

(defun norg/step-forward (n-levels-to-show-or-nil)
  (norg/-step/impl 1 nil n-levels-to-show-or-nil))

(cl-defmethod nomis/tree/step-forward--aux ((k (eql :org)) n-levels-to-show-or-nil)
  (norg/step-forward n-levels-to-show-or-nil))

(defun norg/step-backward (n-levels-to-show-or-nil)
  (norg/-step/impl -1 nil n-levels-to-show-or-nil))

(cl-defmethod nomis/tree/step-backward--aux ((k (eql :org)) n-levels-to-show-or-nil)
  (norg/step-backward n-levels-to-show-or-nil))

(defun norg/step-forward/allow-cross-parent (n-levels-to-show-or-nil)
  (norg/-step/impl 1 t n-levels-to-show-or-nil))

(cl-defmethod nomis/tree/step-forward/allow-cross-parent--aux ((k (eql :org)) n-levels-to-show-or-nil)
  (norg/step-forward/allow-cross-parent n-levels-to-show-or-nil))

(defun norg/step-backward/allow-cross-parent (n-levels-to-show-or-nil)
  (norg/-step/impl -1 t n-levels-to-show-or-nil))

(cl-defmethod nomis/tree/step-backward/allow-cross-parent--aux ((k (eql :org)) n-levels-to-show-or-nil)
  (norg/step-backward/allow-cross-parent n-levels-to-show-or-nil))

;;;; ___________________________________________________________________________
;;;; ____ * Info about trees

(defun norg/deepest-level-below ()
  "The deepest level that exists below the current headline.
When in a body, \"current headline\" means the current body's parent headline.
Example: If we are at level 5 and there are 2 further levels below, the result
is 7."
  (norg/reduce-entries-from-point 0
                                  #'norg/level-incl-body/must-be-at-boh
                                  #'max))

(defun norg/n-levels-below ()
  "The number of levels that exist below the current headline.
When in a body, \"current headline\" means the current body's parent headline.
Example: If we are at level 5 and there are 2 further levels below, the result
is 2."
  (- (norg/deepest-level-below)
     (norg/current-level)))

(defun norg/smallest-invisible-level-below-or-infinity ()
  "The level to use when incrementally expanding the current headline, or
infinity if the current headline is fully expanded.
When in a body, \"current headline\" means the current body's parent headline."
  (- (norg/reduce-entries-from-point
      norg/-plus-infinity
      #'(lambda ()
          (let* ((point-visible? (norg/point-is-visible?))
                 (level (norg/w/level/must-be-at-boh)))
            (if (not point-visible?)
                level
              (cl-destructuring-bind (has-body?
                                      has-visible-body?
                                      has-invisible-body?)
                  (norg/-body-info)
                (if (or (not norg/show-bodies?)
                        (not has-body?)
                        has-visible-body?)
                    norg/-plus-infinity
                  (1+ level))))))
      #'min)
     (norg/current-level)))

;;;; ___________________________________________________________________________
;;;; ____ * The idea of tree-info, and things that use it

(defun norg/-tree-info ()
  "Tree info for the current headline.
When in a body, \"current headline\" means the current body's parent headline."
  ;; This is rather expensive, because the value returned by
  ;; `norg/map-entries-from-point` is processed further and discarded.
  ;; You could do more in the fun passed to `norg/map-entries-from-point`, but
  ;; it's fiddly because:
  ;; - You want to collect multiple items per iteration.
  ;;   (Solution: use nconc on lists.)
  ;; - You want to look at two headlines at a time.
  ;;   (Solution: record previous item in a piece of mutable state.)
  ;; - You'd need to do some fixing up at the end to add that final dummy
  ;;   entry when the final item is visible.
  (let* ((dummy-initial-entry
          '(:dummy-first t :dummy-first :dummy-first :dummy-first))
         (basic-info
          (norg/map-entries-from-point (lambda ()
                                         (list* (norg/w/level/must-be-at-boh)
                                                (norg/point-is-visible?)
                                                (norg/-body-info)))))
         (just-did-a-body? nil))
    (cl-loop for ((prev-level prev-visible? . _)
                  . ((level
                      visible?
                      has-body?
                      has-visible-body?
                      has-invisible-body?)
                     . _))
             on (cons dummy-initial-entry
                      basic-info)
             for first? = (eq prev-level :dummy-first)
             for last? = (null level)
             for prev-was-visible-leaf? = (and (not just-did-a-body?)
                                               (not first?)
                                               prev-visible?
                                               (or last?
                                                   (<= level prev-level)))

             when prev-was-visible-leaf?
             collect (list (1+ prev-level)
                           nil
                           t) ; dummy invisible entry

             when (not last?)
             collect (list level
                           visible?
                           nil)

             when has-body?
             collect (list (1+ level)
                           has-visible-body?
                           nil)

             when has-visible-body?
             collect (list (+ 2 level)
                           nil
                           t) ; dummy invisible entry

             do (setq just-did-a-body? has-body?))))

(defun norg/fully-expanded? (&optional tree-info)
  "Is the tree beneath the current headline fully expanded?
When in a body, \"current headline\" means the current body's parent headline."
  (setq tree-info (or tree-info (norg/-tree-info)))
  (cl-loop for (level visible? dummy?)
           in tree-info
           when (not dummy?)
           always visible?))

(defun norg/level-for-incremental-contract (&optional tree-info)
  "The level to use when incrementally collapsing the current headline.
When in a body, \"current headline\" means the current body's parent headline."
  ;; Collapse the most-deeply-nested expanded level, and expand everything
  ;; else to that level.
  (setq tree-info (or tree-info
                      (norg/-tree-info)))
  (let* ((v (let* ((deepest-visible-levels
                    (cl-loop for ((prev-level prev-visible?)
                                  . ((level visible?) . _))
                             on (cons '(most-negative-fixnum t)
                                      tree-info)
                             when (and prev-visible?
                                       (not visible?)
                                       (> level prev-level))
                             collect prev-level)))
              (- (apply #'max deepest-visible-levels)
                 1))))
    (- v (norg/current-level))))

;;;; ___________________________________________________________________________
;;;; ____ * Operations on root

(defun norg/n-levels-below/root ()
  (norg/save-excursion-to-root
    (norg/n-levels-below)))

(defun norg/level-for-incremental-contract/root ()
  (norg/save-excursion-to-root
    (norg/level-for-incremental-contract)))

(defun norg/smallest-invisible-level-below-or-infinity/root ()
  (norg/save-excursion-to-root
    (norg/smallest-invisible-level-below-or-infinity)))

;;;; ___________________________________________________________________________
;;;; ____ * Operations on buffer

(defun norg/levels/max-in-buffer ()
  (let* ((sofar 0))
    (norg/mapc-entries-from-all-roots (lambda ()
                                        (setq sofar (max (norg/level-incl-body/must-be-at-boh)
                                                         sofar))))
    sofar))

(defun norg/n-levels-below/buffer ()
  (1- (norg/levels/max-in-buffer)))

(defun norg/level-for-incremental-contract/buffer ()
  (->> (norg/map-roots #'norg/level-for-incremental-contract)
       (apply #'max)))

(defun norg/smallest-invisible-level-below-or-infinity/buffer ()
  (->> (norg/map-roots #'norg/smallest-invisible-level-below-or-infinity)
       (apply #'min)))

;;;; ___________________________________________________________________________
;;;; ____ * Expanding and collapsing

;;;; ____ ** General support

(defun norg/-unmodified-value-and-arg->level (unmodified-value arg setting-kind)
  (let* ((delta (if (numberp arg) arg 1))
         (f (cl-ecase setting-kind
              (:less #'-)
              (:more #'+))))
    (funcall f
             unmodified-value
             (1- delta))))

;;;; ____ ** norg/-set-level-etc

(defun norg/-out-of-range (v maximum setting-kind current-value)
  (let* ((min-allowed-value (if *expanding-parent?* 1 0)))
    (cl-ecase setting-kind
      (:no-check
       )
      (:less
       (< v min-allowed-value))
      (:more
       (> v maximum))
      (:setting-min
       (= current-value min-allowed-value))
      (:setting-max
       (= current-value norg/-plus-infinity)))))

(defvar *norg/wrap-expand-collapse? nil)
(defvar *norg/-allow-cycle-wrap-now?* nil)
(defvar *norg/-allow-cycle-wrap-timer* nil)

(defun norg/-cancel-cycle-to-zero-timer ()
  (when *norg/-allow-cycle-wrap-timer*
    (cancel-timer *norg/-allow-cycle-wrap-timer*)
    (setq *norg/-allow-cycle-wrap-timer* nil)
    (setq *norg/-allow-cycle-wrap-now?* nil)))

(defun norg/-allow-cycle-to-zero-for-a-while ()
  (setq *norg/-allow-cycle-wrap-now?* t)
  (let ((secs (if (boundp '*nomis/popup/duration*)
                  *nomis/popup/duration*
                1)))
    (setq *norg/-allow-cycle-wrap-timer*
          (run-at-time secs
                       nil
                       'norg/-cancel-cycle-to-zero-timer))))

(defun norg/-bring-within-range (v maximum)
  (let* ((min-allowed-value (if *expanding-parent?* 1 0)))
    (cl-flet ((normal-behaviour () (list (min (max min-allowed-value v)
                                              maximum)
                                         nil))
              (cycled-behaviour () (list (if (= v (1- min-allowed-value))
                                             maximum
                                           min-allowed-value)
                                         t)))
      (let* ((allow-cycle-wrap-now? *norg/-allow-cycle-wrap-now?*))
        (norg/-cancel-cycle-to-zero-timer)
        (if (or (= maximum 0)
                (not (or (= v (1- min-allowed-value))
                         (= v norg/-plus-infinity))))
            (normal-behaviour)
          (if (not allow-cycle-wrap-now?)
              (progn
                (when *norg/wrap-expand-collapse?
                  (norg/-allow-cycle-to-zero-for-a-while))
                (normal-behaviour))
            ;; Don't cycle if we moved to another position that also
            ;; happens to be fully-expanded.
            ;; Don't cycle if we moved away and came back.
            (if (not (eq this-command (norg/last-command)))
                (normal-behaviour)
              (cycled-behaviour))))))))

(defun norg/-set-level-etc (new-value-action-fun
                            new-level/maybe-out-of-range
                            maximum
                            message-format-string
                            setting-kind
                            current-value)
  (save-excursion ; sometimes position is lost when at an invisible pount-- a hacky fix
    (let* ((v (if (eql new-level/maybe-out-of-range
                       :max)
                  ;; The special value of `:max` means that we don't
                  ;; have to compute the value twice.
                  maximum
                new-level/maybe-out-of-range)))
      (cl-multiple-value-bind (new-level do-cycling?)
          (norg/-bring-within-range v maximum)
        (let* ((out-of-range? (and (not do-cycling?)
                                   (norg/-out-of-range v
                                                       maximum
                                                       setting-kind
                                                       current-value))))
          (prog1
              (progn ; nomis/scrolling/with-force-maintain-line-no-in-window ; Is this needed? Presumably it is, but why? (Or maybe it was needed, but isn't now.) -- 2021-06-22 I've removed it. It was causing one-line scrolling in certain situations.
                (funcall new-value-action-fun new-level))
            (funcall (if out-of-range?
                         #'nomis/popup/error-message
                       #'nomis/popup/message)
                     (concat message-format-string "%s%s")
                     new-level
                     maximum
                     (if norg/show-bodies?
                         ""
                       " (not showing bodies)")
                     (if out-of-range?
                         (cl-ecase setting-kind
                           ((:less :setting-min)
                            " —- already fully collapsed")
                           ((:more :setting-max)
                            " —- already fully expanded"))
                       ""))))))))

;;;; ____ ** norg/show-children-from-point/xxxx support

(defun norg/show-children-from-point* (n) ; TODO You don't use the special negative arg thing. Simplify or get back that functionality.
  "Make point visible if it isn't already, and expand current headline to
n levels.

Details:

If N is not negative, expand to show N levels. Any headlines at level N
will be collapsed.

If N is negative, expand to show (abs N) levels, but do not hide anything
that is already being displayed."
  (norg/show-point)
  (let* ((collapse? (>= n 0))
         (n (abs n)))
    (when collapse?
      (norg/collapse))
    (norg/expand n)))

(defun norg/-show-children-from-point/set-level-etc (level
                                                     setting-kind
                                                     current-value)
  (norg/-set-level-etc #'norg/show-children-from-point*
                       level
                       (norg/n-levels-below)
                       (if *expanding-parent?*
                           "[%s / %s] from parent"
                         "[%s / %s]")
                       setting-kind
                       current-value))

;;;; ____ ** norg/show-children-from-point/xxxx

(defun norg/show-children-from-point (n)
  (interactive "^p")
  "Show N levels from the current headline, and collapse anything that's
at a higher level.
When in a body, \"current headline\" means the current body's parent headline."
  (let* ((v n))
    (norg/-show-children-from-point/set-level-etc v :no-check :dummy)))

(defun norg/show-children-from-point/set-min ()
  "Fully collapse the current headline.
When in a body, \"current headline\" means the current body's parent headline."
  (let* ((current-value (1+ (norg/level-for-incremental-contract)))
         (v (if *expanding-parent?* 1 0)))
    (norg/-show-children-from-point/set-level-etc v :setting-min current-value)))

(cl-defmethod nomis/tree/show-children-from-point/set-min--aux
  ((k (eql :org)))
  (norg/show-children-from-point/set-min))

(defun norg/show-children-from-point/fully-expand ()
  "Fully expand the current headline.
When in a body, \"current headline\" means the current body's parent headline."
  (let* ((current-value (norg/smallest-invisible-level-below-or-infinity))
         (v :max))
    (norg/-show-children-from-point/set-level-etc v :setting-max current-value)))

(cl-defmethod nomis/tree/show-children-from-point/fully-expand--aux
  ((k (eql :org)))
  (norg/show-children-from-point/fully-expand))

(defun norg/show-children-from-point/incremental/less (&optional arg)
  "Incrementally collapse the current headline by `arg` levels, default 1.
When in a body, \"current headline\" means the current body's parent headline."
  (let* ((v (-> (norg/level-for-incremental-contract)
                (norg/-unmodified-value-and-arg->level arg :less))))
    (norg/-show-children-from-point/set-level-etc v :less :dummy)))

(cl-defmethod nomis/tree/show-children-from-point/incremental/less--aux
  ((k (eql :org)) arg)
  (norg/show-children-from-point/incremental/less arg))

(defun norg/show-children-from-point/incremental/more (&optional arg)
  "Incrementally expand the current headline by `arg` levels, default 1.
When in a body, \"current headline\" means the current body's parent headline."
  (let* ((v (-> (norg/smallest-invisible-level-below-or-infinity)
                (norg/-unmodified-value-and-arg->level arg :more))))
    (norg/-show-children-from-point/set-level-etc v :more :dummy)))

(cl-defmethod nomis/tree/show-children-from-point/incremental/more--aux
  ((k (eql :org)) arg)
  (norg/show-children-from-point/incremental/more arg))

;;;; ____ ** norg/show-children-from-parent/xxxx support

(defvar *expanding-parent?* nil)

(defun norg/save-excursion-to-parent-and-then-show-point* (f)
  (prog1
      (let* ((*expanding-parent?* t))
        (norg/save-excursion-to-parent (funcall f)))
    (norg/show-point)))

(cl-defmacro norg/save-excursion-to-parent-and-then-show-point (&body body)
  (declare (indent 0))
  `(norg/save-excursion-to-parent-and-then-show-point* (lambda () ,@body)))

;;;; ____ ** norg/show-children-from-parent/xxxx

(defun norg/show-children-from-parent (n)
  "Like `norg/show-children-from-point`, but from the current entry's parent."
  (interactive "^p")
  (norg/save-excursion-to-parent-and-then-show-point
    (norg/show-children-from-point n)))

(defun norg/show-children-from-parent/set-min ()
  "Like `norg/show-children-from-point/set-min`, but from the
current entry's parent and showing one level."
  (norg/save-excursion-to-parent-and-then-show-point
    (norg/show-children-from-point/set-min)))

(cl-defmethod nomis/tree/show-children-from-parent/set-min--aux
  ((k (eql :org)))
  (norg/show-children-from-parent/set-min))

(defun norg/show-children-from-parent/fully-expand ()
  "Like `norg/show-children-from-point/fully-expand`, but from
the current entry's parent."
  (norg/save-excursion-to-parent-and-then-show-point
    (norg/show-children-from-point/fully-expand)))

(cl-defmethod nomis/tree/show-children-from-parent/fully-expand--aux
  ((k (eql :org)))
  (norg/show-children-from-parent/fully-expand))

(defun norg/show-children-from-parent/incremental/less (&optional arg)
  "Like `norg/show-children-from-point/incremental/less`, but
from the current entry's parent and with the parent always
expanded at least one level."
  (norg/save-excursion-to-parent-and-then-show-point
    (norg/show-children-from-point/incremental/less arg)))

(cl-defmethod nomis/tree/show-children-from-parent/incremental/less--aux
  ((k (eql :org)) arg)
  (norg/show-children-from-parent/incremental/less arg))

(defun norg/show-children-from-parent/incremental/more (&optional arg)
  "Like `norg/show-children-from-point/incremental/more`, but
from the current entry's parent."
  (norg/save-excursion-to-parent-and-then-show-point
    (norg/show-children-from-point/incremental/more arg)))

(cl-defmethod nomis/tree/show-children-from-parent/incremental/more--aux
  ((k (eql :org)) arg)
  (norg/show-children-from-parent/incremental/more arg))

;;;; ____ ** norg/show-children-from-root/xxxx support

(defun norg/show-children-from-root* (n)
  "Call `norg/show-children-from-point*` on the current root headline, with N as
the parameter."
  (norg/save-excursion-to-root
    (norg/show-children-from-point* n)))

(defun norg/-show-children-from-root/set-level-etc (level
                                                    setting-kind
                                                    current-value)
  (norg/-set-level-etc #'norg/show-children-from-root*
                       level
                       (norg/n-levels-below/root)
                       "[%s of %s] from root"
                       setting-kind
                       current-value))

;;;; ____ ** norg/show-children-from-root/xxxx

(defun norg/show-children-from-root (n)
  (interactive "^p")
  (let* ((v n))
    (norg/-show-children-from-root/set-level-etc v :no-check :dummy)))

(defun norg/show-children-from-root/set-min ()
  (let* ((current-value (1+ (norg/level-for-incremental-contract/root)))
         (v 0))
    (norg/-show-children-from-root/set-level-etc v :setting-min current-value)))

(cl-defmethod nomis/tree/show-children-from-root/set-min--aux
  ((k (eql :org)))
  (norg/show-children-from-root/set-min))

(defun norg/show-children-from-root/fully-expand ()
  (let* ((current-value (norg/smallest-invisible-level-below-or-infinity/root))
         (v :max))
    (norg/-show-children-from-root/set-level-etc v :setting-max current-value)))

(cl-defmethod nomis/tree/show-children-from-root/fully-expand--aux
  ((k (eql :org)))
  (norg/show-children-from-root/fully-expand))

(defun norg/show-children-from-root/incremental/less (arg)
  "Incrementally collapse the current root by `arg` levels, default 1."
  (let* ((v (-> (norg/level-for-incremental-contract/root)
                (norg/-unmodified-value-and-arg->level arg :less))))
    (norg/-show-children-from-root/set-level-etc v :less :dummy)))

(cl-defmethod nomis/tree/show-children-from-root/incremental/less--aux
  ((k (eql :org)) arg)
  (norg/show-children-from-root/incremental/less arg))

(defun norg/show-children-from-root/incremental/more (arg)
  "Incrementally expand the current root by `arg` levels, default 1."
  (let* ((v (-> (norg/smallest-invisible-level-below-or-infinity/root)
                (norg/-unmodified-value-and-arg->level arg :more))))
    (norg/-show-children-from-root/set-level-etc v :more :dummy)))

(cl-defmethod nomis/tree/show-children-from-root/incremental/more--aux
  ((k (eql :org)) arg)
  (norg/show-children-from-root/incremental/more arg))

(defun norg/show-children-from-root/to-current-level ()
  (let* ((v (1- (norg/current-level t))))
    (norg/-show-children-from-root/set-level-etc v :no-check :dummy)))

(cl-defmethod nomis/tree/show-children-from-root/to-current-level--aux
  ((k (eql :org)))
  (norg/show-children-from-root/to-current-level))

;;;; ____ ** norg/show-children-from-all-roots/xxxx support

(defun norg/show-children-from-all-roots* (n)
  "Call `norg/show-children-from-point*` on all root headlines, with N as
the parameter."
  (norg/mapc-roots (lambda () (norg/show-children-from-point* n))))

(defun norg/-show-children-from-all-roots/set-level-etc (level
                                                         setting-kind
                                                         current-value)
  (norg/-set-level-etc #'norg/show-children-from-all-roots*
                       level
                       (norg/n-levels-below/buffer)
                       "[%s of %s] from all roots"
                       setting-kind
                       current-value))

;;;; ____ ** norg/show-children-from-all-roots/xxxx

(defun norg/show-children-from-all-roots (n)
  (interactive "^p")
  (let* ((v n))
    (norg/-show-children-from-all-roots/set-level-etc v :no-check :dummy)))

(defun norg/show-children-from-all-roots/set-min ()
  (let* ((current-value (1+ (norg/level-for-incremental-contract/buffer)))
         (v 0))
    (norg/-show-children-from-all-roots/set-level-etc v :setting-min current-value)))

(cl-defmethod nomis/tree/show-children-from-all-roots/set-min--aux
  ((k (eql :org)))
  (norg/show-children-from-all-roots/set-min))

(defun norg/show-children-from-all-roots/fully-expand ()
  (let* ((current-value (norg/smallest-invisible-level-below-or-infinity/buffer))
         (v :max))
    (norg/-show-children-from-all-roots/set-level-etc v :setting-max current-value)))

(cl-defmethod nomis/tree/show-children-from-all-roots/fully-expand--aux
  ((k (eql :org)))
  (norg/show-children-from-all-roots/fully-expand))

(defun norg/show-children-from-all-roots/incremental/less (arg)
  "Incrementally collapse all roots by `arg` levels, default 1."
  (let* ((v (-> (norg/level-for-incremental-contract/buffer)
                (norg/-unmodified-value-and-arg->level arg :less))))
    (norg/-show-children-from-all-roots/set-level-etc v :less :dummy)))

(cl-defmethod nomis/tree/show-children-from-all-roots/incremental/less--aux
  ((k (eql :org)) arg)
  (norg/show-children-from-all-roots/incremental/less arg))

(defun norg/show-children-from-all-roots/incremental/more (arg)
  "Incrementally expand all roots by `arg` levels, default 1."
  (let* ((v (-> (norg/smallest-invisible-level-below-or-infinity/buffer)
                (norg/-unmodified-value-and-arg->level arg :more))))
    (norg/-show-children-from-all-roots/set-level-etc v :more :dummy)))

(cl-defmethod nomis/tree/show-children-from-all-roots/incremental/more--aux
  ((k (eql :org)) arg)
  (norg/show-children-from-all-roots/incremental/more arg))

(defun norg/show-children-from-all-roots/to-current-level ()
  (let* ((v (1- (norg/current-level t))))
    (norg/-show-children-from-all-roots/set-level-etc v :no-check :dummy)))

(cl-defmethod nomis/tree/show-children-from-all-roots/to-current-level--aux
  ((k (eql :org)))
  (norg/show-children-from-all-roots/to-current-level))

;;;; ___________________________________________________________________________
;;;; ____ * Replacements for `org-cycle` and `org-shifttab`

(defun norg/tab (arg)
  (cond ((not (norg/w/at-heading-p))
         (org-cycle arg))
        ((null arg)
         (norg/show-children-from-point/incremental/more))
        ((integerp arg)
         (norg/show-children-from-point arg))
        ((equal arg '(4))   (org-cycle nil))
        ((equal arg '(16))  (org-cycle '(4)))
        ((equal arg '(64))  (org-cycle '(16)))
        ((equal arg '(256)) (org-cycle '(64)))
        (t
         (error "Bad arg"))))

(cl-defmethod nomis/tree/tab--aux ((k (eql :org)) arg)
  (norg/tab arg))

(defun norg/shifttab (arg)
  (cond ((not (norg/w/at-heading-p))
         (org-shifttab arg))
        ((null arg)
         (norg/show-children-from-point/incremental/less))
        ((integerp arg)
         (norg/show-children-from-point arg))
        (t
         (error "Bad arg"))))

(cl-defmethod nomis/tree/shifttab--aux ((k (eql :org)) arg)
  (norg/shifttab arg))

;;;; ___________________________________________________________________________
;;;; ____ * End

(provide 'norg)
;;; norg.el ends here
