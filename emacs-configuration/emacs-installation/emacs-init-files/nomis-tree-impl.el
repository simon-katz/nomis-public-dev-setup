;;; nomis-tree-impl.el --- A layer on top of outline mode  -*- lexical-binding: t; -*-

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

;;;; Some rejected (at least for now) ideas

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
;;      - The lineage stuff is global, but you have things that work
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

;;;; Require things

(require 'a)
(require 'cl-lib)
(require 'dash)
(require 'nomis-outline-common) ; TODO: Check the require chains (and not all needed at top level).
(require 'nomis-tree-lineage-specs)

;; Things that you might want to make into packages if you make norg into a
;; package.

(require 'nomis-popup)
(require 'nomis-scrolling)

;;;; Tailor other functionality

;;;;; what-cursor-position

;; Add org level to the output of `what-cursor-position`.

(defvar norg/add-info-to-what-cursor-position?
  t
  "Control whether we add additional info to the message produced by
`what-cursor-position`. This is here just in case someone might be parsing the
message and in case adding org level messes things up.")

(defvar *-norg/in-what-cursor-position?* nil)

(advice-add 'what-cursor-position
            :around
            (lambda (orig-fun &rest args)
              (let* ((*-norg/in-what-cursor-position?* t))
                (apply orig-fun args)))
            '((name . norg/add-level-info)))

(advice-add 'message
            :around
            (lambda (orig-fun &rest args)
              (if (and norg/add-info-to-what-cursor-position?
                       (or (-nomis/outline/c/org-mode?)
                           (-nomis/outline/c/outline-mode?))
                       *-norg/in-what-cursor-position?*)
                  (let* ((format-string
                          (concat "Level: %s  " (cl-first args)))
                         (format-args
                          (append
                           (list (norg/current-level-or-error-string t))
                           (cl-rest args)))
                         (s (apply #'format format-string format-args)))
                    (funcall orig-fun "%s" s)
                    (nomis/popup/message "%s" s))
                (apply orig-fun args)))
            '((name . norg/add-level-info)))

;;;; Whether to show bodies

(defvar norg/show-bodies? t)

(defun norg/toggle-show-bodies ()
  (interactive)
  (setq norg/show-bodies? (not norg/show-bodies?))
  (message "norg/show-bodies? set to %s" norg/show-bodies?))

;;;; Some wrappers for org functionality

;;;;; Basic stuff

(defun -norg/has-body?/must-be-at-boh/leaving-cursor-at-end-of-heading ()
  (let* ((_ (nomis/outline/c/end-of-heading))
         (end-of-heading-position (point))
         (_ (nomis/outline/c/next-preface))
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

(defun -norg/has-body?/must-be-at-boh ()
  (save-excursion
    (-norg/has-body?/must-be-at-boh/leaving-cursor-at-end-of-heading)))

(defun -norg/body-info ()
  (save-excursion
    (nomis/outline/c/back-to-heading)
    (let* ((has-body?
            (-norg/has-body?/must-be-at-boh/leaving-cursor-at-end-of-heading))
           (has-visible-body? (and has-body?
                                   (nomis/outline/c/visible? (point))))
           (has-invisible-body? (and has-body?
                                     (not has-visible-body?))))
      (list has-body?
            has-visible-body?
            has-invisible-body?))))

(defun norg/level-incl-body/must-be-at-boh ()
  (let* ((heading-level (nomis/outline/c/level)))
    (+ heading-level
       (if (and norg/show-bodies?
                (-norg/has-body?/must-be-at-boh))
           1
         0))))

(defun -norg/in-body? ()
  (> (point)
     (save-excursion
       (nomis/outline/c/back-to-heading)
       (nomis/outline/c/end-of-heading)
       (point))))

(defun norg/current-level-or-error-string (&optional inc-if-in-body?)
  (condition-case _
      (nomis/outline/c/level inc-if-in-body?)
    (error "Before first heading")))

(defun norg/goto-root ()
  (interactive)
  (while (nomis/outline/c/up-heading 1 nil t)))

(cl-defmacro norg/save-excursion-to-root (&body body)
  (declare (indent 0))
  `(save-excursion
     (norg/goto-root)
     ,@body))

(cl-defmacro norg/save-excursion-to-parent (&body body)
  (declare (indent 0))
  `(save-excursion
     (nomis/outline/c/up-heading 1)
     ,@body))

;;;;; Support for do-ing and mapping

(defun -norg/mapc-headlines-satisfying (pred-of-no-args fun)
  (save-excursion
    (cl-flet ((call-fun-when-pred-is-satisfied
                ()
                (when (funcall pred-of-no-args)
                  (funcall fun))))
      (goto-char (point-min))
      (unless (nomis/outline/c/on-heading?)
        (nomis/outline/c/next-heading))
      (when (nomis/outline/c/on-heading?)
        (call-fun-when-pred-is-satisfied)
        (while (progn
                 (nomis/outline/c/next-heading)
                 (not (eobp)))
          (call-fun-when-pred-is-satisfied)))))
  nil)

(defun -norg/convert-mapc-fun-to-map-fun (do-fun)
  (lambda (fun)
    (let* ((acc '()))
      (funcall do-fun (lambda ()
                        (push (funcall fun)
                              acc)))
      (nreverse acc))))

;;;;; Do-ing

(defun norg/mapc-entries-from-point (fun)
  "Call FUN for the current headline and for each headline below the current
headline.
When in a body, \"current headline\" means the current body's parent headline."
  (save-excursion
    (nomis/outline/c/map-tree fun))
  nil)

(defun norg/mapc-entries-from-root (fun)
  (norg/save-excursion-to-root
    (norg/mapc-entries-from-point fun)))

(defun norg/mapc-roots (fun)
  (-norg/mapc-headlines-satisfying (lambda ()
                                     (= (nomis/outline/c/level)
                                        (nomis/outline/c/top-level-level)))
                                   fun))

(defun norg/mapc-entries-from-all-roots (fun)
  (-norg/mapc-headlines-satisfying (lambda () t)
                                   fun)
  nil)

;;;;; Mapping

(defun norg/map-entries-from-point (fun)
  (funcall (-norg/convert-mapc-fun-to-map-fun #'norg/mapc-entries-from-point)
           fun))

(defun norg/map-entries-from-root (fun)
  (funcall (-norg/convert-mapc-fun-to-map-fun #'norg/mapc-entries-from-root)
           fun))

(defun norg/map-roots (fun)
  (funcall (-norg/convert-mapc-fun-to-map-fun #'norg/mapc-roots)
           fun))

(defun norg/map-entries-from-all-roots (fun)
  (funcall (-norg/convert-mapc-fun-to-map-fun #'norg/mapc-entries-from-all-roots)
           fun))

;;;;; Reducing (add more here if and when needed)

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

;;;;; Expanding and collapsing

(defun norg/expand (n &optional collapse-first?)
  "Expand N levels below the current headline. If COLLAPSE-FIRST? is non-nil,
collapse the tree first so that only N levels are shown. When in
a body, \"current headline\" means the current body's parent
headline."
  (when collapse-first? (nomis/outline/c/collapse))
  (nomis/outline/c/show-children n)
  (when norg/show-bodies?
    (let* ((level (nomis/outline/c/level)))
      (norg/mapc-entries-from-point #'(lambda ()
                                        (when (< (- (nomis/outline/c/level)
                                                    level)
                                                 n)
                                          (nomis/outline/c/show-entry)))))))

(defun norg/expand-fully ()
  (norg/expand 1000) ; TODO magic number
  )

;;;; Search heading text

(defun -norg/grab-heading-text ()
  (save-excursion
    ;; Jump to first word of heading
    (nomis/outline/c/back-to-heading)
    (forward-word)
    (backward-word)
    ;; Grab text of heading
    (let* ((beg (point))
           (end (progn
                  (nomis/outline/c/end-of-line)
                  (point))))
      (if (not (< beg end))
          (error "No heading here")
        (let* ((text (buffer-substring beg end)))
          (set-text-properties 0 (length text) nil text)
          text)))))

(defvar -norg/search-heading-text/text nil)

(defun -norg/search-heading-text/search (again?)
  (cl-assert (not (null -norg/search-heading-text/text)))
  (cl-flet ((search-for-text
              ()
              (search-backward (cl-case 1
                                 (1
                                  ;; Simply look for the text.
                                  -norg/search-heading-text/text)
                                 (2
                                  ;; Just find refs. But is awkward -- you need
                                  ;; to press M-. to go back. I wanted H-. but
                                  ;; I'm already using that for something else.
                                  (s-concat "[[*"
                                            -norg/search-heading-text/text
                                            "]")))
                               nil
                               t)))
    (when again?
      (nomis/outline/c/back-to-heading))
    (or (search-for-text)
        (progn
          (goto-char (point-max))
          (search-for-text))))
  (nomis/tree/ls/show-lineage nomis/tree/ls/spec/no-hide-fat-parents))

(defun norg/search-heading-text ()
  (setq -norg/search-heading-text/text (-norg/grab-heading-text))
  (push-mark)
  (-norg/search-heading-text/search nil))

(defun norg/search-heading-text-again ()
  (if (null -norg/search-heading-text/text)
      (error "nomis/tree/search-heading-text hasn't been called yet")
    (-norg/search-heading-text/search t)))

;;;; Navigation

;;;;; Forward and backward at same level

(defun norg/next-sibling ()
  "Move forward one subheading at same level as this one.
Like `org-forward-heading-same-level` but:
- when the target is invisible, make it visible
- if this is the first subheading within its parent, display a popup message."
  (when (nomis/outline/c/prev-or-next-heading 1 :forward :sibling)
    (nomis/outline/c/ensure-heading-shown)))

(defun norg/previous-sibling ()
  "Move backward one subheading at same level as this one.
Like `org-backward-heading-same-level` but:
- when the target is invisible, make it visible
- if this is the first subheading within its parent, display a popup message."
  (when (nomis/outline/c/prev-or-next-heading 1 :backward :sibling)
    (nomis/outline/c/ensure-heading-shown)))

;;;;; Forward and backward at same level, sibling or peer

(defun norg/next-peer ()
  "Move forward one subheading at same level as this one.
Like `org-forward-heading-same-level` but:
- when the target is invisible, make it visible
- if this is the first subheading within its parent, move to the first
  subheading at this level in the next parent."
  (when (nomis/outline/c/prev-or-next-heading 1 :forward :peer)
    (nomis/outline/c/ensure-heading-shown)))

(defun norg/previous-peer ()
  "Move backward one subheading at same level as this one.
Like `org-backward-heading-same-level` but:
- when the target is invisible, make it visible
- if this is the first subheading within its parent, move to the last
subheading at this level in the previous parent."
  (when (nomis/outline/c/prev-or-next-heading 1 :backward :peer)
    (nomis/outline/c/ensure-heading-shown)))

;;;;; Forward and backward at any level

(defvar -norg/heading-any-level-show-entry?
  t
  "Truthy if `norg/next-heading` and `norg/previous-heading` should show bodies.
Same for the `backward` commands.")

(defun norg/next-heading ()
  "Move forward to the next heading at any level."
  (nomis/scrolling/with-maybe-maintain-line-no-in-window
    (nomis/outline/c/next-heading)
    (if -norg/heading-any-level-show-entry?
        (nomis/outline/c/show-entry)
      (nomis/outline/c/ensure-heading-shown))))

(defun norg/previous-heading ()
  "Move backward to the previous heading at any level."
  (nomis/scrolling/with-maybe-maintain-line-no-in-window
    (nomis/outline/c/previous-heading)
    (if -norg/heading-any-level-show-entry?
        (nomis/outline/c/show-entry)
      (nomis/outline/c/ensure-heading-shown))))

(defun norg/step-forward-any-level (n-levels-to-show-or-nil)
  "Move forward to the next heading at any level, then expand it.
N-LEVELS-TO-SHOW-OR-NIL controls how many levels to expand; nil means fully."
  ;; We should use `-norg/step/impl` here (or whatever we replace it with).
  (nomis/scrolling/with-maybe-maintain-line-no-in-window
    (nomis/outline/c/next-heading)
    (nomis/tree/ls/show-lineage nomis/tree/ls/spec/fat-parents)
    (let* ((n-levels-or-nil (or n-levels-to-show-or-nil
                                norg/step-n-levels-to-show)))
      (if (null n-levels-or-nil)
          (norg/expand-fully)
        (norg/expand n-levels-or-nil t)))))

(defun norg/step-backward-any-level (n-levels-to-show-or-nil)
  "Move backward to the previous heading at any level, then expand it.
N-LEVELS-TO-SHOW-OR-NIL controls how many levels to expand; nil means fully."
  ;; We should use `-norg/step/impl` here (or whatever we replace it with).
  (nomis/scrolling/with-maybe-maintain-line-no-in-window
    (nomis/outline/c/previous-heading)
    (nomis/tree/ls/show-lineage nomis/tree/ls/spec/fat-parents)
    (let* ((n-levels-or-nil (or n-levels-to-show-or-nil
                                norg/step-n-levels-to-show)))
      (if (null n-levels-or-nil)
          (norg/expand-fully)
        (norg/expand n-levels-or-nil t)))))

;;;; Info that relies on our navigation stuff

(defun norg/on-first-child?/must-be-at-boh ()
  (save-excursion
    (let ((starting-point (point)))
      (nomis/outline/c/prev-or-next-heading 1 :backward :sibling t)
      (= (point) starting-point))))

(defun norg/on-last-child?/must-be-at-boh ()
  (save-excursion
    (let ((starting-point (point)))
      (nomis/outline/c/prev-or-next-heading 1 :forward :sibling t)
      (= (point) starting-point))))

;;;; Stepping TODO This uses `norg/fully-expanded?`, and so belongs later in the file

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

(defun -norg/step-sibling-then-step-peer? ()
  (let ((cmds (list (nomis/outline/c/last-command)
                    this-command)))
    (member cmds
            '((nomis/tree/step-forward-sibling
               nomis/tree/step-forward-peer)
              (nomis/tree/step-backward-sibling
               nomis/tree/step-backward-peer)))))

(defun -norg/doing-one-of-the-step-forward-commands? ()
  (member this-command
          '(nomis/tree/step-forward-sibling
            nomis/tree/step-forward-peer)))

(defun -norg/doing-one-of-the-step-backward-commands? ()
  (member this-command
          '(nomis/tree/step-backward-sibling
            nomis/tree/step-backward-peer)))

(defun -norg/stepping-forward-on-last-but-not-first-child/must-be-at-boh ()
  (and (-norg/doing-one-of-the-step-forward-commands?)
       (norg/on-last-child?/must-be-at-boh)
       (not (norg/on-first-child?/must-be-at-boh))))

(defun -norg/stepping-backward-on-first-but-not-last-child/must-be-at-boh ()
  (and (-norg/doing-one-of-the-step-backward-commands?)
       (norg/on-first-child?/must-be-at-boh)
       (not (norg/on-last-child?/must-be-at-boh))))

(defvar -norg/most-recent-step-time -9999)

(defvar norg/step-quick-repeat-delay
  (if (boundp '*nomis/popup/duration*)
      *nomis/popup/duration*
    1))

(defun -norg/small-time-gap-since-prev-step-command? ()
  (< (float-time)
     (+ -norg/most-recent-step-time
        norg/step-quick-repeat-delay)))

(defun -norg/step-sibling-then-step-peer-with-small-time-gap? ()
  (and (-norg/small-time-gap-since-prev-step-command?)
       (-norg/step-sibling-then-step-peer?)))

(defun -norg/step/impl (n sibling-or-peer n-levels-to-show-or-nil)
  (let* ((n-levels-or-nil (or n-levels-to-show-or-nil
                              norg/step-n-levels-to-show)))
    (nomis/scrolling/with-maybe-maintain-line-no-in-window
      (cl-flet ((expanded-to-desired-level?
                  ()
                  (if (null n-levels-or-nil)
                      (norg/fully-expanded?)
                    (let* ((n-levels-being-shown-or-infinity
                            (norg/n-levels-being-shown-or-infinity)))
                      (if (= n-levels-being-shown-or-infinity
                             nomis/outline/c/plus-infinity)
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
                  (nomis/outline/c/prev-or-next-heading 1
                                                        (if (< n 0)
                                                            :backward
                                                          :forward)
                                                        sibling-or-peer
                                                        t))
                (tried-to-go-too-far
                  ()
                  (let* ((msg (concat (if (< n 0)
                                          "No previous heading at this level"
                                        "No next heading at this level")
                                      (cl-ecase sibling-or-peer
                                        (:sibling "")
                                        (:peer ", even across parents")))))
                    (nomis/popup/error-message msg)))
                (expand
                  ()
                  (if (null n-levels-or-nil)
                      (norg/expand-fully)
                    (norg/expand n-levels-or-nil t))))
        (nomis/outline/c/back-to-heading)
        (if (not (or (-norg/stepping-forward-on-last-but-not-first-child/must-be-at-boh)
                     (-norg/stepping-backward-on-first-but-not-last-child/must-be-at-boh)
                     ;; If we very recently did a `norg/step-xxxx-sibling` which
                     ;; tried to go too far and which so collapsed the current
                     ;; heading, and if now we're doing a `norg/step-xxxx-peer`,
                     ;; we're happy to do a step across the parent.
                     (-norg/step-sibling-then-step-peer-with-small-time-gap?)
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
                        (nomis/outline/c/up-heading 1))
                      (nomis/outline/c/collapse))
                    ;; Expand.
                    (expand))
                (progn
                  (nomis/outline/c/collapse)
                  (tried-to-go-too-far))))))))
    (setq -norg/most-recent-step-time (float-time))
    (message "n-levels = %s" (or n-levels-or-nil "all"))))

(defun norg/step-forward-sibling (n-levels-to-show-or-nil)
  "Move forward to the next heading at the same level, then expand it.
Stops at parent boundaries.
N-LEVELS-TO-SHOW-OR-NIL controls how many levels to expand; nil means fully."
  (-norg/step/impl 1 :sibling n-levels-to-show-or-nil))

(defun norg/step-backward-sibling (n-levels-to-show-or-nil)
  "Move backward to the previous heading at the same level, then expand it.
Stops at parent boundaries.
N-LEVELS-TO-SHOW-OR-NIL controls how many levels to expand; nil means fully."
  (-norg/step/impl -1 :sibling n-levels-to-show-or-nil))

(defun norg/step-forward-peer (n-levels-to-show-or-nil)
  "Move forward to the next heading at the same level, then expand it.
Can cross parent boundaries.
N-LEVELS-TO-SHOW-OR-NIL controls how many levels to expand; nil means fully."
  (-norg/step/impl 1 :peer n-levels-to-show-or-nil))

(defun norg/step-backward-peer (n-levels-to-show-or-nil)
  "Move backward to the previous heading at the same level, then expand it.
Can cross parent boundaries.
N-LEVELS-TO-SHOW-OR-NIL controls how many levels to expand; nil means fully."
  (-norg/step/impl -1 :peer n-levels-to-show-or-nil))

;;;; Info about trees

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
     (nomis/outline/c/level)))

(defun norg/n-levels-being-shown-or-infinity ()
  "The number of levels being shown from the current headline, or
infinity if it is fully expanded.
When in a body, \"current headline\" means the current body's parent headline."
  (- (norg/reduce-entries-from-point
      nomis/outline/c/plus-infinity
      #'(lambda ()
          (let* ((point-visible? (nomis/outline/c/visible?))
                 (level (nomis/outline/c/level)))
            (if (not point-visible?)
                (1- level)
              (cl-destructuring-bind (has-body?
                                      has-visible-body?
                                      has-invisible-body?)
                  (-norg/body-info)
                (if (or (not norg/show-bodies?)
                        (not has-body?)
                        has-visible-body?)
                    nomis/outline/c/plus-infinity
                  level)))))
      #'min)
     (nomis/outline/c/level)))

;;;; The idea of tree-info, and things that use it

(defun -norg/tree-info* ()
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
                                         (list* (point)
                                                (nomis/outline/c/level)
                                                (nomis/outline/c/visible?)
                                                (-norg/body-info)))))
         (just-did-a-body? nil))
    (cl-loop for ((prev-level prev-visible? . _)
                  . ((pos
                      level
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
             collect (a-hash-table :tree-info/pos     pos
                                   :tree-info/level   (1+ prev-level)
                                   :tree-info/visible? nil
                                   :tree-info/dummy?   t)

             when (not last?)
             collect (a-hash-table :tree-info/pos     pos
                                   :tree-info/level   level
                                   :tree-info/visible? visible?
                                   :tree-info/dummy?   nil)

             when has-body?
             collect (a-hash-table :tree-info/pos     pos
                                   :tree-info/level   (1+ level)
                                   :tree-info/visible? has-visible-body?
                                   :tree-info/dummy?   nil
                                   :tree-info/body?    t)

             when has-visible-body?
             collect (a-hash-table :tree-info/pos         pos
                                   :tree-info/level       (+ 2 level)
                                   :tree-info/visible?    nil
                                   :tree-info/dummy?      t
                                   :tree-info/body-extra? t)

             do (setq just-did-a-body? has-body?))))

(defun -norg/tree-info ()
  (let* ((res (-norg/tree-info*)))
    ;; (pp (mapcar (lambda (x)
    ;;               (list :pos         (a-get x :tree-info/pos)
    ;;                     :level       (a-get x :tree-info/level)
    ;;                     :visible?    (a-get x :tree-info/visible?)
    ;;                     :dummy?      (a-get x :tree-info/dummy?)
    ;;                     :body?       (a-get x :tree-info/body?)
    ;;                     :body-extra? (a-get x :tree-info/body-extra?)))
    ;;             res))
    res))

(defun norg/fully-expanded? ()
  "Is the tree beneath the current headline fully expanded?

When in a body, \"current headline\" means the current body's parent headline.

If `norg/show-bodies?' is truthy, this returns truthy only if all bodies
are visible. Otherwise body visibiity is not taken into account."
  (cl-loop for entry in (-norg/tree-info)
           when (not (a-get entry :tree-info/dummy?))
           always (if norg/show-bodies?
                      (a-get entry :tree-info/visible?)
                    (or (a-get entry :tree-info/visible?)
                        (a-get entry :tree-info/body?)))))

(defun norg/start-level-for-incremental-contract ()
  "The level to use when incrementally collapsing the current headline.
When in a body, \"current headline\" means the current body's parent headline."
  ;; Collapse the most-deeply-nested expanded level, and expand everything
  ;; else to that level.
  (let* ((deepest-visible-levels
          (cl-loop for (prev-entry entry . _rest)
                   on (cons (a-hash-table :tree-info/level   most-negative-fixnum
                                          :tree-info/visible? t)
                            (-norg/tree-info))
                   while entry
                   for prev-level    = (a-get prev-entry :tree-info/level)
                   for prev-visible? = (a-get prev-entry :tree-info/visible?)
                   for level         = (a-get entry :tree-info/level)
                   for visible?      = (a-get entry :tree-info/visible?)
                   when (and prev-visible?
                             (not visible?)
                             (> level prev-level))
                   collect prev-level))
         (v (apply #'max deepest-visible-levels)))
    (- v (nomis/outline/c/level))))

;;;; Operations on root

(defun norg/n-levels-below/root ()
  (norg/save-excursion-to-root
    (norg/n-levels-below)))

(defun norg/start-level-for-incremental-contract/root ()
  (norg/save-excursion-to-root
    (norg/start-level-for-incremental-contract)))

(defun norg/n-levels-being-shown-or-infinity/root ()
  (norg/save-excursion-to-root
    (norg/n-levels-being-shown-or-infinity)))

;;;; Operations on buffer

(defun norg/levels/max-in-buffer ()
  (let* ((sofar 0))
    (norg/mapc-entries-from-all-roots (lambda ()
                                        (setq sofar (max (norg/level-incl-body/must-be-at-boh)
                                                         sofar))))
    sofar))

(defun norg/n-levels-below/buffer ()
  (1- (norg/levels/max-in-buffer)))

(defun norg/start-level-for-incremental-contract/buffer ()
  (->> (norg/map-roots #'norg/start-level-for-incremental-contract)
       (apply #'max)))

(defun norg/n-levels-being-shown-or-infinity/buffer ()
  (->> (norg/map-roots #'norg/n-levels-being-shown-or-infinity)
       (apply #'min)))

;;;; Expanding and collapsing

;;;;; -norg/set-level-etc

(defun -norg/out-of-range (v maximum setting-kind current-value)
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
       (= current-value nomis/outline/c/plus-infinity)))))

(defvar *norg/wrap-expand-collapse? nil)
(defvar *-norg/allow-cycle-wrap-now?* nil)
(defvar *-norg/allow-cycle-wrap-timer* nil)

(defun -norg/cancel-cycle-to-zero-timer ()
  (when *-norg/allow-cycle-wrap-timer*
    (cancel-timer *-norg/allow-cycle-wrap-timer*)
    (setq *-norg/allow-cycle-wrap-timer* nil)
    (setq *-norg/allow-cycle-wrap-now?* nil)))

(defun -norg/allow-cycle-to-zero-for-a-while ()
  (setq *-norg/allow-cycle-wrap-now?* t)
  (let ((secs (if (boundp '*nomis/popup/duration*)
                  *nomis/popup/duration*
                1)))
    (setq *-norg/allow-cycle-wrap-timer*
          (run-at-time secs
                       nil
                       '-norg/cancel-cycle-to-zero-timer))))

(defun -norg/bring-within-range (v maximum)
  (let* ((min-allowed-value (if *expanding-parent?* 1 0)))
    (cl-flet ((normal-behaviour () (list (min (max min-allowed-value v)
                                              maximum)
                                         nil))
              (cycled-behaviour () (list (if (= v (1- min-allowed-value))
                                             maximum
                                           min-allowed-value)
                                         t)))
      (let* ((allow-cycle-wrap-now? *-norg/allow-cycle-wrap-now?*))
        (-norg/cancel-cycle-to-zero-timer)
        (if (or (= maximum 0)
                (not (or (= v (1- min-allowed-value))
                         (= v nomis/outline/c/plus-infinity))))
            (normal-behaviour)
          (if (not allow-cycle-wrap-now?)
              (progn
                (when *norg/wrap-expand-collapse?
                  (-norg/allow-cycle-to-zero-for-a-while))
                (normal-behaviour))
            ;; Don't cycle if we moved to another position that also
            ;; happens to be fully-expanded.
            ;; Don't cycle if we moved away and came back.
            (if (not (eq this-command (nomis/outline/c/last-command)))
                (normal-behaviour)
              (cycled-behaviour))))))))

(defun -norg/set-level-etc (new-value-action-fun
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
      (cl-destructuring-bind (new-level do-cycling?)
          (-norg/bring-within-range v maximum)
        (let* ((out-of-range? (and (not do-cycling?)
                                   (-norg/out-of-range v
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

;;;;; norg/show-children-from-point/xxxx support

(defun norg/show-children-from-point* (n) ; TODO You don't use the special negative arg thing. Simplify or get back that functionality.
  "Make point visible if it isn't already, and expand current headline to
n levels.

Details:

If N is not negative, expand to show N levels. Any headlines at level N
will be collapsed.

If N is negative, expand to show (abs N) levels, but do not hide anything
that is already being displayed."
  (nomis/outline/c/ensure-heading-shown)
  (let* ((collapse? (>= n 0))
         (n (abs n)))
    (when collapse?
      (nomis/outline/c/collapse))
    (norg/expand n)))

(defun -norg/show-children-from-point/set-level-etc (level
                                                     setting-kind
                                                     current-value)
  (-norg/set-level-etc #'norg/show-children-from-point*
                       level
                       (norg/n-levels-below)
                       (if *expanding-parent?*
                           "[%s / %s] from parent"
                         "[%s / %s]")
                       setting-kind
                       current-value))

;;;;; norg/show-children-from-point/xxxx

(defun norg/show-children-from-point (n)
  "Show N levels from the current headline, and collapse anything that's
at a higher level.
When in a body, \"current headline\" means the current body's parent headline."
  (let* ((v n))
    (-norg/show-children-from-point/set-level-etc v :no-check :dummy)))

(defun norg/show-children-from-point/set-min ()
  "Fully collapse the current headline.
When in a body, \"current headline\" means the current body's parent headline."
  (let* ((current-value (norg/start-level-for-incremental-contract))
         (v (if *expanding-parent?* 1 0)))
    (-norg/show-children-from-point/set-level-etc v :setting-min current-value)))

(defun norg/show-children-from-point/fully-expand ()
  "Fully expand the current headline.
When in a body, \"current headline\" means the current body's parent headline."
  (let* ((current-value (norg/n-levels-being-shown-or-infinity))
         (v :max))
    (-norg/show-children-from-point/set-level-etc v :setting-max current-value)))

(defun norg/show-children-from-point/incremental/less (n)
  "If `N` is not provided, collapse the current headline by one level.
If `N` is provided, set the number of child levels to `N`.
When in a body, \"current headline\" means the current body's parent headline."
  (if n
      (norg/show-children-from-point n)
    (-norg/show-children-from-point/set-level-etc
     (1- (norg/start-level-for-incremental-contract)) :less :dummy)))

(defun norg/show-children-from-point/incremental/more (n)
  "If `N` is not provided, expand the current headline by one level.
If `N` is provided, set the number of child levels to `N`.
When in a body, \"current headline\" means the current body's parent headline."
  (if n
      (norg/show-children-from-point n)
    (-norg/show-children-from-point/set-level-etc
     (1+ (norg/n-levels-being-shown-or-infinity)) :more :dummy)))

;;;;; norg/show-children-from-parent/xxxx support

(defvar *expanding-parent?* nil)

(defun norg/save-excursion-to-parent-and-then-show-point* (f)
  (prog1
      (let* ((*expanding-parent?* t))
        (norg/save-excursion-to-parent (funcall f)))
    (nomis/outline/c/ensure-heading-shown)))

(cl-defmacro norg/save-excursion-to-parent-and-then-show-point (&body body)
  (declare (indent 0))
  `(norg/save-excursion-to-parent-and-then-show-point* (lambda () ,@body)))

;;;;; norg/show-children-from-parent/xxxx

(defun norg/show-children-from-parent (n)
  "Like `norg/show-children-from-point`, but from the current entry's parent."
  (norg/save-excursion-to-parent-and-then-show-point
    (norg/show-children-from-point n)))

(defun norg/show-children-from-parent/set-min ()
  "Like `norg/show-children-from-point/set-min`, but from the
current entry's parent and showing one level."
  (norg/save-excursion-to-parent-and-then-show-point
    (norg/show-children-from-point/set-min)))

(defun norg/show-children-from-parent/fully-expand ()
  "Like `norg/show-children-from-point/fully-expand`, but from
the current entry's parent."
  (norg/save-excursion-to-parent-and-then-show-point
    (norg/show-children-from-point/fully-expand)))

(defun norg/show-children-from-parent/incremental/less (n)
  "If `N` is not provided, collapse the current headline's parent by one level.
Keep the parent expanded by at least one level.
If `N` is provided, set the number of child levels to `N`."
  (norg/save-excursion-to-parent-and-then-show-point
    (norg/show-children-from-point/incremental/less n)))

(defun norg/show-children-from-parent/incremental/more (n)
  "If `N` is not provided, expand the current headline's parent by one level.
If `N` is provided, set the number of child levels to `N`."
  (norg/save-excursion-to-parent-and-then-show-point
    (norg/show-children-from-point/incremental/more n)))

;;;;; norg/show-children-from-root/xxxx support

(defun norg/show-children-from-root* (n)
  "Call `norg/show-children-from-point*` on the current root headline, with N as
the parameter."
  (norg/save-excursion-to-root
    (norg/show-children-from-point* n)))

(defun -norg/show-children-from-root/set-level-etc (level
                                                    setting-kind
                                                    current-value)
  (-norg/set-level-etc #'norg/show-children-from-root*
                       level
                       (norg/n-levels-below/root)
                       "[%s of %s] from root"
                       setting-kind
                       current-value))

;;;;; norg/show-children-from-root/xxxx

(defun norg/show-children-from-root (n)
  (let* ((v n))
    (-norg/show-children-from-root/set-level-etc v :no-check :dummy)))

(defun norg/show-children-from-root/set-min ()
  (let* ((current-value (norg/start-level-for-incremental-contract/root))
         (v 0))
    (-norg/show-children-from-root/set-level-etc v :setting-min current-value)))

(defun norg/show-children-from-root/fully-expand ()
  (let* ((current-value (norg/n-levels-being-shown-or-infinity/root))
         (v :max))
    (-norg/show-children-from-root/set-level-etc v :setting-max current-value)))

(defun norg/show-children-from-root/incremental/less (n)
  "If `N` is not provided, collapse the current headline's root by one level.
If `N` is provided, set the number of child levels to `N`."
  (if n
      (norg/show-children-from-root n)
    (-norg/show-children-from-root/set-level-etc
     (1- (norg/start-level-for-incremental-contract/root)) :less :dummy)))

(defun norg/show-children-from-root/incremental/more (n)
  "If `N` is not provided, expand the current headline's root by one level.
If `N` is provided, set the number of child levels to `N`."
  (if n
      (norg/show-children-from-root n)
    (-norg/show-children-from-root/set-level-etc
     (1+ (norg/n-levels-being-shown-or-infinity/root)) :more :dummy)))

(defun norg/show-children-from-root/to-current-level ()
  (let* ((v (1- (nomis/outline/c/level t))))
    (-norg/show-children-from-root/set-level-etc v :no-check :dummy)))

;;;;; norg/show-children-from-all-roots/xxxx support

(defun norg/show-children-from-all-roots* (n)
  "Call `norg/show-children-from-point*` on all root headlines, with N as
the parameter."
  (norg/mapc-roots (lambda () (norg/show-children-from-point* n))))

(defun -norg/show-children-from-all-roots/set-level-etc (level
                                                         setting-kind
                                                         current-value)
  (-norg/set-level-etc #'norg/show-children-from-all-roots*
                       level
                       (norg/n-levels-below/buffer)
                       "[%s of %s] from all roots"
                       setting-kind
                       current-value))

;;;;; norg/show-children-from-all-roots/xxxx

(defun norg/show-children-from-all-roots (n)
  (let* ((v n))
    (-norg/show-children-from-all-roots/set-level-etc v :no-check :dummy)))

(defun norg/show-children-from-all-roots/set-min ()
  (let* ((current-value (norg/start-level-for-incremental-contract/buffer))
         (v 0))
    (-norg/show-children-from-all-roots/set-level-etc v :setting-min current-value)))

(defun norg/show-children-from-all-roots/fully-expand ()
  (let* ((current-value (norg/n-levels-being-shown-or-infinity/buffer))
         (v :max))
    (-norg/show-children-from-all-roots/set-level-etc v :setting-max current-value)))

(defun norg/show-children-from-all-roots/incremental/less (n)
  "If `N` is not provided, collapse all roots by one level.
If `N` is provided, set the number of child levels to `N`."
  (if n
      (norg/show-children-from-all-roots n)
    (-norg/show-children-from-all-roots/set-level-etc
     (1- (norg/start-level-for-incremental-contract/buffer)) :less :dummy)))

(defun norg/show-children-from-all-roots/incremental/more (n)
  "If `N` is not provided, expand all roots by one level.
If `N` is provided, set the number of child levels to `N`."
  (if n
      (norg/show-children-from-all-roots n)
    (-norg/show-children-from-all-roots/set-level-etc
     (1+ (norg/n-levels-being-shown-or-infinity/buffer)) :more :dummy)))

(defun norg/show-children-from-all-roots/to-current-level ()
  (let* ((v (1- (nomis/outline/c/level t))))
    (-norg/show-children-from-all-roots/set-level-etc v :no-check :dummy)))

;;; End

(provide 'nomis-tree-impl)

;;; norg.el ends here
