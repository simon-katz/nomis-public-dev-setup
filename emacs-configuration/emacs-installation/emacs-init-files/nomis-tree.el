;;; nomis-tree.el --- A protocol for navigating trees in files  -*- lexical-binding: t; -*-

;;; Commentary:

;; A layer on top of outline mode providing additional functionality for
;; navigating and for expanding & collapsing. Works with org mode too.

;;; Code:

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

;;;; Require things

(require 'a)
(require 'cl-lib)
(require 'dash)
(require 'nomis-msg)
(require 'nomis-outline-wrappers)
(require 'nomis-popup)
(require 'nomis-scrolling)
(require 'nomis-tree-lineage-specs)
(require 'outline)
(require 's)

;;;; nomis/tree-mode

(defvar nomis/tree-mode-map
  (make-sparse-keymap)
  "Keymap for `nomis/tree-mode'.")

(define-minor-mode nomis/tree-mode
  "Nomis-Tree Minor Mode"
  :group 'nomis/tree
  :keymap nomis/tree-mode-map
  ;; This minor mode exists only as a place to define key bindings, so we don't
  ;; need to do anything when turning on and off.
  )

;;;; Turn on the mode

(add-hook 'org-mode-hook 'nomis/tree-mode)
(add-hook 'outline-minor-mode-hook 'nomis/tree-mode)

;;;; Utilities

;;;;; -nomis/tree/command

(defvar -nomis/tree/prev-command-timestamp -9999)
(defvar *-nomis/tree/in-command?* nil)

(defun -nomis/tree/command* (opts f)
  (if *-nomis/tree/in-command?*
      (funcall f)
    (let* ((start-time (float-time))
           (*-nomis/tree/in-command?* t))
      (cl-flet* ((do-it ()
                   (nomis/scrolling/with-maybe-maintain-line-no-in-window
                     (prog1
                         (funcall f)
                       (setq -nomis/tree/prev-command-timestamp start-time)))))
        (if (or mark-active
                (a-get opts :no-push-mark))
            (do-it)
          (let* ((start-point (point)))
            (prog1
                (do-it)
              (let* ((end-point (point)))
                (unless (= start-point end-point)
                  (push-mark start-point
                             ;; Use `nomsg` arg in case `f` has displayed
                             ;; a message already.
                             t))))))))))

(cl-defmacro -nomis/tree/command (opts &body body)
  "A wrapper for certain commands. Nested calls just run the body."
  (declare (indent 1))
  `(-nomis/tree/command* ,opts (lambda () ,@body)))

;;;;; Time values

(defconst -nomis/tree/nav+lineage/sibling-then-peer-small-delay-s
  (if (boundp '*nomis/popup/duration*)
      *nomis/popup/duration*
    1))

(defconst -nomis/tree/allow-wrap-duration
  ;; This should be smaller than the OS's initial-delay-for-repeat-key value,
  ;; otherwise we get wrapping when pressing-and-holding the TAB key on
  ;; an already-fully-expanded heading.
  0.25)

(defconst -nomis/tree/repeat-key-assumed-interval-s 0.1)

(defun -nomis/tree/repeat-key-likely-used? ()
  ;; Unfortunately there's no good way to determine whether this is a first
  ;; repeat (after the long initial delay). This means that if we are already
  ;; fully expanded when the user starts, we will allow cycling.
  ;;
  ;; Ah, it's OK if we make `-nomis/tree/allow-wrap-duration` smaller
  ;; than the repeat-initial-delay value.
  (< (float-time)
     (+ -nomis/tree/prev-command-timestamp
        -nomis/tree/repeat-key-assumed-interval-s)))

;;;; Tailor other functionality

;;;;; what-cursor-position

;; Add outline level to the output of `what-cursor-position`.

(defvar nomis/tree/add-info-to-what-cursor-position?
  t
  "Control whether we add additional info to the message produced by
`what-cursor-position`. This is here just in case someone might be parsing the
message and in case adding outline level messes things up.")

(defvar *-nomis/tree/in-what-cursor-position?* nil)

(advice-add 'what-cursor-position
            :around
            (lambda (orig-fun &rest args)
              (let* ((*-nomis/tree/in-what-cursor-position?* t))
                (apply orig-fun args)))
            '((name . nomis/tree/add-level-info)))

(advice-add 'message
            :around
            (lambda (orig-fun &rest args)
              (if (and nomis/tree/add-info-to-what-cursor-position?
                       (or (nomis/outline/w/org-mode?)
                           (nomis/outline/w/outline-mode?))
                       *-nomis/tree/in-what-cursor-position?*)
                  (let* ((format-string
                          (concat "Level: %s  " (cl-first args)))
                         (format-args
                          (append
                           (list (nomis/tree/current-level-or-error-string))
                           (cl-rest args)))
                         (s (apply #'format format-string format-args)))
                    (funcall orig-fun "%s" s)
                    (nomis/popup/message "%s" s))
                (apply orig-fun args)))
            '((name . nomis/tree/add-level-info)))

;;;; Whether to show bodies

(defvar-local -nomis/tree/show-bodies? t)

(defun nomis/tree/toggle-show-bodies ()
  (interactive)
  (setq -nomis/tree/show-bodies? (not -nomis/tree/show-bodies?))
  (if -nomis/tree/show-bodies?
      (nomis/outline/w/show-bodies)
    (nomis/outline/w/hide-bodies))
  (message "show-bodies? set to %s" -nomis/tree/show-bodies?))

;;;; Some wrappers for outline functionality

;;;;; Basic stuff

(defun -nomis/tree/body-info/boh ()
  (save-excursion
    (let* ((has-body? (-nomis/outline/w/has-body?/boh)))
      ;; Go to end of heading rather than end of preface.
      ;; Without this, if we have a link at the end of the body (and if links
      ;; are being displayed in the usual way so that the actual link text is
      ;; invisible), we don't know whether the body is being displayed.
      ;; This newline char being visible or not tells us what we want to know.
      (nomis/outline/w/end-of-heading)
      (let* ((point-invisible? (nomis/outline/w/invisible?))
             (has-visible-body? (and has-body?
                                     (not point-invisible?)))
             (has-invisible-body? (and has-body?
                                       point-invisible?)))
        (list has-body?
              has-visible-body?
              has-invisible-body?)))))

(defun nomis/tree/level-incl-any-body/boh ()
  (let* ((heading-level (nomis/outline/w/level/boh)))
    (+ heading-level
       (if (and -nomis/tree/show-bodies?
                (-nomis/outline/w/has-body?/boh))
           1
         0))))

(defun -nomis/tree/in-body? ()
  (> (point)
     (save-excursion
       (nomis/outline/w/back-to-heading)
       (nomis/outline/w/end-of-heading)
       (point))))

(defun nomis/tree/current-level-or-error-string ()
  (if (nomis/outline/w/before-first-heading?)
      "Before first heading"
    (nomis/outline/w/level/inc-if-in-body)))

(defun nomis/tree/goto-root ()
  (while (nomis/outline/w/up-heading* 1 nil t)))

(cl-defmacro nomis/tree/save-excursion-to-root (&body body)
  (declare (indent 0))
  `(save-excursion
     (nomis/tree/goto-root)
     ,@body))

(cl-defmacro nomis/tree/save-excursion-to-parent (&body body)
  (declare (indent 0))
  `(save-excursion
     (nomis/outline/w/back-to-heading)
     (nomis/outline/w/up-heading* 1)
     ,@body))

;;;;; Support for do-ing and mapping

(defun -nomis/tree/mapc-headings-satisfying (pred-of-no-args fun)
  (save-excursion
    (cl-flet ((call-fun-when-pred-is-satisfied
                ()
                (when (funcall pred-of-no-args)
                  (funcall fun))))
      (goto-char (point-min))
      (unless (nomis/outline/w/on-heading?)
        (nomis/outline/w/next-heading))
      (when (nomis/outline/w/on-heading?)
        (call-fun-when-pred-is-satisfied)
        (while (progn
                 (nomis/outline/w/next-heading)
                 (not (eobp)))
          (call-fun-when-pred-is-satisfied)))))
  nil)

(defun -nomis/tree/convert-mapc-fun-to-map-fun (do-fun)
  (lambda (fun)
    (let* ((acc '()))
      (funcall do-fun (lambda ()
                        (push (funcall fun)
                              acc)))
      (nreverse acc))))

;;;;; Do-ing

(defun nomis/tree/mapc-entries-from-point (fun)
  "Call FUN for the current heading and for each heading below the current
heading.
When in a body, \"current heading\" means the current body's parent heading."
  (save-excursion
    (nomis/outline/w/map-tree fun))
  nil)

(defun nomis/tree/mapc-entries-from-root (fun)
  (nomis/tree/save-excursion-to-root
    (nomis/tree/mapc-entries-from-point fun)))

(defun nomis/tree/mapc-roots (fun)
  (-nomis/tree/mapc-headings-satisfying (lambda ()
                                          (= (nomis/outline/w/level/boh)
                                             (nomis/outline/w/top-level-level)))
                                        fun))

(defun nomis/tree/mapc-entries-from-all-roots (fun)
  (-nomis/tree/mapc-headings-satisfying (lambda () t)
                                        fun)
  nil)

;;;;; Mapping

(defun nomis/tree/map-entries-from-point (fun)
  (funcall (-nomis/tree/convert-mapc-fun-to-map-fun #'nomis/tree/mapc-entries-from-point)
           fun))

(defun nomis/tree/map-entries-from-root (fun)
  (funcall (-nomis/tree/convert-mapc-fun-to-map-fun #'nomis/tree/mapc-entries-from-root)
           fun))

(defun nomis/tree/map-roots (fun)
  (funcall (-nomis/tree/convert-mapc-fun-to-map-fun #'nomis/tree/mapc-roots)
           fun))

(defun nomis/tree/map-entries-from-all-roots (fun)
  (funcall (-nomis/tree/convert-mapc-fun-to-map-fun #'nomis/tree/mapc-entries-from-all-roots)
           fun))

;;;;; Reducing (add more here if and when needed)

(cl-defun nomis/tree/reduce-entries-from-point (initial-value
                                                value-fun
                                                reducing-function
                                                &optional
                                                (pred-of-no-args (lambda () t)))
  (let* ((sofar initial-value))
    (nomis/tree/mapc-entries-from-point (lambda ()
                                          (when (funcall pred-of-no-args)
                                            (let* ((v (funcall value-fun)))
                                              (setq sofar
                                                    (funcall reducing-function
                                                             sofar
                                                             v))))))
    sofar))

;;;;; Expanding and collapsing

(defun nomis/tree/expand (n &optional collapse-first?)
  "Expand N levels below the current heading. If COLLAPSE-FIRST? is non-nil,
collapse the tree first so that only N levels are shown. When in
a body, \"current heading\" means the current body's parent
heading."
  (when collapse-first? (nomis/outline/w/collapse))
  (nomis/outline/w/show-children n)
  (when -nomis/tree/show-bodies?
    (let* ((level (nomis/outline/w/level/no-inc-if-in-body)))
      (nomis/tree/mapc-entries-from-point
       #'(lambda ()
           (when (< (- (nomis/outline/w/level/boh)
                       level)
                    n)
             (nomis/outline/w/show-entry)))))))

(defun nomis/tree/expand-fully ()
  (nomis/tree/expand 1000) ; TODO magic number
  )

;;;; Search heading text

(defun -nomis/tree/grab-heading-text ()
  (save-excursion
    ;; Jump to first word of heading
    (nomis/outline/w/back-to-heading)
    (forward-word)
    (backward-word)
    ;; Grab text of heading
    (let* ((beg (point))
           (end (progn
                  (nomis/outline/w/end-of-line)
                  (point))))
      (if (not (< beg end))
          (error "No heading here")
        (let* ((text (buffer-substring beg end)))
          (set-text-properties 0 (length text) nil text)
          text)))))

(defvar-local -nomis/tree/search-heading-text/text nil)

(defun -nomis/tree/search-heading-text/search (again?)
  (cl-assert (not (null -nomis/tree/search-heading-text/text)))
  (cl-flet ((search-for-text
              ()
              (search-backward (cl-case 1
                                 (1
                                  ;; Simply look for the text.
                                  -nomis/tree/search-heading-text/text)
                                 (2
                                  ;; Just find refs. But is awkward -- you need
                                  ;; to press M-. to go back. I wanted H-. but
                                  ;; I'm already using that for something else.
                                  (s-concat "[[*"
                                            -nomis/tree/search-heading-text/text
                                            "]")))
                               nil
                               t)))
    (when again?
      (nomis/outline/w/back-to-heading))
    (or (search-for-text)
        (progn
          (goto-char (point-max))
          (search-for-text))))
  (nomis/outline/w/ensure-heading-shown))

(defun nomis/tree/search-heading-text ()
  (interactive)
  (-nomis/tree/command
      nil
    (setq -nomis/tree/search-heading-text/text (-nomis/tree/grab-heading-text))
    (push-mark)
    (-nomis/tree/search-heading-text/search nil)))

(defun nomis/tree/search-heading-text-again ()
  (interactive)
  (-nomis/tree/command
      (a-list :no-push-mark t)
    (if (null -nomis/tree/search-heading-text/text)
        (error "nomis/tree/search-heading-text hasn't been called yet")
      (-nomis/tree/search-heading-text/search t))))

;;;; Navigation

;;;;; nomis/tree/up-heading

(defun nomis/tree/up-heading (n)
  (interactive "p")
  (-nomis/tree/command
      nil
    (nomis/outline/w/up-heading n)))

;;;;; Forward and backward

(defun nomis/tree/previous-heading ()
  "Move backward to the previous heading at any level.

When the target is invisible, make it visible.

If there is no previous heading, display a popup message."
  (interactive)
  (-nomis/tree/command
      nil
    (when (nomis/outline/w/prev-or-next-heading 1 :backward :any-level)
      (nomis/outline/w/ensure-heading-shown))))

(defun nomis/tree/next-heading ()
  "Move forward to the next heading at any level.

When the target is invisible, make it visible.

If there is no next heading, display a popup message."
  (interactive)
  (-nomis/tree/command
      nil
    (when (nomis/outline/w/prev-or-next-heading 1 :forward :any-level)
      (nomis/outline/w/ensure-heading-shown))))

(defun nomis/tree/previous-sibling ()
  "Move backward one sibling.

When the target is invisible, make it visible.

If there is no previous sibling, display a popup message."
  (interactive)
  (-nomis/tree/command
      nil
    (when (nomis/outline/w/prev-or-next-heading 1 :backward :sibling)
      (nomis/outline/w/ensure-heading-shown))))

(defun nomis/tree/next-sibling ()
  "Move forward one sibling.

When the target is invisible, make it visible.

If there is no next sibling, display a popup message."
  (interactive)
  (-nomis/tree/command
      nil
    (when (nomis/outline/w/prev-or-next-heading 1 :forward :sibling)
      (nomis/outline/w/ensure-heading-shown))))

(defun nomis/tree/previous-peer ()
  "Move backward one peer.

When the target is invisible, make it visible.

If there is no previous peer, display a popup message."
  (interactive)
  (-nomis/tree/command
      nil
    (when (nomis/outline/w/prev-or-next-heading 1 :backward :peer)
      (nomis/outline/w/ensure-heading-shown))))

(defun nomis/tree/next-peer ()
  "Move forward one peer.

When the target is invisible, make it visible.

If there is no next peer, display a popup message."
  (interactive)
  (-nomis/tree/command
      nil
    (when (nomis/outline/w/prev-or-next-heading 1 :forward :peer)
      (nomis/outline/w/ensure-heading-shown))))

;;;; Nav+lineage

;;;;; Preamble

;; TODO This uses `nomis/tree/fully-expanded?`, and so belongs later in
;;      the file.

(defvar *-nomis/tree/inhibit-set-level-etc-message?* nil)

;;;;; Nav+lineage settings

;;;;;; Ancestors

(defvar -nomis/tree/nav+lineage/ancestors-approach ; Buffer-local? No.
  :nav+lineage/ancestors/all-roots-to-current-level
  "What to do with ancestors in `-nomis/tree/nav+lineage/show-lineage'.

One of:
- `:nav+lineage/ancestors/leave-as-is` (but show ancestors if hidden)
- `:nav+lineage/ancestors/all-roots-to-current-level`
- `:nav+lineage/ancestors/fat'
- `:nav+lineage/ancestors/thin`.")

(defconst -nomis/tree/nav+lineage/ancestors-approach/pairs
  '((:nav+lineage/ancestors/leave-as-is                . "leave-as-is")
    (:nav+lineage/ancestors/all-roots-to-current-level . "all roots to current level")
    (:nav+lineage/ancestors/fat                        . "fat")
    (:nav+lineage/ancestors/thin                       . "thin")))

(defun -nomis/tree/nav+lineage/ancestors-approach-text ()
  (cl-loop for (sym . text) in -nomis/tree/nav+lineage/ancestors-approach/pairs
           when (eq sym -nomis/tree/nav+lineage/ancestors-approach)
           return text
           finally (error "Unexpected value for -nomis/tree/nav+lineage/ancestors-approach")))

;;;;;; Children

(defvar -nomis/tree/nav+lineage/n-child-levels-to-show nil) ; Buffer-local? No.

;;;;; -nomis/tree/nav+lineage/show-lineage

(defun -nomis/tree/nav+lineage/show-lineage (&optional n-or-nil)
  (let* ((n-levels-or-nil (or n-or-nil
                              -nomis/tree/nav+lineage/n-child-levels-to-show)))
    (cl-ecase -nomis/tree/nav+lineage/ancestors-approach
      (:nav+lineage/ancestors/leave-as-is
       ;; Show self and ancestors to provide context.
       (nomis/outline/w/make-self-and-ancestors-visible))
      (:nav+lineage/ancestors/all-roots-to-current-level
       (let* ((*-nomis/tree/inhibit-set-level-etc-message?* t))
         (nomis/tree/show-children-from-all-roots/to-current-level)))
      (:nav+lineage/ancestors/fat
       (nomis/tree/ls/show-lineage
        nomis/tree/ls/spec/hide-all--fat-ancestors--no-children))
      (:nav+lineage/ancestors/thin
       (nomis/tree/ls/show-lineage
        nomis/tree/ls/spec/hide-all--thin-ancestors--no-children)))
    (if (null n-levels-or-nil)
        (nomis/tree/expand-fully)
      (nomis/tree/expand n-levels-or-nil t))
    (message "ancestors: %s  n-children: %s"
             (-nomis/tree/nav+lineage/ancestors-approach-text)
             (or n-levels-or-nil "all"))))

;;;;; nomis/tree/nav+lineage/set-n-child-levels-to-show

(defun nomis/tree/nav+lineage/set-n-child-levels-to-show (n-or-nil)
  (interactive "P")
  (-nomis/tree/command
      nil
    (when (null n-or-nil)
      (setq n-or-nil
            (let* ((s (read-string
                       (cl-format nil
                                  "Number of levels to show ~
                                 (empty string for all children) ~
                                 (currently ~s): "
                                  -nomis/tree/nav+lineage/n-child-levels-to-show))))
              (if (member (s-trim s) '("" "nil"))
                  nil
                (string-to-number s)))))
    (setq -nomis/tree/nav+lineage/n-child-levels-to-show
          (if (null n-or-nil) n-or-nil (max 0 (floor n-or-nil))))
    (-nomis/tree/nav+lineage/show-lineage)
    (message "nav+lineage n-child-levels-to-show set to %s"
             -nomis/tree/nav+lineage/n-child-levels-to-show)))

;;;;; nomis/tree/nav+lineage/set-ancestors-approach

(defun -nomis/tree/nav+lineage/set-ancestors-approach* (v)
  (setq -nomis/tree/nav+lineage/ancestors-approach v)
  (-nomis/tree/nav+lineage/show-lineage)
  (message "nav+lineage ancestors-approach set to %s"
           (-nomis/tree/nav+lineage/ancestors-approach-text)))

(defun nomis/tree/nav+lineage/set-ancestors-approach ()
  (interactive)
  (-nomis/tree/command
      nil
    (let* ((curr-string (-nomis/tree/nav+lineage/ancestors-approach-text))
           (prompt (format "Choose ancestors approach — currently %S: "
                           curr-string))
           (response
            (ido-completing-read
             prompt
             (-map #'cdr -nomis/tree/nav+lineage/ancestors-approach/pairs)
             nil
             t
             nil
             'nomis/tree/nav+lineage/set-ancestors-approach/prompt-history
             curr-string))
           (v (car (rassoc response
                           -nomis/tree/nav+lineage/ancestors-approach/pairs))))
      (-nomis/tree/nav+lineage/set-ancestors-approach* v))))

;;;;; On first/last sibling/peer

(defun nomis/tree/on-first-sibling?/boh ()
  "Truthy if on first or only sibling."
  (save-excursion
    (let ((starting-point (point)))
      (nomis/outline/w/prev-or-next-heading 1 :backward :sibling t)
      (= (point) starting-point))))

(defun nomis/tree/on-last-sibling?/boh ()
  "Truthy if on last or only sibling."
  (save-excursion
    (let ((starting-point (point)))
      (nomis/outline/w/prev-or-next-heading 1 :forward :sibling t)
      (= (point) starting-point))))

(defun nomis/tree/on-first-peer?/boh ()
  "Truthy if on first or only peer."
  (save-excursion
    (let ((starting-point (point)))
      (nomis/outline/w/prev-or-next-heading 1 :backward :peer t)
      (= (point) starting-point))))

(defun nomis/tree/on-last-peer?/boh ()
  "Truthy if on last or only peer."
  (save-excursion
    (let ((starting-point (point)))
      (nomis/outline/w/prev-or-next-heading 1 :forward :peer t)
      (= (point) starting-point))))

;;;;; Nav+lineage algorithm

(defun -nomis/tree/nav+lineage/doing-sibling-final-not-lone?/boh ()
  "Return non-nil if doing sibling nav+lineage from \"final\" non-lone sibling.

\"Final\" means \"last\" for a forward navigation and \"first\" for a
backward navigation."
  (let* ((first? (nomis/tree/on-first-sibling?/boh))
         (last? (nomis/tree/on-last-sibling?/boh)))
    (cond ((eq this-command 'nomis/tree/nav+lineage/backward-sibling)
           (and first? (not last?)))
          ((eq this-command 'nomis/tree/nav+lineage/forward-sibling)
           (and last? (not first?))))))

(defun -nomis/tree/nav+lineage/doing-peer-final-not-lone?/boh ()
  "Return non-nil if doing peer nav+lineage from \"final\" non-lone peer.

\"Final\" means \"last\" for a forward navigation and \"first\" for a
backward navigation."
  (let* ((first? (nomis/tree/on-first-peer?/boh))
         (last? (nomis/tree/on-last-peer?/boh)))
    (cond ((eq this-command 'nomis/tree/nav+lineage/backward-peer)
           (and first? (not last?)))
          ((eq this-command 'nomis/tree/nav+lineage/forward-peer)
           (and last? (not first?))))))

(defun -nomis/tree/nav+lineage/doing-same-level-final-not-lone?/boh ()
  "Return non-nil if doing same-level nav+lineage from \"final\" non-lone item.

\"Final\" means \"last\" for a forward navigation and \"first\" for a
backward navigation."
  (or (-nomis/tree/nav+lineage/doing-sibling-final-not-lone?/boh)
      (-nomis/tree/nav+lineage/doing-peer-final-not-lone?/boh)))

(defun -nomis/tree/nav+lineage/sibling-then-peer? ()
  (let ((cmds (list (nomis/outline/w/last-command)
                    this-command)))
    (member cmds
            '((nomis/tree/nav+lineage/forward-sibling
               nomis/tree/nav+lineage/forward-peer)
              (nomis/tree/nav+lineage/backward-sibling
               nomis/tree/nav+lineage/backward-peer)))))

(defun -nomis/tree/nav+lineage/sibling-then-peer-with-small-time-gap? ()
  (and (-nomis/tree/nav+lineage/sibling-then-peer?)
       (< (float-time)
          (+ -nomis/tree/prev-command-timestamp
             -nomis/tree/nav+lineage/sibling-then-peer-small-delay-s))))

(defun -nomis/tree/nav+lineage/impl (n kind n-or-nil)
  (let* ((n-levels-or-nil (or n-or-nil
                              -nomis/tree/nav+lineage/n-child-levels-to-show)))
    (cl-flet* ((expanded-to-desired-level? ()
                 (if (null n-levels-or-nil)
                     (nomis/tree/fully-expanded?)
                   (let* ((n-levels-being-shown-or-infinity
                           (nomis/tree/n-levels-being-shown-or-infinity)))
                     (if (= n-levels-being-shown-or-infinity
                            nomis/outline/w/plus-infinity)
                         ;; The tree is fully expanded at point. This is truthy
                         ;; if the number of levels below is less than or equal
                         ;; to the desired number of levels to show.
                         (<= (nomis/tree/n-levels-below)
                             n-levels-or-nil)
                       ;; The tree is not fully expanded at point. This is
                       ;; truthy if the number of levels being shown is the same
                       ;; as the desired number of levels.
                       (= n-levels-being-shown-or-infinity
                          n-levels-or-nil)))))
               (try-to-nav ()
                 (nomis/outline/w/prev-or-next-heading 1
                                                       (if (< n 0)
                                                           :backward
                                                         :forward)
                                                       kind))
               (show-lineage ()
                 (-nomis/tree/nav+lineage/show-lineage n-or-nil))
               (try-to-nav-then-show-lineage ()
                 (let* ((starting-point (point)))
                   (nomis/outline/w/collapse)
                   (try-to-nav)
                   (let* ((moved? (not (= (point) starting-point))))
                     (when moved?
                       (show-lineage))))))
      (nomis/outline/w/back-to-heading)
      (cond ((-nomis/tree/nav+lineage/doing-same-level-final-not-lone?/boh)
             ;; This will display a can't-move message, then collapse:
             (try-to-nav-then-show-lineage))
            ((-nomis/tree/nav+lineage/sibling-then-peer-with-small-time-gap?)
             ;; If we very recently did a `nomis/tree/nav+lineage/xxxx-sibling`
             ;; which tried to go too far and which so collapsed the current
             ;; heading, and if now we're doing
             ;; a `nomis/tree/nav+lineage/xxxx-peer`, we're happy to do
             ;; a nav+lineage/peer -- we'll forego the normal
             ;; expand-before-navigating:
             (try-to-nav-then-show-lineage))
            (t
             ;; Expand-before-navigating: If expanded, do nav+lineage.
             ;; Otherwise expand (and repeating the command will do
             ;; nav+lineage).
             (if (expanded-to-desired-level?)
                 (try-to-nav-then-show-lineage)
               (show-lineage)))))))

;;;;; Nav+lineage commands

(defun nomis/tree/nav+lineage/backward-any-level (n-or-nil)
  "Move backward to the previous heading at any level, then expand it.
N-OR-NIL controls how many levels to expand; nil means fully."
  (interactive "P")
  (-nomis/tree/command
      nil
    (-nomis/tree/nav+lineage/impl -1 :any-level n-or-nil)))

(defun nomis/tree/nav+lineage/forward-any-level (n-or-nil)
  "Move forward to the next heading at any level, then expand it.
N-OR-NIL controls how many levels to expand; nil means fully."
  (interactive "P")
  (-nomis/tree/command
      nil
    (-nomis/tree/nav+lineage/impl 1 :any-level n-or-nil)))

(defun nomis/tree/nav+lineage/backward-sibling (n-or-nil)
  "Move backward to the previous sibling, then expand it.
N-OR-NIL controls how many levels to expand; nil means fully."
  (interactive "P")
  (-nomis/tree/command
      nil
    (-nomis/tree/nav+lineage/impl -1 :sibling n-or-nil)))

(defun nomis/tree/nav+lineage/forward-sibling (n-or-nil)
  "Move forward to the next sibling, then expand it.
N-OR-NIL controls how many levels to expand; nil means fully."
  (interactive "P")
  (-nomis/tree/command
      nil
    (-nomis/tree/nav+lineage/impl 1 :sibling n-or-nil)))

(defun nomis/tree/nav+lineage/backward-peer (n-or-nil)
  "Move backward to the previous peer, then expand it.
N-OR-NIL controls how many levels to expand; nil means fully."
  (interactive "P")
  (-nomis/tree/command
      nil
    (-nomis/tree/nav+lineage/impl -1 :peer n-or-nil)))

(defun nomis/tree/nav+lineage/forward-peer (n-or-nil)
  "Move forward to the next peer, then expand it.
N-OR-NIL controls how many levels to expand; nil means fully."
  (interactive "P")
  (-nomis/tree/command
      nil
    (-nomis/tree/nav+lineage/impl 1 :peer n-or-nil)))

;;;; Info about trees

(defun nomis/tree/deepest-level-below ()
  "The deepest level that exists below the current heading.
When in a body, \"current heading\" means the current body's parent heading.
Example: If we are at level 5 and there are 2 further levels below, the result
is 7."
  (nomis/tree/reduce-entries-from-point 0
                                        #'nomis/tree/level-incl-any-body/boh
                                        #'max))

(defun nomis/tree/n-levels-below ()
  "The number of levels that exist below the current heading.
When in a body, \"current heading\" means the current body's parent heading.
Example: If we are at level 5 and there are 2 further levels below, the result
is 2."
  (- (nomis/tree/deepest-level-below)
     (nomis/outline/w/level/no-inc-if-in-body)))

(defun nomis/tree/n-levels-being-shown-or-infinity ()
  "The number of levels being shown from the current heading, or
infinity if it is fully expanded.
When in a body, \"current heading\" means the current body's parent heading."
  (- (nomis/tree/reduce-entries-from-point
      nomis/outline/w/plus-infinity
      #'(lambda ()
          (let* ((point-invisible? (nomis/outline/w/invisible?))
                 (level (nomis/outline/w/level/boh)))
            (cond (point-invisible?
                   (1- level))
                  ((not -nomis/tree/show-bodies?)
                   nomis/outline/w/plus-infinity)
                  (t
                   (cl-destructuring-bind (has-body?
                                           has-visible-body?
                                           _has-invisible-body?)
                       (-nomis/tree/body-info/boh)
                     (if (and has-body?
                              (not has-visible-body?))
                         level
                       nomis/outline/w/plus-infinity))))))
      #'min)
     (nomis/outline/w/level/no-inc-if-in-body)))

;;;; The idea of tree-info, and things that use it

(defun -nomis/tree/tree-info* ()
  "Tree info for the current heading.
When in a body, \"current heading\" means the current body's parent heading."
  ;; This is rather expensive, because the value returned by
  ;; `nomis/tree/map-entries-from-point` is processed further and
  ;; discarded. You could do more in the fun passed to
  ;; `nomis/tree/map-entries-from-point`, but it's fiddly because:
  ;; - You want to collect multiple items per iteration.
  ;;   (Solution: use nconc on lists.)
  ;; - You want to look at two headings at a time.
  ;;   (Solution: record previous item in a piece of mutable state.)
  ;; - You'd need to do some fixing up at the end to add that final dummy
  ;;   entry when the final item is visible.
  (let* ((just-did-a-body? nil))
    ;; We're using this `cl-loop` pattern:
    ;;   `(cl-loop for (x y) on (cons nil (list 1 2 3)) collect (list x y))`
    ;;   => `((nil 1) (1 2) (2 3) (3 nil))`
    (cl-loop for (prev-entry entry)
             on (cons nil ; dummy initial entry
                      (nomis/tree/map-entries-from-point
                       (lambda ()
                         (cl-list* (point)
                                   (nomis/outline/w/level/boh)
                                   (not (nomis/outline/w/invisible?))
                                   (-nomis/tree/body-info/boh)))))

             for first? = t then nil
             for post-last-tidy-up? = (null entry)
             for (_ prev-level prev-visible? . _) = prev-entry
             for (pos level visible? has-body?
                      has-visible-body? _has-invisible-body?) = entry
             for prev-was-visible-leaf? = (and (not first?)
                                               (not just-did-a-body?)
                                               prev-visible?
                                               (or post-last-tidy-up?
                                                   (<= level prev-level)))

             when prev-was-visible-leaf?
             collect (a-hash-table :tree-info/pos     pos
                                   :tree-info/level   (1+ prev-level)
                                   :tree-info/visible? nil
                                   :tree-info/dummy?   t)

             while (not post-last-tidy-up?)

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

(defun -nomis/tree/tree-info ()
  (let* ((res (-nomis/tree/tree-info*)))
    ;; (pp (mapcar (lambda (x)
    ;;               (list :pos         (a-get x :tree-info/pos)
    ;;                     :level       (a-get x :tree-info/level)
    ;;                     :visible?    (a-get x :tree-info/visible?)
    ;;                     :dummy?      (a-get x :tree-info/dummy?)
    ;;                     :body?       (a-get x :tree-info/body?)
    ;;                     :body-extra? (a-get x :tree-info/body-extra?)))
    ;;             res))
    res))

(defun nomis/tree/fully-expanded? ()
  "Is the tree beneath the current heading fully expanded?

When in a body, \"current heading\" means the current body's parent heading.

If `-nomis/tree/show-bodies?' is truthy, this returns truthy only if all
bodies are visible. Otherwise body visibiity is not taken into account."
  (cl-loop for entry in (-nomis/tree/tree-info)
           always (or (a-get entry :tree-info/dummy?)
                      (a-get entry :tree-info/visible?)
                      (and (a-get entry :tree-info/body?)
                           (not -nomis/tree/show-bodies?)))))

(defun nomis/tree/start-level-for-incremental-contract ()
  "The level to use when incrementally collapsing the current heading.
When in a body, \"current heading\" means the current body's parent heading."
  ;; Collapse the most-deeply-nested expanded level, and expand everything
  ;; else to that level.
  (let* ((deepest-visible-levels
          (cl-loop for (prev-entry entry . _rest)
                   on (cons (a-hash-table :tree-info/level   most-negative-fixnum
                                          :tree-info/visible? t)
                            (-nomis/tree/tree-info))
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
    (- v (nomis/outline/w/level/no-inc-if-in-body))))

;;;; Operations on root

(defun nomis/tree/n-levels-below/root ()
  (nomis/tree/save-excursion-to-root
    (nomis/tree/n-levels-below)))

(defun nomis/tree/start-level-for-incremental-contract/root ()
  (nomis/tree/save-excursion-to-root
    (nomis/tree/start-level-for-incremental-contract)))

(defun nomis/tree/n-levels-being-shown-or-infinity/root ()
  (nomis/tree/save-excursion-to-root
    (nomis/tree/n-levels-being-shown-or-infinity)))

;;;; Operations on buffer

(defun nomis/tree/levels/max-in-buffer ()
  (let* ((sofar 0))
    (nomis/tree/mapc-entries-from-all-roots
     (lambda ()
       (setq sofar (max (nomis/tree/level-incl-any-body/boh)
                        sofar))))
    sofar))

(defun nomis/tree/n-levels-below/buffer ()
  (1- (nomis/tree/levels/max-in-buffer)))

(defun nomis/tree/start-level-for-incremental-contract/buffer ()
  (->> (nomis/tree/map-roots #'nomis/tree/start-level-for-incremental-contract)
       (apply #'max)))

(defun nomis/tree/n-levels-being-shown-or-infinity/buffer ()
  (->> (nomis/tree/map-roots #'nomis/tree/n-levels-being-shown-or-infinity)
       (apply #'min)))

;;;; Expanding and collapsing

;;;;; *expanding-parent?*

(defvar *expanding-parent?* nil)

;;;;; -nomis/tree/set-level-etc

(defun -nomis/tree/out-of-range (v maximum setting-kind current-value)
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
       (= current-value nomis/outline/w/plus-infinity)))))

;; "wrapex" means "wrap-expand-collapse".

(defconst -nomis/tree/wrapex? t)
(defvar -nomis/tree/allow-wrapex-timer nil)

(defun -nomis/tree/cancel-wrapex-timer ()
  (when -nomis/tree/allow-wrapex-timer
    (cancel-timer -nomis/tree/allow-wrapex-timer)
    (setq -nomis/tree/allow-wrapex-timer nil)))

(defun -nomis/tree/allow-wrapex-for-a-while ()
  (setq -nomis/tree/allow-wrapex-timer
        (run-at-time -nomis/tree/allow-wrap-duration
                     nil
                     '-nomis/tree/cancel-wrapex-timer)))

(defun -nomis/tree/bring-within-range (v maximum)
  (let* ((repeat-key-likely-used? (-nomis/tree/repeat-key-likely-used?))
         (min-allowed-value (if *expanding-parent?* 1 0)))
    (cl-flet ((normal-behaviour () (list (min (max min-allowed-value v)
                                              maximum)
                                         nil))
              (wrapping-behaviour () (list (if (= v (1- min-allowed-value))
                                               maximum
                                             min-allowed-value)
                                           t)))
      (let* ((allow-wrapex-now? (and -nomis/tree/allow-wrapex-timer
                                     (not repeat-key-likely-used?))))
        (-nomis/tree/cancel-wrapex-timer)
        (if (or (= maximum 0)
                (not (or (= v (1- min-allowed-value))
                         (= v nomis/outline/w/plus-infinity))))
            (normal-behaviour)
          (if (not allow-wrapex-now?)
              (progn
                (when (and -nomis/tree/wrapex?
                           (not repeat-key-likely-used?))
                  (-nomis/tree/allow-wrapex-for-a-while))
                (normal-behaviour))
            ;; Don't wrap if we moved to another position that also happens to
            ;; be fully-expanded. Don't wrap if we moved away and came back.
            (if (not (eq this-command (nomis/outline/w/last-command)))
                (normal-behaviour)
              (wrapping-behaviour))))))))

(defun -nomis/tree/set-level-etc (new-value-action-fun
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
          (-nomis/tree/bring-within-range v maximum)
        (let* ((out-of-range? (and (not do-cycling?)
                                   (-nomis/tree/out-of-range v
                                                             maximum
                                                             setting-kind
                                                             current-value))))
          (prog1
              (progn ; nomis/scrolling/with-force-maintain-line-no-in-window ; Is this needed? Presumably it is, but why? (Or maybe it was needed, but isn't now.) -- 2021-06-22 I've removed it. It was causing one-line scrolling in certain situations.
                (funcall new-value-action-fun new-level))
            (unless *-nomis/tree/inhibit-set-level-etc-message?*
              (funcall (if out-of-range?
                           #'nomis/popup/error-message
                         #'nomis/popup/message)
                       (concat message-format-string "%s%s")
                       new-level
                       maximum
                       (if -nomis/tree/show-bodies?
                           ""
                         " (not showing bodies)")
                       (if out-of-range?
                           (cl-ecase setting-kind
                             ((:less :setting-min)
                              " —- already fully collapsed")
                             ((:more :setting-max)
                              " —- already fully expanded"))
                         "")))))))))

;;;;; nomis/tree/show-children-from-point/xxxx support

(defun nomis/tree/show-children-from-point* (n) ; TODO You don't use the special negative arg thing. Simplify or get back that functionality.
  "Make point visible if it isn't already, and expand current heading to
n levels.

Details:

If N is not negative, expand to show N levels. Any headings at level N
will be collapsed.

If N is negative, expand to show (abs N) levels, but do not hide anything
that is already being displayed."
  (nomis/outline/w/ensure-heading-shown)
  (let* ((collapse? (>= n 0))
         (n (abs n)))
    (when collapse?
      (nomis/outline/w/collapse))
    (nomis/tree/expand n)))

(defun -nomis/tree/show-children-from-point/set-level-etc (level
                                                           setting-kind
                                                           current-value)
  (-nomis/tree/set-level-etc #'nomis/tree/show-children-from-point*
                             level
                             (nomis/tree/n-levels-below)
                             (if *expanding-parent?*
                                 "[%s / %s] from parent"
                               "[%s / %s]")
                             setting-kind
                             current-value))

;;;;; nomis/tree/show-children-from-point/xxxx

(defun nomis/tree/show-children-from-point (n)
  "Show N levels from the current heading, and collapse anything that's
at a higher level.
When in a body, \"current heading\" means the current body's parent heading."
  (let* ((v n))
    (-nomis/tree/show-children-from-point/set-level-etc v :no-check :dummy)))

(defun nomis/tree/show-children-from-point/set-min ()
  "Fully collapse the current heading.
When in a body, \"current heading\" means the current body's parent heading."
  (interactive)
  (-nomis/tree/command
      nil
    (let* ((current-value (nomis/tree/start-level-for-incremental-contract))
           (v (if *expanding-parent?* 1 0)))
      (-nomis/tree/show-children-from-point/set-level-etc v :setting-min current-value))))

(defun nomis/tree/show-children-from-point/fully-expand ()
  "Fully expand the current heading.
When in a body, \"current heading\" means the current body's parent heading."
  (interactive)
  (-nomis/tree/command
      nil
    (let* ((current-value (nomis/tree/n-levels-being-shown-or-infinity))
           (v :max))
      (-nomis/tree/show-children-from-point/set-level-etc v :setting-max current-value))))

(defun nomis/tree/show-children-from-point/incremental/less (n-or-nil)
  "If `N-OR-NIL` is not provided, collapse the current heading by one level.
If `N-OR-NIL` is provided, set the number of child levels to `N-OR-NIL`.
When in a body, \"current heading\" means the current body's parent heading."
  (interactive "P")
  (-nomis/tree/command
      nil
    (if n-or-nil
        (nomis/tree/show-children-from-point n-or-nil)
      (-nomis/tree/show-children-from-point/set-level-etc
       (1- (nomis/tree/start-level-for-incremental-contract)) :less :dummy))))

(defun nomis/tree/backtab (n-or-nil)
  (interactive "P")
  (if (nomis/outline/w/on-heading?)
      (nomis/tree/show-children-from-point/incremental/less n-or-nil)
    (error "Can't do <backtab> when in a body")))

(defun nomis/tree/show-children-from-point/incremental/more (n-or-nil)
  "If `N-OR-NIL` is not provided, expand the current heading by one level.
If `N-OR-NIL` is provided, set the number of child levels to `N-OR-NIL`.
When in a body, \"current heading\" means the current body's parent heading."
  (interactive "P")
  (-nomis/tree/command
      nil
    (if n-or-nil
        (nomis/tree/show-children-from-point n-or-nil)
      (-nomis/tree/show-children-from-point/set-level-etc
       (1+ (nomis/tree/n-levels-being-shown-or-infinity)) :more :dummy))))

;;;;; nomis/tree/show-children-from-parent/xxxx support

(defun nomis/tree/save-excursion-to-parent-and-then-show-point* (f)
  (prog1
      (let* ((*expanding-parent?* t))
        (nomis/tree/save-excursion-to-parent (funcall f)))
    (nomis/outline/w/ensure-heading-shown)))

(cl-defmacro nomis/tree/save-excursion-to-parent-and-then-show-point (&body body)
  (declare (indent 0))
  `(nomis/tree/save-excursion-to-parent-and-then-show-point* (lambda () ,@body)))

;;;;; nomis/tree/show-children-from-parent/xxxx

(defun nomis/tree/show-children-from-parent (n)
  "Like `nomis/tree/show-children-from-point`, but from the
current entry's parent."
  (nomis/tree/save-excursion-to-parent-and-then-show-point
    (nomis/tree/show-children-from-point n)))

(defun nomis/tree/show-children-from-parent/set-min ()
  "Like `nomis/tree/show-children-from-point/set-min`, but from the
current entry's parent and showing one level."
  (interactive)
  (-nomis/tree/command
      nil
    (nomis/tree/save-excursion-to-parent-and-then-show-point
      (nomis/tree/show-children-from-point/set-min))))

(defun nomis/tree/show-children-from-parent/fully-expand ()
  "Like `nomis/tree/show-children-from-point/fully-expand`, but from
the current entry's parent."
  (interactive)
  (-nomis/tree/command
      nil
    (nomis/tree/save-excursion-to-parent-and-then-show-point
      (nomis/tree/show-children-from-point/fully-expand))))

(defun nomis/tree/show-children-from-parent/incremental/less (n-or-nil)
  "If `N-OR-NIL` is not provided, collapse current heading's parent by one level.
Keep the parent expanded by at least one level.
If `N-OR-NIL` is provided, set the number of child levels to `N-OR-NIL`."
  (interactive "P")
  (-nomis/tree/command
      nil
    (nomis/tree/save-excursion-to-parent-and-then-show-point
      (nomis/tree/show-children-from-point/incremental/less n-or-nil))))

(defun nomis/tree/show-children-from-parent/incremental/more (n-or-nil)
  "If `N-OR-NIL` is not provided, expand current heading's parent by one level.
If `N-OR-NIL` is provided, set the number of child levels to `N-OR-NIL`."
  (interactive "P")
  (-nomis/tree/command
      nil
    (nomis/tree/save-excursion-to-parent-and-then-show-point
      (nomis/tree/show-children-from-point/incremental/more n-or-nil))))

;;;;; nomis/tree/show-children-from-root/xxxx support

(defun nomis/tree/show-children-from-root* (n)
  "Call `nomis/tree/show-children-from-point*` on the current root
heading, with N as the parameter."
  (nomis/tree/save-excursion-to-root
    (nomis/tree/show-children-from-point* n)))

(defun -nomis/tree/show-children-from-root/set-level-etc (level
                                                          setting-kind
                                                          current-value)
  (-nomis/tree/set-level-etc #'nomis/tree/show-children-from-root*
                             level
                             (nomis/tree/n-levels-below/root)
                             "[%s of %s] from root"
                             setting-kind
                             current-value))

;;;;; nomis/tree/show-children-from-root/xxxx

(defun nomis/tree/show-children-from-root (n)
  (let* ((v n))
    (-nomis/tree/show-children-from-root/set-level-etc v :no-check :dummy)))

(defun nomis/tree/show-children-from-root/set-min ()
  "Fully collapse the root of the current heading."
  (interactive)
  (-nomis/tree/command
      nil
    (let* ((current-value (nomis/tree/start-level-for-incremental-contract/root))
           (v 0))
      (-nomis/tree/show-children-from-root/set-level-etc v :setting-min current-value))))

(defun nomis/tree/show-children-from-root/fully-expand ()
  "Fully expand the root of the current heading."
  (interactive)
  (-nomis/tree/command
      nil
    (let* ((current-value (nomis/tree/n-levels-being-shown-or-infinity/root))
           (v :max))
      (-nomis/tree/show-children-from-root/set-level-etc v :setting-max current-value))))

(defun nomis/tree/show-children-from-root/incremental/less (n-or-nil)
  "If `N-OR-NIL` is not provided, collapse the current heading's root by one level.
If `N-OR-NIL` is provided, set the number of child levels to `N-OR-NIL`."
  (interactive "P")
  (-nomis/tree/command
      nil
    (if n-or-nil
        (nomis/tree/show-children-from-root n-or-nil)
      (-nomis/tree/show-children-from-root/set-level-etc
       (1- (nomis/tree/start-level-for-incremental-contract/root)) :less :dummy))))

(defun nomis/tree/show-children-from-root/incremental/more (n-or-nil)
  "If `N-OR-NIL` is not provided, expand the current heading's root by one level.
If `N-OR-NIL` is provided, set the number of child levels to `N-OR-NIL`."
  (interactive "P")
  (-nomis/tree/command
      nil
    (if n-or-nil
        (nomis/tree/show-children-from-root n-or-nil)
      (-nomis/tree/show-children-from-root/set-level-etc
       (1+ (nomis/tree/n-levels-being-shown-or-infinity/root)) :more :dummy))))

(defun nomis/tree/show-children-from-root/to-current-level ()
  "Expand the root of the current heading to the current heading's level."
  (interactive)
  (-nomis/tree/command
      nil
    (let* ((v (1- (nomis/outline/w/level/inc-if-in-body))))
      (-nomis/tree/show-children-from-root/set-level-etc v :no-check :dummy))))

;;;;; nomis/tree/show-children-from-all-roots/xxxx support

(defun nomis/tree/show-children-from-all-roots* (n)
  "Call `nomis/tree/show-children-from-point*` on all root headings,
with N as the parameter."
  (nomis/tree/mapc-roots (lambda () (nomis/tree/show-children-from-point* n))))

(defun -nomis/tree/show-children-from-all-roots/set-level-etc (level
                                                               setting-kind
                                                               current-value)
  (-nomis/tree/set-level-etc #'nomis/tree/show-children-from-all-roots*
                             level
                             (nomis/tree/n-levels-below/buffer)
                             "[%s of %s] from all roots"
                             setting-kind
                             current-value))

;;;;; nomis/tree/show-children-from-all-roots/xxxx

(defun nomis/tree/show-children-from-all-roots (n)
  (let* ((v n))
    (-nomis/tree/show-children-from-all-roots/set-level-etc v :no-check :dummy)))

(defun nomis/tree/show-children-from-all-roots/set-min ()
  "Fully collapse all roots."
  (interactive)
  (-nomis/tree/command
      nil
    (let* ((current-value (nomis/tree/start-level-for-incremental-contract/buffer))
           (v 0))
      (-nomis/tree/show-children-from-all-roots/set-level-etc v :setting-min current-value))))

(defun nomis/tree/show-children-from-all-roots/fully-expand ()
  "Fully expand all roots."
  (interactive)
  (-nomis/tree/command
      nil
    (let* ((current-value (nomis/tree/n-levels-being-shown-or-infinity/buffer))
           (v :max))
      (-nomis/tree/show-children-from-all-roots/set-level-etc v :setting-max current-value))
    (nomis/msg/pulse-buffer)))

(defun nomis/tree/show-children-from-all-roots/incremental/less (n-or-nil)
  "If `N-OR-NIL` is not provided, collapse all roots by one level.
If `N-OR-NIL` is provided, set the number of child levels to `N-OR-NIL`."
  (interactive "P")
  (-nomis/tree/command
      nil
    (if n-or-nil
        (nomis/tree/show-children-from-all-roots n-or-nil)
      (-nomis/tree/show-children-from-all-roots/set-level-etc
       (1- (nomis/tree/start-level-for-incremental-contract/buffer)) :less :dummy))))

(defun nomis/tree/show-children-from-all-roots/incremental/more (n-or-nil)
  "If `N-OR-NIL` is not provided, expand all roots by one level.
If `N-OR-NIL` is provided, set the number of child levels to `N-OR-NIL`."
  (interactive "P")
  (-nomis/tree/command
      nil
    (if n-or-nil
        (nomis/tree/show-children-from-all-roots n-or-nil)
      (-nomis/tree/show-children-from-all-roots/set-level-etc
       (1+ (nomis/tree/n-levels-being-shown-or-infinity/buffer)) :more :dummy))))

(defun nomis/tree/show-children-from-all-roots/to-current-level ()
  "Expand all roots to the current heading's level."
  (interactive)
  (-nomis/tree/command
      nil
    (let* ((v (1- (nomis/outline/w/level/inc-if-in-body))))
      (-nomis/tree/show-children-from-all-roots/set-level-etc v :no-check :dummy))))

;;;;; Lineage

(defun nomis/tree/lineage/less ()
  (interactive)
  (-nomis/tree/command
      nil
    (nomis/tree/ls/lineage/less)))

(defun nomis/tree/lineage/more ()
  (interactive)
  (-nomis/tree/command
      nil
    (nomis/tree/ls/lineage/more)))

(defun nomis/tree/lineage/set-min ()
  (interactive)
  (-nomis/tree/command
      nil
    (nomis/tree/ls/lineage/set-min)))

(defun nomis/tree/lineage/set-max ()
  (interactive)
  (-nomis/tree/command
      nil
    (nomis/tree/ls/lineage/set-max)))

;;;; Beacon

(with-eval-after-load 'beacon
  (dolist (cmd '(nomis/tree/nav+lineage/backward-any-level
                 nomis/tree/nav+lineage/forward-any-level
                 nomis/tree/nav+lineage/backward-sibling
                 nomis/tree/nav+lineage/forward-sibling
                 nomis/tree/nav+lineage/backward-peer
                 nomis/tree/nav+lineage/forward-peer))
    (add-to-list 'beacon-dont-blink-commands cmd) ; noflycheck
    ))

;;; End

(provide 'nomis-tree)
