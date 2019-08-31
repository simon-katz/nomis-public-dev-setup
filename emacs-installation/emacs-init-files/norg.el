;;;; norg --- A layer on top of Org mode  ---  -*- lexical-binding: t -*-

(progn) ; this-stops-hs-hide-all-from-hiding-the-next-comment

;;;; ___________________________________________________________________________
;;;; ____ * TODOs

;;;; TODO Bring some stuff from `nomis-org` into `norg`. See your org mode
;;;;      keybindings for candidates.

;;;; ___________________________________________________________________________
;;;; ____ * Some rejected (at least for now) ideas

;;;; XXXX Bodies:
;;;;      - REJECTED for now at least.
;;;;        - You could continue with this, but:
;;;;          - it's not worth the effort at the moment
;;;;          - you'd have to add calls to `-norg/body-expanded?/experimental`
;;;;            when collecting tree-info. Maybe that will make things a fair
;;;;            bit slower.
;;;;      - Body visibility
;;;;        - Is there a way to know whether a node has a collapsed body?
;;;           - I think so -- see `-norg/body-expanded?/experimental`.
;;;;        - `norg/fully-expanded?` needs to take account of this.
;;;;        - `norg/n-levels-below` needs to take account of this.
;;;;          - Oh, that needs to know whether there is a body.
;;;;            You could find out by comparing the points of the end of the
;;;;            headline and `(1- (progn (norg/next-preface) (point)))`.
;;;;        - Anything else?
;;;;      - Perhaps you could have an extra level between your current levels;
;;;;        they'd differ by whether bodies are shown.
;;;;        - REJECTED because that's too many levels; just treat the body as
;;;;          being one level below its headline.

;;;; XXXX Ellipsis symbols disappear in some places while popup is being
;;;;      displayed.

;;;; XXXX (Too hard! Park this for now.)
;;;;      Sometimes things take a long time and a busy pointer would be useful.
;;;;      Why don't you have one?
;;;;      Emacs is supposed to do this for you.
;;;;      Some investigation:
;;;;      - Tested with -Q option:
;;;;          /Applications/Emacs-26-1-2.app/Contents/MacOS/Emacs -Q
;;;;      - Same behaviour on two Macs.
;;;;      - There seem to be several problems:
;;;;        - On OS X, the pointer disappears when typing. You have to move
;;;;          the mouse to get it back. So even if you fixed this problem,
;;;;          you wouldn't have a busy cursor when busy unless you moved the
;;;;          pointer.
;;;;        - Busy pointer is an I-beam, not an hourglass.
;;;;        - Busy pointer doesn't appear as soon as it should.
;;;;        - Busy pointer sometimes sticks until you move the mouse.
;;;;      - Google not helping -- can't find any mention of this.

;;;; XXXX Idea of treating level -1 as show only parents, and not siblings.
;;;;      But first:
;;;;      - The visibility-span stuff is global, but you have things that work
;;;;        from point and from root. So you might have to roll your own to o
;;;;        this.
;;;;      - Can you detect a parents-not-siblings state? (Remember: you are
;;;;        no longer storing state for positions. (But you could in this
;;;;        special case, I guess. But that's getting complicated. It was nice
;;;;        to get rid of the state.))
;;;;      - Consider whether there's anything else you might want to do with
;;;;        visibility.
;;;;      - Sort out visibiity issues that are mentioned above:
;;;;        - Bodies not being expanded.
;;;;        - (Anything else?)

;;;; XXXX Bug: When a M-x is used to invoke a command (even something not
;;;;      org-related (such as `version` or `what-cursor-position)`, if the
;;;;      point is hidden it gets changed to be a visible point.
;;;;      To reproduce:
;;;;      - M-x what-cursor-position
;;;;      - Do something that hides point.
;;;;      - M-x what-cursor-position
;;;;        - Observe that point has not changed.
;;;;      - M-x what-cursor-position
;;;;        - Observe that point has changed.
;;;;      So, it seems that after running a M-x command, point gets changed.

;;;; XXXX When getting to 0 or max, first flash then cycle.
;;;;      REJECTED
;;;;      This would give a modal UI. An invisibly-modal UI.

;;;; ___________________________________________________________________________
;;;; ____ * Require things

(require 'org)
(require 'cl)
(require 'dash)
(require 'dash-functional)

;;;; Things that you might want to make into packages if you make norg into a
;;;; package.

(require 'nomis-popup nil t)
(require 'nomis-msg nil t)

;;;; ___________________________________________________________________________
;;;; ____ * Tailor other functionality

;;;; ____ ** norg/popup/message

(defvar norg/use-nomis-popup-when-available? t)

(defun norg/popup/message (format-string &rest args)
  (let* ((fun (if (and norg/use-nomis-popup-when-available?
                       (featurep 'nomis-popup))
                  #'nomis/popup/message
                #'message)))
    (apply fun
           format-string
           args)))

;;;; ____ ** what-cursor-position
;;;; Add org level to the output of `what-cursor-position`.

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
            '((name . add-org-level-info)))

(advice-add 'message
            :around
            (lambda (orig-fun &rest args)
              (if (and norg/add-info-to-what-cursor-position?
                       (eq major-mode 'org-mode)
                       *-norg/in-what-cursor-position?*)
                  (apply orig-fun
                         (concat (first args) " org-level=%s")
                         (append (rest args) (list (norg/current-level-or-error-string))))
                (apply orig-fun args)))
            '((name . add-org-level-info)))

;;;; ___________________________________________________________________________
;;;; ____ * Infinity

(defconst -norg/plus-infinity   1.0e+INF)
(defconst -norg/minus-infinity -1.0e+INF)

;;;; ___________________________________________________________________________
;;;; ____ * Wrappers for `outline` and `org`

;;;; I'm not clear about the public API of `outline` and `org`, so let's
;;;; be safe.
;;;; Besides, it's useful to isolate how we use `outline` and `org`.

(defun norg/level/must-be-at-boh ()
  "Point must be at the beginning of a headline.
Return the nesting depth of the headline in the outline."
  (funcall outline-level))

(defalias 'norg/next-preface 'outline-next-preface)
(defalias 'norg/up-heading 'outline-up-heading)
(defalias 'norg/next-heading 'outline-next-heading)
(defalias 'norg/show-children 'outline-show-children) ; Not `org-show-children`, because that shows first level when n is 0
(defalias 'norg/show-entry 'outline-show-entry)
(defalias 'norg/invisible-p 'org-invisible-p)
(defalias 'norg/back-to-heading 'org-back-to-heading)
(defalias 'norg/cycle 'org-cycle)
(defalias 'norg/check-before-invisible-edit 'org-check-before-invisible-edit)
(defalias 'norg/at-heading-p 'org-at-heading-p)
(defalias 'norg/map-tree 'org-map-tree)
(defalias 'norg/overview 'org-overview)
(defalias 'norg/show-set-visibility 'org-show-set-visibility)
(defalias 'norg/flag-subtree 'org-flag-subtree)

;;;; ___________________________________________________________________________
;;;; ____ * Some wrappers for org functionality

;;;; Basic stuff

(defun norg/point-is-visible? ()
  (not (norg/invisible-p)))

(defun -norg/body-expanded?/experimental ()
  ;; This simply checks whether the start of the headline and end of the item
  ;; including any body are visible, so it might give a wrong answer.
  (save-excursion
    (norg/back-to-heading)
    (let* ((start (point))
           (end   (1- (progn (norg/next-preface) (point)))))
      (not (or (norg/invisible-p start)
               (norg/invisible-p end))))))

(defun norg/current-level ()
  (save-excursion
    (norg/back-to-heading t)
    (norg/level/must-be-at-boh)))

(defun norg/current-level-or-error-string ()
  (condition-case err
      (norg/current-level)
    (error (cdr err))))

(defun norg/goto-root ()
  (interactive)
  (while (ignore-errors (norg/up-heading 1))))

(cl-defmacro norg/save-excursion-to-root (&body body)
  (declare (indent 0))
  `(save-excursion
     (norg/goto-root)
     ,@body))

(defun norg/show-point ()
  (interactive)
  (case 1
    (1
     (unless (norg/point-is-visible?)
       ;; Make point visible and leave subtree collapsed
       (dotimes (_ 3) (norg/cycle))))
    (2
     ;; This makes lots of stuff visible, but seems to be the "official" way.
     ;; Leave this here as a point of interest.
     (let ((org-catch-invisible-edits 'show))
       (norg/check-before-invisible-edit 'insert)))))

;;;; Support for do-ing and mapping

(defun -norg/mapc-headlines-satisfying (pred-of-no-args fun)
  (save-excursion
    (cl-flet ((call-fun-when-pred-is-satisfied
               ()
               (when (funcall pred-of-no-args)
                 (funcall fun))))
      (beginning-of-buffer)
      (unless (norg/at-heading-p)
        (norg/next-heading))
      (when (norg/at-heading-p)
        (call-fun-when-pred-is-satisfied)
        (while (progn
                 (norg/next-heading)
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

;;;; Do-ing

(defun norg/mapc-entries-from-point (fun)
  (save-excursion
    (norg/map-tree fun))
  nil)

(defun norg/mapc-entries-from-root (fun)
  (norg/save-excursion-to-root
    (norg/mapc-entries-from-point fun)))

(defun norg/mapc-roots (fun)
  (-norg/mapc-headlines-satisfying (lambda ()
                                     (= (norg/level/must-be-at-boh)
                                        1))
                                   fun))

(defun norg/mapc-entries-from-all-roots (fun)
  (-norg/mapc-headlines-satisfying (lambda () t)
                                   fun)
  nil)

;;;; Mapping

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

;;;; Reducing (add more here if and when needed)

(cl-defun norg/reduce-entries-from-point (initial-value
                                          reducing-function
                                          &optional
                                          (pred-of-no-args (lambda () t)))
  (let* ((sofar initial-value))
    (norg/mapc-entries-from-point (lambda ()
                                    (when (funcall pred-of-no-args)
                                      (let* ((v (norg/level/must-be-at-boh)))
                                        (setq sofar
                                              (if (null sofar)
                                                  v
                                                (funcall reducing-function
                                                         sofar
                                                         v)))))))
    sofar))

;;;; Expanding and collapsing

(defun norg/collapse ()
  (case 2
    (1
     ;; This hides too much stuff.
     (norg/overview)
     (norg/show-set-visibility 'canonical))
    (2
     ;; This hides just the subtree under the headline at point.
     ;; Idea from http://christiantietze.de/posts/2019/06/org-fold-heading/.
     ;; But what does `org-flag-subtree` do, is it part of the org public API,
     ;; and why can't I find any useful info by googling?
     (norg/flag-subtree t))))

(defun norg/expand (n)
  (norg/show-children n)
  (let* ((level (norg/current-level)))
    (norg/mapc-entries-from-point #'(lambda ()
                                      ;; TODO Why did you change this? Isn't the
                                      ;;      old, simpler code OK?
                                      (when (case :old
                                              (:old
                                               (norg/point-is-visible?))
                                              (:new
                                               (< (- (norg/level/must-be-at-boh)
                                                     level)
                                                  n)))
                                        (norg/show-entry))))))

(defun norg/expand-fully ()
  (norg/expand 1000) ; TODO magic number
  )

;;;; ___________________________________________________________________________
;;;; ____ * Info about trees

(defun norg/n-levels-below ()
  (- (norg/reduce-entries-from-point 0 #'max)
     (norg/current-level)))

(defun norg/smallest-invisible-level-below-or-infinity ()
  (- (norg/reduce-entries-from-point -norg/plus-infinity
                                     #'min
                                     (-compose #'not
                                               #'norg/point-is-visible?))
     (norg/current-level)))

;;;; ___________________________________________________________________________
;;;; ____ * The idea of tree-info, and things that use it

(defun -norg/tree-info ()
  ;; This is rather expensive, because the value returned by
  ;; `norg/map-entries-from-point` is processed further and discarded.
  ;; You could do more in the fun passed to `norg/map-entries-from-point`, but
  ;; it's fiddly because:
  ;; - You want to collect multiple items per iteration.
  ;;   (Solution: use nconc on lists.)
  ;; - You want to look at two headlines at a time.
  ;;   (Solution: record previous item in a piece of mutable state.)
  (let* ((dummy-initial-entry
          '(:dummy-first t nil))
         (basic-info
          (norg/map-entries-from-point (lambda ()
                                         (list (norg/level/must-be-at-boh)
                                               (norg/point-is-visible?))))))
    (cl-loop for ((prev-level prev-visible?) . ((level visible?) . _))
             on (cons dummy-initial-entry
                      basic-info)
             for first? = (eq prev-level :dummy-first)
             for last? = (null level)
             when (and (not first?)
                       prev-visible?
                       (or last?
                           (<= level prev-level)))
             collect (list (1+ prev-level) nil t) ; dummy invisible entry
             unless last?
             collect (list
                      ;; :last? last?
                      ;; :prev-level prev-level
                      ;; :prev-visible? prev-visible?
                      ;; :level level
                      ;; :visible? visible?
                      level
                      visible?
                      nil))))

(defun norg/fully-expanded? (&optional tree-info)
  (setq tree-info (or tree-info (-norg/tree-info)))
  (cl-loop for (level visible? dummy?)
           in tree-info
           when (not dummy?)
           always visible?))

(defun norg/level-for-incremental-contract (&optional tree-info)
  ;; Collapse the most-deeply-nested expanded level, and expand everything
  ;; else to that level.
  (setq tree-info (or tree-info
                      (-norg/tree-info)))
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
                                        (setq sofar (max (norg/level/must-be-at-boh)
                                                         sofar))))
    sofar))

(defun norg/n-levels-below/buffer ()
  (1- (norg/levels/max-in-buffer)))

(defun norg/level-for-incremental-contract/buffer ()
  (->> (norg/map-roots #'norg/level-for-incremental-contract)
       (apply #'max)))

(defun norg/smallest-invisible-level-below/or-infinity/buffer ()
  (->> (norg/map-roots #'norg/smallest-invisible-level-below-or-infinity)
       (apply #'min)))

;;;; ___________________________________________________________________________
;;;; ____ * Expanding and collapsing

;;;; ____ ** -norg/set-level-etc

(defun -norg/out-of-range (v maximum)
  (or (< v 0)
      (> v maximum)))

(defun -norg/bring-within-range (v maximum)
  (min (max 0 v)
       maximum))

(defun -norg/set-level-etc (new-value-action-fun
                            new-level/maybe-out-of-range
                            maximum
                            message-format-string)
  (let* ((v (-> (if (eql new-level/maybe-out-of-range
                         :max)
                    ;; The special value of `:max` means that we don't
                    ;; have to compute the value twice.
                    maximum
                  new-level/maybe-out-of-range)))
         (out-of-range? (-norg/out-of-range v maximum))
         (new-level (-norg/bring-within-range v maximum)))
    (prog1
        (funcall new-value-action-fun new-level)
      (when (and out-of-range? (featurep 'nomis-msg))
        (nomis/msg/grab-user-attention/low))
      (funcall #'norg/popup/message
               (concat message-format-string "%s")
               new-level
               maximum
               (if out-of-range?
                   " —- can't go further than this"
                 "")))))

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

(defun -norg/show-children-from-point/set-level-etc (level)
  (-norg/set-level-etc #'norg/show-children-from-point*
                       level
                       (norg/n-levels-below)
                       "[%s / %s]"))

;;;; ____ ** norg/show-children-from-point/xxxx

(defun norg/show-children-from-point (n)
  (interactive "^p")
  (-> n
      -norg/show-children-from-point/set-level-etc))

(defun norg/show-children-from-point/set-0 ()
  (interactive)
  (-> 0
      -norg/show-children-from-point/set-level-etc))

(defun norg/show-children-from-point/fully-expand ()
  (interactive)
  (-> :max
      -norg/show-children-from-point/set-level-etc))

(defun norg/show-children-from-point/incremental/less ()
  (interactive)
  (-> (norg/level-for-incremental-contract)
      -norg/show-children-from-point/set-level-etc))

(defun norg/show-children-from-point/incremental/more ()
  (interactive)
  (-> (norg/smallest-invisible-level-below-or-infinity)
      -norg/show-children-from-point/set-level-etc))

;;;; ____ ** norg/show-children-from-root/xxxx support

(defun norg/show-children-from-root* (n)
  "Call `norg/show-children-from-point*` on the current root headline, with N as
the parameter."
  (norg/save-excursion-to-root
    (norg/show-children-from-point* n)))

(defun -norg/show-children-from-root/set-level-etc (level)
  (-norg/set-level-etc #'norg/show-children-from-root*
                       level
                       (norg/n-levels-below/root)
                       "[%s of %s] from root"))

;;;; ____ ** norg/show-children-from-root/xxxx

(defun norg/show-children-from-root (n)
  (interactive "^p")
  (-> n
      -norg/show-children-from-root/set-level-etc))

(defun norg/show-children-from-root/set-0 ()
  (interactive)
  (-> 0
      -norg/show-children-from-root/set-level-etc))

(defun norg/show-children-from-root/fully-expand ()
  (interactive)
  (-> :max
      -norg/show-children-from-root/set-level-etc))

(defun norg/show-children-from-root/incremental/less ()
  (interactive)
  (-> (norg/level-for-incremental-contract/root)
      -norg/show-children-from-root/set-level-etc))

(defun norg/show-children-from-root/incremental/more ()
  (interactive)
  (-> (norg/smallest-invisible-level-below-or-infinity/root)
      -norg/show-children-from-root/set-level-etc))

;;;; ____ ** norg/show-children-from-all-roots/xxxx support

(defun norg/show-children-from-all-roots* (n)
  "Call `norg/show-children-from-point*` on all root headlines, with N as
the parameter."
  (norg/mapc-roots (lambda () (norg/show-children-from-point* n))))

(defun -norg/show-children-from-all-roots/set-level-etc (level)
  (-norg/set-level-etc #'norg/show-children-from-all-roots*
                       level
                       (norg/n-levels-below/buffer)
                       "[%s of %s] from all roots"))

;;;; ____ ** norg/show-children-from-all-roots/xxxx

(defun norg/show-children-from-all-roots (n)
  (interactive "^p")
  (-> n
      -norg/show-children-from-all-roots/set-level-etc))

(defun norg/show-children-from-all-roots/set-0 ()
  (interactive)
  (-> 0
      -norg/show-children-from-all-roots/set-level-etc))

(defun norg/show-children-from-all-roots/fully-expand ()
  (interactive)
  (-> :max
      -norg/show-children-from-all-roots/set-level-etc))

(defun norg/show-children-from-all-roots/incremental/less ()
  (interactive)
  (->> (norg/level-for-incremental-contract/buffer)
       -norg/show-children-from-all-roots/set-level-etc))

(defun norg/show-children-from-all-roots/incremental/more ()
  (interactive)
  (->> (norg/smallest-invisible-level-below/or-infinity/buffer)
       -norg/show-children-from-all-roots/set-level-etc))

;;;; ____ ** norg/show-all-to-current-level

(defun norg/show-all-to-current-level ()
  (interactive)
  (-> (1- (norg/current-level))
      -norg/show-children-from-all-roots/set-level-etc))

;;;; ___________________________________________________________________________
;;;; ____ * End

(provide 'norg)
