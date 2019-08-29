;;;; norg --- A layer on top of Org mode  ---  -*- lexical-binding: t -*-

(progn) ; this-stops-hs-hide-all-from-hiding-the-next-comment

;;;; ___________________________________________________________________________
;;;; ____ * TODOs

;;;; TODO Look at expansion of headlines with bodies (or whatever they
;;;;      are called).
;;;;      (Bodies are not being expanded. Maybe want a way to expand them.)
;;;;      Perhaps you could have an extra level between your current levels;
;;;;      they'd differ by whether bodies are shown.
;;;;      (And is there anything else apart from bodies to consider?)

;;;; TODO Ellipsis symbols disappear in some places while popup is being
;;;;      displayed.

;;;; TODO (Too hard! Park this for now.)
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

;;;; TODO Idea of treating level -1 as show only parents, and not siblings.
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

;;;; TODO Bug: When a M-x is used to invoke a command (even something not
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

;;;; ___________________________________________________________________________
;;;; ____ * Some rejected ideas

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

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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
;;;; ____ * Some wrappers for org functionality

(defun norg/point-is-visible? ()
  (not (get-char-property (point) 'invisible)))

(defun norg/map-roots (fun)
  (let* ((dummy (cons nil nil)))
    (remove dummy
            (org-map-entries (lambda ()
                               (let* ((level (norg/current-level)))
                                 (if (= level 1)
                                     (funcall fun)
                                   dummy)))
                             t
                             'file))))

(defun norg/current-level ()
  (nth 1 (org-heading-components)))

(defun norg/current-level-or-error-string ()
  (condition-case err
      (norg/current-level)
    (error (cdr err))))

(defun norg/goto-root ()
  (interactive)
  (while (ignore-errors (outline-up-heading 1))))

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
       (dotimes (_ 3) (org-cycle))))
    (2
     ;; This makes lots of stuff visible, but seems to be the "official" way.
     ;; Leave this here as a point of interest.
     (let ((org-catch-invisible-edits 'show))
       (org-check-before-invisible-edit 'insert)))))

(defun -norg/collapse ()
  (norg/show-point)
  (case 2
    (1
     ;; This hides too much stuff.
     (org-overview)
     (org-show-set-visibility 'canonical))
    (2
     ;; This hides just the subtree under the headline at point.
     ;; Idea from http://christiantietze.de/posts/2019/06/org-fold-heading/.
     ;; But what does `org-flag-subtree` do, is it part of the org public API,
     ;; and why can't I find any useful info by googling?
     (org-flag-subtree t))))

(defun -norg/expand ()
  (norg/show-point)
  (-norg/collapse) ; so that we can expand in a predicable way
  (case 3
    ;; I tried various approaches until I found one that seems to work.
    (1 (outline-show-children 99))
    (2 (dotimes (_ 5)
         ;; The 5 should work no matter how many levels there are below
         ;; this one. It does if you hit TAB five times.
         (org-cycle)))
    (3 (org-map-tree #'org-cycle) ; see also `org-map-tree` if you copy this
       )))

;;;; ___________________________________________________________________________
;;;; ____ * Things I did before I had tree-info -- perhaps redo with tree-info

(defun -norg/levels/below-point-helper (pred-of-no-args
                                        reducing-function)
  ;; TODO Think about what `reduce` does.
  ;;      Maybe add initial value, and value to return when you get a nil.
  (let* ((max-level-beneath
          (let* ((sofar nil))
            (org-map-entries (lambda ()
                               (when (funcall pred-of-no-args)
                                 (let* ((v (norg/current-level)))
                                   (setq sofar
                                         (if (null sofar)
                                             v
                                           (funcall reducing-function
                                                    sofar
                                                    v))))))
                             t
                             'tree)
            sofar)))
    (if (null max-level-beneath)
        nil
      (- max-level-beneath
         (norg/current-level)))))

(defun norg/n-levels-below ()
  (-norg/levels/below-point-helper (lambda () t)
                                   #'max))

(defun norg/smallest-invisible-level-below-or-infinity ()
  (or (let* ((not-visible? (-compose #'not
                                     #'norg/point-is-visible?)))
        (-norg/levels/below-point-helper not-visible?
                                         #'min))
      -norg/plus-infinity))

;;;; ___________________________________________________________________________
;;;; ____ * The idea of tree-info, and things that use it

(defun -norg/tree-info ()
  ;; This is rather expensive, because the value returned by `org-map-entries`
  ;; is processed further and discarded.
  ;; You could do more in `org-map-entries`, but it's fiddly because:
  ;; - You want to collect multiple items per iteration.
  ;;   (Solution: use nconc on lists.)
  ;; - You want to look at two headlines at a time.
  ;;   (Solution: record previous item in a piece of mutable state.)
  (let* ((dummy-initial-entry '(:dummy-first t nil))
         (basic-info (org-map-entries (lambda ()
                                        (list (norg/current-level)
                                              (norg/point-is-visible?)))
                                      t
                                      'tree)))
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

(defun -norg/fully-expanded? (&optional tree-info)
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
    (org-map-entries (lambda ()
                       (setq sofar (max (norg/current-level)
                                        sofar)))
                     t
                     'file)
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
;;;; ____ * -norg/set-level-etc

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

;;;; ___________________________________________________________________________
;;;; ____ * Expanding and collapsing

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;;; ____ ** norg/show-children-from-point/xxxx support

(defun norg/show-children-from-point* (n) ; TODO You don't use the special negative arg thing. Simplify or get back that functionality.
  "Expand current headline to n levels.

Details:

If N is not negative, expand to show N levels. Any headlines at level N
will be collapsed.

If N is negative, expand to show (abs N) levels, but do not hide anything
that is already being displayed."
  (let* ((collapse? (>= n 0))
         (n (abs n)))
    (when collapse?
      (-norg/collapse))
    (outline-show-children n)))

(defun -norg/show-children-from-point/set-level-etc (level)
  (-norg/set-level-etc #'norg/show-children-from-point*
                       level
                       (norg/n-levels-below)
                       "[%s / %s]"))

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;;; ____ ** norg/show-children-from-all-roots/xxxx support

(defun norg/show-children-from-all-roots* (n)
  "Call `norg/show-children-from-point*` on all root headlines, with N as
the parameter."
  (norg/map-roots (lambda () (norg/show-children-from-point* n))))

(defun -norg/show-children-from-all-roots/set-level-etc (level)
  (-norg/set-level-etc #'norg/show-children-from-all-roots*
                       level
                       (norg/n-levels-below/buffer)
                       "[%s of %s] from all roots"))

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;;; ____ ** norg/show-all-to-current-level

(defun norg/show-all-to-current-level ()
  (interactive)
  (-> (1- (norg/current-level))
      -norg/show-children-from-all-roots/set-level-etc))

;;;; ___________________________________________________________________________
;;;; ____ * End

(provide 'norg)
