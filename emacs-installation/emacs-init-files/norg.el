;;;; norg --- A layer on top of Org mode  ---  -*- lexical-binding: t -*-

(progn) ; this-stops-hs-hide-all-from-hiding-the-next-comment

;;;; ___________________________________________________________________________
;;;; ____ * TODOs

;;;; TODO Remove all mentions of `nomis`.

;;;; TODO You don't need both `norg/levels/max-in-buffer` and `-norg/max-level`.

;;;; TODO When `tree-info` is not supplied as an arg, maybe use a different
;;;;      approach (and don't get tree-info).

;;;; TODO Finish destroying `nomis-repeated-commands`, or put new stuff there.

;;;; TODO Look into which `save-excursion`s and `(goto-char 1)`s are needed.

;;;; TODO There's a bug in incremental collapsing when there a child is more
;;;;      than one level deeper than its parent.

;;;; TODO ++about-uses-of-org-reveal++ (This may be already-dealt-with.)
(defconst ++about-uses-of-org-reveal++
  "Without certain ueses of `org-reveal`, point gets automatically reset
to a visible point.
This seems to happen in idle time.

I'm sure I had point being hidden before in some situation --
maybe not this situation -- and later revealing would take me
back to where I had previously been.

And `org-reveal` is interactive, so, yes, there are times when
  point is not visible.")

;;;; TODO When getting to 0 or max, first flash then cycle.

;;;; TODO Look at expansion of headlines with bodies (or whatever they
;;;;      are called).
;;;;      (Bodies are not being expanded. Maybe want a way to expand them.)

;;;; TODO Ellipsis symbols disappear in some places while popup is being
;;;;      displayed.

;;;; TODO Want to not have to show point. Can you have it not move point when
;;;;      you hide point?
;;;;      - This has started doing what you want for the `from-all-roots` stuff.
;;;;        Is that related to the `save-excursion`s you've added?
;;;;        - Yes! I've added `save-excursion`s for the `from-root` stuff,
;;;;          and that's doing the same.
;;;;     - Cool, buy why?

;;;; TODO Put the popup stuff somewhere new.

;;;; TODO Sometimes things take a long time and a busy cursor would be useful.

;;;; TODO At the beginning of the commands, go to beginning of
;;;;      - the headline, or
;;;;      - the current top-level headline
;;;;      as appropriate that the position lookup works well.
;;;;      Do this inside a `save-excursion`.
;;;;      For the commands whose scope is the whole file you don't need to
;;;;      record positions (or record a position of 1 always).

;;;; TODO Fix the nasty macros.
;;;;      - Can you functionify some of it?
;;;;      - Macro hygiene.

;;;; ___________________________________________________________________________
;;;; ____ * Require things

(require 'org)
(require 'cl)
(require 'dash)
(require 'dash-functional)
(require 'nomis-repeated-commands)

;;;; ___________________________________________________________________________
;;;; ____ * Infinity

(defconst -norg/plus-infinity   1.0e+INF)
(defconst -norg/minus-infinity -1.0e+INF)

;;;; ___________________________________________________________________________
;;;; ____ * Some wrappers for org functionality

(defun norg/report-org-info ()
  (interactive)
  (message "Current level = %s%s"
           (norg/current-level)
           (if (not (fboundp 'nomis/point-etc-string))
               ""
             (concat "    "
                     (nomis/point-etc-string)))))

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

(defun norg/goto-root ()
  (interactive)
  (while (ignore-errors (outline-up-heading 1))))

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

(defun norg/levels/n-below-point ()
  (-norg/levels/below-point-helper (lambda () t)
                                   #'max))

(defun norg/levels/smallest-invisible-level-below-point/or-nil ()
  (let* ((not-visible? (-compose #'not
                                 #'norg/point-is-visible?)))
    (-norg/levels/below-point-helper not-visible?
                                     #'min)))

(defun norg/levels/max-below-root ()
  (save-excursion
    (norg/goto-root)
    (norg/levels/n-below-point)))

(defun norg/levels/max-in-buffer ()
  (let* ((sofar 0))
    (org-map-entries (lambda ()
                       (setq sofar (max (norg/current-level)
                                        sofar)))
                     t
                     'file)
    sofar))

;;;; ___________________________________________________________________________
;;;; ____ * The idea of tree-info, and things that use it

(defun -norg/tree-info ()
  (org-map-entries (lambda ()
                     (list (norg/current-level)
                           (norg/point-is-visible?)))
                   t
                   'tree))

(defun -norg/tree-info/with-dummy-invisible-levels ()
  (let* ((dummy-initial-entry '(:dummy-first t nil))
         (basic-info (-norg/tree-info)))
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

(defun -norg/max-level (&optional tree-info)
  (setq tree-info (or tree-info (-norg/tree-info)))
  (cl-loop for (level visible? dummy?)
           in tree-info
           when (not dummy?)
           maximize level))

(defun -norg/fully-expanded? (&optional tree-info)
  (setq tree-info (or tree-info (-norg/tree-info)))
  (cl-loop for (level visible? dummy?)
           in tree-info
           when (not dummy?)
           always visible?))

(defun norg/levels/level-for-incremental-contract (&optional tree-info)
  ;; Collapse the most-deeply-nested expanded level, and expand everything
  ;; else to that level.
  (setq tree-info (or tree-info
                      (-norg/tree-info/with-dummy-invisible-levels)))
  (let* ((v (let* ((initial-invisible-levels
                    (cl-loop for ((prev-level prev-visible?)
                                  . ((level visible?) . _))
                             on (cons '(most-negative-fixnum t)
                                      tree-info)
                             when (and prev-visible?
                                       (not visible?)
                                       (> level prev-level))
                             collect level)))
              (- (apply #'max initial-invisible-levels) 2))))
    ;; TODO Rationlise where you count levels from. This is a place where you
    ;;      convert. Be consistent.
    (- v (norg/current-level))))

;;;; ___________________________________________________________________________
;;;; ____ * show-children

(defun norg/show-children (n)
  "Expand current headline to n levels.

Details:

If N is not negative, expand to show N levels. Any headlines at level N
will be collapsed.

If N is negative, expand to show (abs N) levels, but do not hide anything
that is already being displayed."
  (interactive "^p")
  (let* ((collapse? (>= n 0))
         (n (abs n)))
    (when collapse?
      (-norg/collapse))
    (outline-show-children n)))

;;;; ___________________________________________________________________________
;;;; ____ * *-norg/level-format-string*

(defvar *-norg/level-format-string* nil)

;;;; ___________________________________________________________________________
;;;; ____ * show-children/incremental

(nomis/define-repeated-command-stuff
    -norg/show-children/incremental
  -norg/show-children/incremental/with-stuff/set
  #'norg/levels/n-below-point
  #'norg/show-children)

(defun norg/with-show-children-stuff* (fun)
  (let* ((*-norg/level-format-string*
          (or *-norg/level-format-string*
              "[%s of %s]")))
    (funcall fun)))

(cl-defmacro norg/with-show-children-stuff (&body body)
  (declare (indent 0))
  `(norg/with-show-children-stuff* (lambda () ,@body)))

(defun norg/show-children/set-0/impl ()
  (interactive)
  (let* ((new-level 0))
    (-norg/show-children/incremental/with-stuff/set new-level)))

(defun norg/show-children/fully-expand/impl ()
  (interactive)
  (let* ((new-level (norg/levels/n-below-point)))
    (-norg/show-children/incremental/with-stuff/set new-level)))

(defun norg/show-children/incremental/less/impl ()
  (interactive)
  (let* ((new-level (norg/levels/level-for-incremental-contract)))
    (-norg/show-children/incremental/with-stuff/set new-level)))

(defun norg/show-children/incremental/more/impl ()
  (interactive)
  (let* ((new-level
          (or (norg/levels/smallest-invisible-level-below-point/or-nil)
              -norg/plus-infinity)))
    (-norg/show-children/incremental/with-stuff/set new-level)))

(defun norg/show-children/set-0 ()
  (interactive)
  (norg/with-show-children-stuff
    (norg/show-children/set-0/impl)))

(defun norg/show-children/fully-expand ()
  (interactive)
  (norg/with-show-children-stuff
    (norg/show-children/fully-expand/impl)))

(defun norg/show-children/incremental/less ()
  (interactive)
  (norg/with-show-children-stuff
    (norg/show-children/incremental/less/impl)))

(defun norg/show-children/incremental/more ()
  (interactive)
  (norg/with-show-children-stuff
    (norg/show-children/incremental/more/impl)))

;;;; ___________________________________________________________________________
;;;; ____ * show-children-from-root

(defun norg/show-children-from-root (n)
  (interactive "^p")
  "Call `norg/show-children` on the current root headline, with N as
the parameter.
When done, call `org-reveal` so that the current point is shown.
But see ++about-uses-of-org-reveal++"
  (save-excursion
    (norg/goto-root)
    (norg/show-children n))
  (org-reveal) ; see ++about-uses-of-org-reveal++
  )

;;;; ___________________________________________________________________________
;;;; ____ * show-children-from-root/incremental

(nomis/define-repeated-command-stuff
    -norg/show-children-from-root/incremental
  -norg/show-children-from-root/incremental/with-stuff/set
  #'norg/levels/max-below-root
  #'norg/show-children-from-root)

(defun norg/with-show-children-from-root-stuff* (fun)
  (let* ((*-norg/level-format-string*
          (or *-norg/level-format-string*
              "[%s of %s] from root")))
    (funcall fun)))

(cl-defmacro norg/with-show-children-from-root-stuff (&body body)
  (declare (indent 0))
  `(norg/with-show-children-from-root-stuff* (lambda () ,@body)))

(defun norg/show-children-from-root/set-0 ()
  (interactive)
  (save-excursion
    (norg/goto-root)
    (norg/with-show-children-from-root-stuff
      (norg/show-children/set-0/impl))))

(defun norg/show-children-from-root/fully-expand ()
  (interactive)
  (save-excursion
    (norg/goto-root)
    (norg/with-show-children-from-root-stuff
      (norg/show-children/fully-expand/impl))))

(defun norg/show-children-from-root/incremental/less ()
  (interactive)
  (save-excursion
    (norg/goto-root)
    (norg/with-show-children-from-root-stuff
      (norg/show-children/incremental/less/impl))))

(defun norg/show-children-from-root/incremental/more ()
  (interactive)
  (save-excursion
    (norg/goto-root)
    (norg/with-show-children-from-root-stuff
      (norg/show-children/incremental/more/impl))))

;;;; ___________________________________________________________________________
;;;; ____ * show-children-from-all-roots

(defun norg/show-children-from-all-roots (n)
  "Call `norg/show-children` on all root headlines, with N as
the parameter.
When done, call `org-reveal` so that the current point is shown.
But see ++about-uses-of-org-reveal++"
  (interactive "^p")
  (norg/map-roots (lambda () (norg/show-children n)))
  (org-reveal) ; see ++about-uses-of-org-reveal++
  )

;;;; ___________________________________________________________________________
;;;; ____ * show-children-from-all-roots/incremental

(nomis/define-repeated-command-stuff
    -norg/show-children-from-all-roots/incremental
  -norg/show-children-from-all-roots/incremental/with-stuff/set
  #'norg/levels/max-in-buffer
  #'norg/show-children-from-all-roots)

(defun norg/with-show-children-from-all-roots-stuff* (fun)
  (let* ((*-norg/level-format-string*
          (or *-norg/level-format-string*
              "[%s of %s] from all roots")))
    (funcall fun)))

(cl-defmacro norg/with-show-children-from-all-roots-stuff (&body body)
  (declare (indent 0))
  `(norg/with-show-children-from-all-roots-stuff* (lambda () ,@body)))

(defun norg/show-children-from-all-roots/set-0 ()
  (interactive)
  (save-excursion ; Keep this -- it allows point to become hidden
    (goto-char 1) ; share the same point->level mapping
    (let* ((new-level 0))
      (norg/with-show-children-from-all-roots-stuff
        (-norg/show-children-from-all-roots/incremental/with-stuff/set
         new-level)))))

(defun norg/show-children-from-all-roots/fully-expand ()
  (interactive)
  (save-excursion ; Keep this -- it allows point to become hidden
    (goto-char 1) ; share the same point->level mapping
    (let* ((new-level (norg/levels/max-in-buffer)))
      (norg/with-show-children-from-all-roots-stuff
        (-norg/show-children-from-all-roots/incremental/with-stuff/set
         new-level)))))

(defun norg/show-children-from-all-roots/incremental/less ()
  (interactive)
  (save-excursion ; Keep this -- it allows point to become hidden
    (goto-char 1) ; share the same point->level mapping
    (let* ((new-level (->> (norg/map-roots
                            #'norg/levels/level-for-incremental-contract)
                           (apply #'max))))
      (norg/with-show-children-from-all-roots-stuff
        (-norg/show-children-from-all-roots/incremental/with-stuff/set
         new-level)))))

(defun norg/show-children-from-all-roots/incremental/more ()
  (interactive)
  (save-excursion ; Keep this -- it allows point to become hidden
    (goto-char 1) ; share the same point->level mapping
    (let* ((new-level (->> (norg/map-roots
                            (lambda ()
                              (or (norg/levels/smallest-invisible-level-below-point/or-nil)
                                  -norg/plus-infinity)))
                           (apply #'min))))
      (norg/with-show-children-from-all-roots-stuff
        (-norg/show-children-from-all-roots/incremental/with-stuff/set
         new-level)))))

;;;; ___________________________________________________________________________
;;;; ____ * norg/show-all-to-current-level

(defun norg/show-all-to-current-level ()
  (interactive)
  (norg/with-show-children-from-all-roots-stuff
    (-norg/show-children-from-all-roots/incremental/with-stuff/set
     (1- (norg/current-level)))))

;;;; ___________________________________________________________________________
;;;; * End

(provide 'norg)
