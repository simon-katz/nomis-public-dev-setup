;;;; norg --- A layer on top of Org mode  ---  -*- lexical-binding: t -*-

(progn) ; this-stops-hs-hide-all-from-hiding-the-next-comment

;;;; ___________________________________________________________________________
;;;; ____ * TODOs

;;;; TODO Finish destroying `nomis-repeated-commands`, or put new stuff there.

;;;; TODO Look into which `save-excursion`s and `(goto-char 1)`s are needed.

;;;; TODO Set up lexical binding.

;;;; TODO There's a bug in incremental collapsing when there a child is more
;;;;      than one level deeper than its parent.

;;;; TODO This is a mix of personal things and things that could be good in
;;;;      a library.
;;;;      Move stuff to "nomis-org-setup.el".
;;;;      Maybe have a third file for just key bindings.

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

;;;; TODO So, are you moving away from the incremental thing?
;;;;      - (No... Ah! - Add a timer purge with a small timeout?)
;;;;      - (No... Ah! - Use the last-command thing, and command families, and
;;;;        place.)
;;;;      - Oh, I think you /are/ scrapping the incremental thing -- tree-info!
;;;;        - Stateless is good.

;;;; TODO When getting to 0 or max, first flash then cycle.

;;;; TODO Look at expansion of headlines with bodies (or whatever they
;;;;      are called).
;;;;      (Bodies are not being expanded. Maybe want a way to expand them.)

;;;; TODO For from-root and for from-all-roots, make the initial value be the
;;;;      current level.

;;;; TODO Ellipsis symbols disappear in some places while popup is being
;;;;      displayed.

;;;; TODO Getting rid of old markers: `(set-marker m1 nil)`.
;;;;      - When buffer closes.
;;;;      - A time limit? (No, I don't think so.)
;;;;      - More? eg by time? by number for a buffer?

;;;; TODO Want to not have to show point. Can you have it not move point when
;;;;      you hide point?
;;;;      - This has started doing what you want for the `from-all-roots` stuff.
;;;;        Is that related to the `save-excursion`s you've added?
;;;;        - Yes! I've added `save-excursion`s for the `from-root` stuff,
;;;;          and that's doing the same.
;;;;     - Cool, buy why?

;;;; TODO Put the popup stuff somewhere new.

;;;; TODO Get rid of the hash table. Use something simple.
;;;; TODO Use of `ht-find`: Can you just lookup by key?

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
;;;; ____ * Towards a nicer API

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;;; ____ ** Last command

(defun nomis/org/last-command ()
  (or (bound-and-true-p *nomis/smex/last-command*)
      last-command))

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;;; ____ ** Infinity

(defconst -nomis/org/plus-infinity   1.0e+INF)
(defconst -nomis/org/minus-infinity -1.0e+INF)

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;;; ____ ** Some wrappers for org functionality

(defun nomis/org/report-org-info ()
  (interactive)
  (message "Current level = %s%s"
           (nomis/org/current-level)
           (if (not (fboundp 'nomis/point-etc-string))
               ""
             (concat "    "
                     (nomis/point-etc-string)))))

(defun nomis/org/point-is-visible? ()
  (not (get-char-property (point) 'invisible)))

(defun nomis/org/map-roots (fun)
  (let* ((dummy (cons nil nil)))
    (remove dummy
            (org-map-entries (lambda ()
                               (let* ((level (nomis/org/current-level)))
                                 (if (= level 1)
                                     (funcall fun)
                                   dummy)))
                             t
                             'file))))

(defun nomis/org/current-level ()
  (nth 1 (org-heading-components)))

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;;; ____ ** Things I did before I had tree-info -- perhaps redo with tree-info

(defun -nomis/org/levels/below-point-helper (pred-of-no-args
                                             reducing-function)
  ;; TODO Think about what `reduce` does.
  ;;      Maybe add initial value, and value to return when you get a nil.
  (let* ((max-level-beneath
          (let* ((sofar nil))
            (org-map-entries (lambda ()
                               (when (funcall pred-of-no-args)
                                 (let* ((v (nomis/org/current-level)))
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
         (nomis/org/current-level)))))

(defun nomis/org/levels/n-below-point ()
  (-nomis/org/levels/below-point-helper (lambda () t)
                                        #'max))

(defun nomis/org/levels/smallest-invisible-level-below-point/or-nil ()
  (let* ((not-visible? (-compose #'not
                                 #'nomis/org/point-is-visible?)))
    (-nomis/org/levels/below-point-helper not-visible?
                                          #'min)))

(defun nomis/org/levels/max-below-root ()
  (save-excursion
    (nomis/org/goto-root)
    (nomis/org/levels/n-below-point)))

(defun nomis/org/levels/max-in-buffer ()
  (let* ((sofar 0))
    (org-map-entries (lambda ()
                       (setq sofar (max (nomis/org/current-level)
                                        sofar)))
                     t
                     'file)
    sofar))

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;;; ____ ** The idea of tree-info, and things that use it

(defun -nomis/org/tree-info ()
  (org-map-entries (lambda ()
                     (list (nomis/org/current-level)
                           (nomis/org/point-is-visible?)))
                   t
                   'tree))

(defun -nomis/org/tree-info/with-dummy-invisible-levels ()
  (let* ((dummy-initial-entry '(:dummy-first t nil))
         (basic-info (-nomis/org/tree-info)))
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

(defun -nomis/org/max-level (&optional tree-info)
  (setq tree-info (or tree-info (-nomis/org/tree-info)))
  (cl-loop for (level visible? dummy?)
           in tree-info
           when (not dummy?)
           maximize level))

(defun -nomis/org/fully-expanded? (&optional tree-info)
  (setq tree-info (or tree-info (-nomis/org/tree-info)))
  (cl-loop for (level visible? dummy?)
           in tree-info
           when (not dummy?)
           always visible?))

(defun nomis/org/levels/level-for-incremental-contract (&optional tree-info)
  ;; Collapse the most-deeply-nested expanded level, and expand everything
  ;; else to that level.
  (setq tree-info (or tree-info
                      (-nomis/org/tree-info/with-dummy-invisible-levels)))
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
    (- v (nomis/org/current-level))))

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;;; ____ ** show-children

(defun nomis/org/show-children (n)
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
      (-nomis/org/collapse))
    (outline-show-children n)))

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;;; ____ ** show-children/incremental

(nomis/define-repeated-command-stuff
    -nomis/org/show-children/incremental
  -nomis/org/show-children/incremental/with-stuff/set ; TODO Can you just use `nomis/org/show-children`? -- Ah! Is it just the popup you still need?
  #'nomis/org/levels/n-below-point
  #'nomis/org/show-children)

(defun nomis/org/show-children/level-formatter (v maximum)
  (format "[%s of %s]" v maximum))

(defun nomis/org/with-show-children-stuff* (fun)
  (let* ((*nomis/drcs/level-formatter*
          #'nomis/org/show-children/level-formatter))
    (funcall fun)))

(cl-defmacro nomis/org/with-show-children-stuff (&body body)
  (declare (indent 0))
  `(nomis/org/with-show-children-stuff* (lambda () ,@body)))

(defun nomis/org/show-children/set-0/impl ()
  (interactive)
  (let* ((new-level 0))
    (-nomis/org/show-children/incremental/with-stuff/set new-level)))

(defun nomis/org/show-children/fully-expand/impl ()
  (interactive)
  (let* ((new-level (nomis/org/levels/n-below-point)))
    (-nomis/org/show-children/incremental/with-stuff/set new-level)))

(defun nomis/org/show-children/incremental/less/impl ()
  (interactive)
  (let* ((new-level (nomis/org/levels/level-for-incremental-contract)))
    (-nomis/org/show-children/incremental/with-stuff/set new-level)))

(defun nomis/org/show-children/incremental/more/impl ()
  (interactive)
  (let* ((new-level
          (or (nomis/org/levels/smallest-invisible-level-below-point/or-nil)
              -nomis/org/plus-infinity)))
    (-nomis/org/show-children/incremental/with-stuff/set new-level)))

(defun nomis/org/show-children/set-0 ()
  (interactive)
  (nomis/org/with-show-children-stuff
    (nomis/org/show-children/set-0/impl)))

(defun nomis/org/show-children/fully-expand ()
  (interactive)
  (nomis/org/with-show-children-stuff
    (nomis/org/show-children/fully-expand/impl)))

(defun nomis/org/show-children/incremental/less ()
  (interactive)
  (nomis/org/with-show-children-stuff
    (nomis/org/show-children/incremental/less/impl)))

(defun nomis/org/show-children/incremental/more ()
  (interactive)
  (nomis/org/with-show-children-stuff
    (nomis/org/show-children/incremental/more/impl)))

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;;; ____ ** show-children-from-root

(defun nomis/org/show-children-from-root (n)
  (interactive "^p")
  "Call `nomis/org/show-children` on the current root headline, with N as
the parameter.
When done, call `org-reveal` so that the current point is shown.
But see ++about-uses-of-org-reveal++"
  (save-excursion
    (nomis/org/goto-root)
    (nomis/org/show-children n))
  (org-reveal) ; see ++about-uses-of-org-reveal++
  )

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;;; ____ ** show-children-from-root/incremental

(nomis/define-repeated-command-stuff
    -nomis/org/show-children-from-root/incremental
  -nomis/org/show-children-from-root/incremental/with-stuff/set
  #'nomis/org/levels/max-below-root
  #'nomis/org/show-children-from-root)

(defun nomis/org/show-children-from-root/level-formatter (v maximum)
  (format "[%s of %s] from root" v maximum))

(defun nomis/org/with-show-children-from-root-stuff* (fun)
  (let* ((*nomis/drcs/level-formatter*
          #'nomis/org/show-children-from-root/level-formatter))
    (funcall fun)))

(cl-defmacro nomis/org/with-show-children-from-root-stuff (&body body)
  (declare (indent 0))
  `(nomis/org/with-show-children-from-root-stuff* (lambda () ,@body)))

(defun nomis/org/show-children-from-root/set-0 ()
  (interactive)
  (save-excursion
    (nomis/org/goto-root)
    (nomis/org/with-show-children-from-root-stuff
      (nomis/org/show-children/set-0/impl))))

(defun nomis/org/show-children-from-root/fully-expand ()
  (interactive)
  (save-excursion
    (nomis/org/goto-root)
    (nomis/org/with-show-children-from-root-stuff
      (nomis/org/show-children/fully-expand/impl))))

(defun nomis/org/show-children-from-root/incremental/less ()
  (interactive)
  (save-excursion
    (nomis/org/goto-root)
    (nomis/org/with-show-children-from-root-stuff
      (nomis/org/show-children/incremental/less/impl))))

(defun nomis/org/show-children-from-root/incremental/more ()
  (interactive)
  (save-excursion
    (nomis/org/goto-root)
    (nomis/org/with-show-children-from-root-stuff
      (nomis/org/show-children/incremental/more/impl))))

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;;; ____ ** show-children-from-all-roots

(defun nomis/org/show-children-from-all-roots (n)
  "Call `nomis/org/show-children` on all root headlines, with N as
the parameter.
When done, call `org-reveal` so that the current point is shown.
But see ++about-uses-of-org-reveal++"
  (interactive "^p")
  (nomis/org/map-roots (lambda () (nomis/org/show-children n)))
  (org-reveal) ; see ++about-uses-of-org-reveal++
  )

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;;; ____ ** show-children-from-all-roots/incremental

(nomis/define-repeated-command-stuff
    -nomis/org/show-children-from-all-roots/incremental
  -nomis/org/show-children-from-all-roots/incremental/with-stuff/set
  #'nomis/org/levels/max-in-buffer
  #'nomis/org/show-children-from-all-roots)

(defun nomis/org/show-children-from-all-roots/level-formatter (v maximum)
  (format "[%s of %s] from all roots" v maximum))

(defun nomis/org/with-show-children-from-all-roots-stuff* (fun)
  (let* ((*nomis/drcs/level-formatter*
          #'nomis/org/show-children-from-all-roots/level-formatter))
    (funcall fun)))

(cl-defmacro nomis/org/with-show-children-from-all-roots-stuff (&body body)
  (declare (indent 0))
  `(nomis/org/with-show-children-from-all-roots-stuff* (lambda () ,@body)))

(defun nomis/org/show-children-from-all-roots/set-0 ()
  (interactive)
  (save-excursion ; Keep this -- it allows point to become hidden
    (goto-char 1) ; share the same point->level mapping
    (let* ((new-level 0))
      (nomis/org/with-show-children-from-all-roots-stuff
        (-nomis/org/show-children-from-all-roots/incremental/with-stuff/set
         new-level)))))

(defun nomis/org/show-children-from-all-roots/fully-expand ()
  (interactive)
  (save-excursion ; Keep this -- it allows point to become hidden
    (goto-char 1) ; share the same point->level mapping
    (let* ((new-level (nomis/org/levels/max-in-buffer)))
      (nomis/org/with-show-children-from-all-roots-stuff
        (-nomis/org/show-children-from-all-roots/incremental/with-stuff/set
         new-level)))))

(defun nomis/org/show-children-from-all-roots/incremental/less ()
  (interactive)
  (save-excursion ; Keep this -- it allows point to become hidden
    (goto-char 1) ; share the same point->level mapping
    (let* ((new-level (->> (nomis/org/map-roots
                            #'nomis/org/levels/level-for-incremental-contract)
                           (apply #'max))))
      (nomis/org/with-show-children-from-all-roots-stuff
        (-nomis/org/show-children-from-all-roots/incremental/with-stuff/set
         new-level)))))

(defun nomis/org/show-children-from-all-roots/incremental/more ()
  (interactive)
  (save-excursion ; Keep this -- it allows point to become hidden
    (goto-char 1) ; share the same point->level mapping
    (let* ((new-level (->> (nomis/org/map-roots
                            (lambda ()
                              (or (nomis/org/levels/smallest-invisible-level-below-point/or-nil)
                                  -nomis/org/plus-infinity)))
                           (apply #'min))))
      (nomis/org/with-show-children-from-all-roots-stuff
        (-nomis/org/show-children-from-all-roots/incremental/with-stuff/set
         new-level)))))

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;;; ____ ** nomis/org/show-all-to-current-level

(defun nomis/org/show-all-to-current-level ()
  (interactive)
  (nomis/org/with-show-children-from-all-roots-stuff
    (-nomis/org/show-children-from-all-roots/incremental/with-stuff/set
     (nomis/org/current-level))))

(provide 'norg)
