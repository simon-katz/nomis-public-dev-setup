;;;; norg --- A layer on top of Org mode  ---  -*- lexical-binding: t -*-

(progn) ; this-stops-hs-hide-all-from-hiding-the-next-comment

;;;; ___________________________________________________________________________
;;;; ____ * TODOs

;;;; TODO Remove all mentions of `nomis`.

;;;; TODO If you call `norg/show-children`, `norg/show-children-from-all-roots`
;;;;      or `norg/show-children-from-all-roots` directly, you don't go through
;;;;      the `-norg/set-level-etc` logic.

;;;; TODO There's a bug in incremental collapsing when there a child is more
;;;;      than one level deeper than its parent.

;;;; TODO When getting to 0 or max, first flash then cycle.

;;;; TODO Look at expansion of headlines with bodies (or whatever they
;;;;      are called).
;;;;      (Bodies are not being expanded. Maybe want a way to expand them.)
;;;;      Perhaps you could have an extra level between your current levels;
;;;;      they'd differ by whether bodies are shown.
;;;;      (And is there anything else apart from bodies to consider?)

;;;; TODO Ellipsis symbols disappear in some places while popup is being
;;;;      displayed.

;;;; TODO Sometimes things take a long time and a busy cursor would be useful.

;;;; TODO At the beginning of the commands, go to beginning of
;;;;      - the headline, or
;;;;      - the current top-level headline
;;;;      as appropriate that the position lookup works well.
;;;;      Do this inside a `save-excursion`.
;;;;      For the commands whose scope is the whole file you don't need to
;;;;      record positions (or record a position of 1 always).

;;;; TODO Idea of treating level -1 as show only parents, and not siblings.
;;;;      But first:
;;;;      - Can you detect a parents-not-siblings state? (Remember: you are
;;;;        no longer storing state for positions. (But you could in this
;;;;        special case, I guess. But that's getting complicated. It was nice
;;;;        to get rid of the state.))
;;;;      - Consider whether there's anything else you might want to do with
;;;;        visibility.
;;;;      - Sort out visibiity issues that are mentioned above:
;;;;        - Bodies not being expanded.
;;;;        - (Anything else?)

;;;; ___________________________________________________________________________
;;;; ____ * Require things

(require 'org)
(require 'cl)
(require 'dash)
(require 'dash-functional)

;;;; Things that you might want to make into packages if you make norg into a
;;;; package.

(require 'nomis-popup nil t)

;;;; ___________________________________________________________________________
;;;; ____ * Tailor other functionality

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;;; ____ ** norg/popup/message

(defun norg/popup/message (format-string &rest args)
  (apply (if (not (featurep 'nomis-popup))
             #'message
           #'nomis/popup/message)
         format-string
         args))

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
      (when out-of-range?
        (nomis/grab-user-attention/low))
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
;;;; ____ ** show-children

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

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;;; ____ ** show-children/incremental

(defun -norg/set-level-etc/show-children (level)
  (-norg/set-level-etc #'norg/show-children
                       level
                       (norg/n-levels-below)
                       "[%s / %s]"))

(defun norg/show-children/set-0 ()
  (interactive)
  (-> 0
      -norg/set-level-etc/show-children))

(defun norg/show-children/fully-expand ()
  (interactive)
  (-> :max
      -norg/set-level-etc/show-children))

(defun norg/show-children/incremental/less ()
  (interactive)
  (-> (norg/level-for-incremental-contract)
      -norg/set-level-etc/show-children))

(defun norg/show-children/incremental/more ()
  (interactive)
  (-> (norg/smallest-invisible-level-below-or-infinity)
      -norg/set-level-etc/show-children))

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;;; ____ ** show-children-from-root

(defun norg/show-children-from-root (n)
  (interactive "^p")
  "Call `norg/show-children` on the current root headline, with N as
the parameter."
  (norg/save-excursion-to-root
    (norg/show-children n)))

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;;; ____ ** show-children-from-root/incremental

(defun -norg/set-level-etc/show-children-from-root (level)
  (-norg/set-level-etc #'norg/show-children-from-root
                       level
                       (norg/n-levels-below/root)
                       "[%s of %s] from root"))

(defun norg/show-children-from-root/set-0 ()
  (interactive)
  (-> 0
      -norg/set-level-etc/show-children-from-root))

(defun norg/show-children-from-root/fully-expand ()
  (interactive)
  (-> :max
      -norg/set-level-etc/show-children-from-root))

(defun norg/show-children-from-root/incremental/less ()
  (interactive)
  (-> (norg/level-for-incremental-contract/root)
      -norg/set-level-etc/show-children-from-root))

(defun norg/show-children-from-root/incremental/more ()
  (interactive)
  (-> (norg/smallest-invisible-level-below-or-infinity/root)
      -norg/set-level-etc/show-children-from-root))

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;;; ____ ** show-children-from-all-roots

(defun norg/show-children-from-all-roots (n)
  "Call `norg/show-children` on all root headlines, with N as
the parameter."
  (interactive "^p")
  (norg/map-roots (lambda () (norg/show-children n))))

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;;; ____ ** show-children-from-all-roots/incremental

(defun -norg/set-level-etc/show-children-from-all-roots (level)
  (-norg/set-level-etc #'norg/show-children-from-all-roots
                       level
                       (norg/n-levels-below/buffer)
                       "[%s of %s] from all roots"))

(defun norg/show-children-from-all-roots/set-0 ()
  (interactive)
  (-> 0
      -norg/set-level-etc/show-children-from-all-roots))

(defun norg/show-children-from-all-roots/fully-expand ()
  (interactive)
  (-> :max
      -norg/set-level-etc/show-children-from-all-roots))

(defun norg/show-children-from-all-roots/incremental/less ()
  (interactive)
  (->> (norg/level-for-incremental-contract/buffer)
       -norg/set-level-etc/show-children-from-all-roots))

(defun norg/show-children-from-all-roots/incremental/more ()
  (interactive)
  (->> (norg/smallest-invisible-level-below/or-infinity/buffer)
       -norg/set-level-etc/show-children-from-all-roots))

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;;; ____ ** norg/show-all-to-current-level

(defun norg/show-all-to-current-level ()
  (interactive)
  (-> (1- (norg/current-level))
      -norg/set-level-etc/show-children-from-all-roots))

;;;; ___________________________________________________________________________
;;;; ____ * End

(provide 'norg)
