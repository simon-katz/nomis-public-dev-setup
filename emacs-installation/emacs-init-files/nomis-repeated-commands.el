;;;; nomis-repeated-commands.el ---  -*- lexical-binding: t -*-

;;;; ___________________________________________________________________________
;;;; ____ * TODO

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

;;;; TODO Command to show all roots to current headline's level.

;;;; ___________________________________________________________________________
;;;; ____ * Require things

(require 'popup nil t)

;;;; ___________________________________________________________________________
;;;; * nomis/define-repeated-command-stuff

(defun -nomis/drcs/bring-within-range (v maximum)
  (when (or (< v 0)
            (> v maximum))
    (nomis/grab-user-attention/low))
  (min (max 0 v)
       maximum))

(defvar *nomis/drcs/level-formatter* nil)

(defun -nomis/drcs/default-level-formatter (v maximum)
  (format "[%s / %s]" v maximum))

(defvar -nomis/drcs/most-recent-popup nil)

(defun -nomis/drcs/do-the-biz (name
                               maximum
                               value
                               new-value-action-fun)
  (let* ((new-value (-> value
                        (-nomis/drcs/bring-within-range maximum))))
    (prog1
        (funcall new-value-action-fun new-value)
      (let* ((msg (funcall (or *nomis/drcs/level-formatter*
                               #'-nomis/drcs/default-level-formatter)
                           new-value
                           maximum)))
        (if (not (featurep 'popup))
            (message "%s value = %s" name new-value)
          (run-at-time 0
                       nil
                       (lambda ()
                         (when (and -nomis/drcs/most-recent-popup
                                    (popup-live-p -nomis/drcs/most-recent-popup))
                           (popup-delete -nomis/drcs/most-recent-popup)
                           (setq -nomis/drcs/most-recent-popup nil))
                         (let* ((popup
                                 (popup-tip msg
                                            :nowait t
                                            :point (save-excursion
                                                     (unless (get-char-property
                                                              (point)
                                                              'invisible)
                                                       (ignore-errors
                                                         (previous-line)))
                                                     (point)))))
                           (setq -nomis/drcs/most-recent-popup popup)
                           (run-at-time 1
                                        nil
                                        (lambda ()
                                          (when (popup-live-p popup)
                                            (popup-delete popup))))))))))))

(cl-defmacro nomis/define-repeated-command-stuff (name
                                                  with-stuff-name/set
                                                  maximum-fun
                                                  new-value-action-fun)
  (declare (indent 1))
  `(progn

     (defvar ,name nil) ; so that definition can be found -- and must provide a value for that to work!

     (defun ,with-stuff-name/set (value)
       (let* ((maximum (funcall ,maximum-fun)))
         (-nomis/drcs/do-the-biz ',name
                                 maximum
                                 value
                                 ,new-value-action-fun)))))

;;;; ___________________________________________________________________________
;;;; * End

(provide 'nomis-repeated-commands)
