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

;;;; TODO Sometimes things take a long time and a busy cursor would be useful.

;;;; TODO Command to set a particular level and remember the setting.

;;;; TODO Command to show all roots to current headline's level.

;;;; TODO Want to not have to show point. Can you have it not move point when
;;;;      you hide point?

;;;; TODO Can you show the popup for just a brief time?

;;;; TODO The positions you record should be for the headline that is being
;;;;      worked on.
;;;;      So somtimes the current root and not point.
;;;;      And for the commands whose scope is the whole file you don't need to
;;;;      record positions.

;;;; TODO At the beginning of the commands, go to beginning of the headline so
;;;;      that the position lookup works well.
;;;;      Do this inside a `save-excursion`.

;;;; TODO Fix the nasty macros.
;;;;      - Can you functionify some of it?
;;;;      - Macro hygiene.

;;;; TODO Use of `ht-find`: Can you just lookup by key?

;;;; TODO Getting rid of old markers: `(set-marker m1 nil)`.
;;;;      - When buffer closes.
;;;;      - A time limit? (No, I don't think so.)
;;;;      - More? eg by time? by number for a buffer?

;;;; ___________________________________________________________________________
;;;; ____ * Require things

(require 'ht)
(require 'popup nil t)

;;;; ___________________________________________________________________________
;;;; * Debug support

(defvar -nomis/drcs/debug? nil)

(defun -nomis/drcs/debug-message (format-string &rest args)
  (when -nomis/drcs/debug?
    (apply message format-string args)))

;;;; ___________________________________________________________________________
;;;; * nomis/define-repeated-command-stuff

(defun -nomis/drcs/place= (place1 place2)
  (-nomis/drcs/debug-message ">>>> Comparing %s and %s" place1 place2)
  (let* ((res (condition-case err
                  (and (= ; This can compare positions (which are integers) and markers.
                        (first place1)
                        (first place2))
                       (equal (rest place1)
                              (rest place2)))
                (error
                 (nomis/grab-user-attention/high)
                 (-nomis/drcs/debug-message "******** -nomis/drcs/place= error: %s" (cdr err))
                 (-nomis/drcs/debug-message "place1 = %s" place1)
                 (-nomis/drcs/debug-message "place2 = %s" place2)
                 nil))))
    (-nomis/drcs/debug-message "<<<< Comparing -- result is %s" res)
    res))

(defun -nomis/drcs/hash (place)
  (let* ((res (let* ((marker-or-position (first place))
                     (position (if (markerp marker-or-position)
                                   (marker-position marker-or-position)
                                 marker-or-position)))
                (+ ;; (sxhash-eq position)  ; TODO Oh, you can't hash on the position, because the position changes.
                 (sxhash-equal (rest place))))))
    (-nomis/drcs/debug-message "Hash result = %s" res)
    res))

(define-hash-table-test 'nomis/drcs/ht-test
  '-nomis/drcs/place=
  '-nomis/drcs/hash)

;; (progn

;;   (defun nomis/make-marker (v) ; use (point-marker)
;;     (let* ((m (make-marker)))
;;       (set-marker m v)
;;       m))

;;   (defvar xx-ht (ht-create 'nomis/drcs/ht-test))

;;   (setq m1 (nomis/make-marker 100))
;;   (setq m2 (nomis/make-marker 200))

;;   (ht-set! xx-ht (list m1 :some-buffer) 999)

;;   (list (ht-get xx-ht (list m1 :some-buffer))
;;         (ht-get xx-ht (list 100 :some-buffer))
;;         (ht-get xx-ht (list m2 :some-buffer))))

(defun -nomis/drcs/do-the-biz (name
                               previous-values-ht
                               value-fun
                               fun-to-call-with-new-value)
  (let* ((current-place (list (point)
                              (current-buffer)))
         (previous-value (ht-get previous-values-ht
                                 current-place))
         (previous-marker (when previous-value
                            (let* ((kv (ht-find (lambda (k v) (-nomis/drcs/place= k current-place))
                                                previous-values-ht))
                                   (marker (caar kv)))
                              (-nomis/drcs/debug-message "kv = %s" kv)
                              (assert (markerp marker))
                              marker)))
         (_ (-nomis/drcs/debug-message "previous-value = %s" previous-value))
         (new-value (funcall value-fun previous-value)))
    ;; We remove the current entry because we want to nullify old markers.
    ;; If we didn't remove the current entry, when we update, the key with
    ;; the old marker would stay in the table and the value would be
    ;; replaced.
    ;; Maybe you could not create a marker when the key is not already
    ;; in the table, but then you would be relying on hash table
    ;; implementation details.
    (ht-remove! previous-values-ht
                current-place)
    (ht-set! previous-values-ht
             (let* ((marker (point-marker)))
               (set-marker-insertion-type marker t)
               (list marker
                     (current-buffer)))
             new-value)
    (when previous-marker
      (-nomis/drcs/debug-message "Nullifying marker %s" previous-marker)
      ;; Debugging: I think this assertion might fail.
      ;; **** YOU ARE HERE. THIS IS FAILING AFTER YOU INSERT TEXT IN THE
      ;;      BUFFER, BUT IT"S OK BEFORE THAT.
      (let* ((kv (ht-find (lambda (k v) (-nomis/drcs/place= k current-place))
                          previous-values-ht))
             (marker (caar kv)))
        (-nomis/drcs/debug-message "marker = %s" marker)
        (-nomis/drcs/debug-message "previous-marker = %s" previous-marker)
        (assert (not (eql marker previous-marker))))
      ;; Stop no-longer needed marker from slowing down editing, and
      ;; allow it to be garbage collected.
      ;; Do this now, after updating the hash table, otherwise you break
      ;; the hash table.
      (set-marker previous-marker nil))
    (prog1
        (funcall fun-to-call-with-new-value new-value)
      (if (featurep 'popup)
          (popup-tip (format "[%s]" new-value))
        (message "%s value = %s" name new-value)))))

(cl-defmacro nomis/define-repeated-command-stuff (name
                                                  fun-to-call-with-new-value
                                                  with-stuff-name/incremental
                                                  with-stuff-name/set
                                                  previous-values-var-name
                                                  next-value)
  (declare (indent 1))
  `(progn

     (defvar ,name nil) ; so that definition can be found -- and must provide a value for that to work!

     (defvar ,previous-values-var-name (ht-create 'nomis/drcs/ht-test))

     (defun ,with-stuff-name/incremental (initial-value
                                          in-value)
       (-nomis/drcs/debug-message "________________________________________")
       (-nomis/drcs/do-the-biz ',name
                               ,previous-values-var-name
                               (lambda (previous-value)
                                 (if previous-value
                                     (let* ((%in-value% in-value)
                                            (%previous-value% previous-value))
                                       ,next-value)
                                   initial-value))
                               ',fun-to-call-with-new-value))

     (defun ,with-stuff-name/set (value)
       ;; TODO This is hacky. Need to update the previous-value thing.
       ;;      Some refactoring needed.
       (-nomis/drcs/debug-message "________________________________________")
       (-nomis/drcs/do-the-biz ',name
                               ,previous-values-var-name
                               (lambda (_) value)
                               ',fun-to-call-with-new-value))))

;;;; ___________________________________________________________________________
;;;; * End

(provide 'nomis-repeated-commands)
