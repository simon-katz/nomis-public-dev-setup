;;;; nomis-popup --- A layer on top of popup  ---  -*- lexical-binding: t -*-

(progn) ; this stops `hs-hide-all` from hiding the next comment

;;;; ___________________________________________________________________________
;;;; ____ * Require things

(require 'nomis-scrolling)

;;;; ___________________________________________________________________________
;;;; ____ * Parameterisation

(defvar nomis/popup/duration 1)

;;;; ___________________________________________________________________________
;;;; ____ * Private parameterisation

(defvar -nomis/popup/prefix " ")
(defvar -nomis/popup/suffix " ")
(defvar -nomis/popup/error-prefix " ")
(defvar -nomis/popup/error-suffix " ")

(defvar -nomis/popup/muted-yellow "#fefd90")

(defface -nomis/popup/face
  `((t (:foreground "black" :background ,-nomis/popup/muted-yellow)))
  "Face used for popups.")

(defvar -nomis/popup/pink "pink")

(defface -nomis/popup/error-face
  `((t (:foreground "black" :background ,-nomis/popup/pink)))
  "Face used for popups.")

;;;; ___________________________________________________________________________
;;;; ____ * nomis/popup/message

(defun -make-nomis-popup-overlay (sticky? face start-pos end-pos &rest props)
  (let* ((ov (make-overlay start-pos end-pos)))
    (overlay-put ov 'category (if sticky?
                                  'nomis-popup/sticky
                                'nomis-popup/non-sticky))
    (overlay-put ov 'face     face)
    (while props (overlay-put ov (pop props) (pop props)))
    ov))

(defun -nomis/popup/point-invisible? (&optional pos)
  ;; Copied from `org-invisible-p`. Why "after POS" (in doc string)?
  "Non-nil if the character after POS is invisible.
If POS is nil, use `point' instead."
  (get-char-property (or pos (point)) 'invisible))

(defun -nomis/popup/a-good-popup-position ()
  (save-excursion
    ;; If point is invisible, go back to a visible point.
    (cl-loop while (and (-nomis/popup/point-invisible?)
                        (> (point) (point-min)))
             do (backward-char))
    (when (< (point) (window-start))
      ;; The point we found is off-screen, so go forward instead.
      (cl-loop do (forward-char)
               while (and (-nomis/popup/point-invisible?)
                          (< (point) (point-max)))))
    ;; Go to beginning of line.
    (beginning-of-line)
    ;; We'll pop up on the previous line if that's on-screen and visible,
    ;; otherwise on the current line.
    ;; We use `previous-line` and `next-line` below. Doc strings say they
    ;; not for programmatic use, but the things it suggests to use instead
    ;; don't do what we want -- we want screen lines (we want to jump over
    ;; invisible lines).
    (unless (= (nomis/line-no-in-window) 1)
      (previous-line)
      ;; Sometimes org mode gets into a state where there's a strange invisible
      ;; line at the top of the window, so check for that.
      (when (-nomis/popup/point-invisible?)
        (next-line)))
    ;; We're done. Where are we?
    (point)))

(defun -nomis/popup/remove-non-sticky-popups ()
  (remove-overlays nil nil 'category 'nomis-popup/non-sticky))

(add-hook 'pre-command-hook '-nomis/popup/remove-non-sticky-popups)

;;;; Useful in dev:
;;;;   (remove-overlays nil nil 'category 'nomis-popup/sticky)

(defun -nomis/popup/message* (sticky? popup-pos face msg)
  (cl-flet ((n-chars-we-can-replace-at-pos
             (pos)
             (let* ((n-chars-before-eol
                     (save-excursion
                       (- (- (progn (goto-char pos) (point))
                             (progn (end-of-line) (point)))))))
               (or (loop for i from 0 to n-chars-before-eol
                         when (-nomis/popup/point-invisible? (+ pos i))
                         return (1- i))
                   n-chars-before-eol))))
    (-nomis/popup/remove-non-sticky-popups)
    (let* ((len (length msg))
           (msg-part-1-len (min len
                                (n-chars-we-can-replace-at-pos popup-pos)))
           (msg-part-1 (substring msg 0 msg-part-1-len))
           (msg-part-2 (substring msg msg-part-1-len)))
      (unless (equal msg-part-2 "")
        (put-text-property 0
                           (length msg-part-2)
                           'face
                           face
                           msg-part-2))
      (let* ((ov1-start-pos popup-pos)
             (ov2-start-pos (+ popup-pos msg-part-1-len))
             (ov1 (-make-nomis-popup-overlay sticky?
                                             face
                                             ov1-start-pos
                                             ov2-start-pos
                                             'display  msg-part-1))
             (ov2 (-make-nomis-popup-overlay sticky?
                                             face
                                             ov2-start-pos
                                             ov2-start-pos
                                             'before-string msg-part-2))
             (buffer (current-buffer)))
        (run-at-time nomis/popup/duration
                     nil
                     (lambda ()
                       (when (buffer-live-p buffer)
                         (with-current-buffer buffer
                           ;; TODO: Doc says these will still exist. Eek, a leak!
                           ;;       See https://www.gnu.org/software/emacs/manual/html_node/elisp/Managing-Overlays.html#Managing-Overlays
                           (delete-overlay ov1)
                           (delete-overlay ov2)))))))))

(defun nomis/popup/message (format-string &rest args)
  (-nomis/popup/message* nil
                         (-nomis/popup/a-good-popup-position)
                         '-nomis/popup/face
                         (concat -nomis/popup/prefix
                                 (apply #'format format-string args)
                                 -nomis/popup/suffix)))

(defun nomis/popup/error-message (format-string &rest args)
  (-nomis/popup/message* nil
                         (-nomis/popup/a-good-popup-position)
                         '-nomis/popup/error-face
                         (concat -nomis/popup/error-prefix
                                 (apply #'format format-string args)
                                 -nomis/popup/error-suffix)))

(defun nomis/popup/message-v2 (sticky? position format-string &rest args)
  (-nomis/popup/message* sticky?
                         position
                         '-nomis/popup/face
                         (concat -nomis/popup/prefix
                                 (apply #'format format-string args)
                                 -nomis/popup/suffix)))

(defun nomis/popup/error-message-v2 (sticky? position format-string &rest args)
  (-nomis/popup/message* sticky?
                         position
                         '-nomis/popup/error-face
                         (concat -nomis/popup/error-prefix
                                 (apply #'format format-string args)
                                 -nomis/popup/error-suffix)))

;;;; ___________________________________________________________________________
;;;; * End

(provide 'nomis-popup)
