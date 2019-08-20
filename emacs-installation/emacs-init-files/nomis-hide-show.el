;;;; Init stuff -- nomis-hide-show

;;;; TODO Consider more use of
;;;;          `nomis/goto-beginning-of-sexp/or-end/forward`
;;;;      and `nomis/goto-beginning-of-sexp/or-end/backward`.

;;;; ___________________________________________________________________________

(require 'hideshow)
(require 'cl)
(require 'subr-x)
(require 'nomis-sexp-utils)
(require 'nomis-key-chord)
(require 'nomis-hydra)
(require 'nomis-scrolling)

;;;; ___________________________________________________________________________
;;;; Stuff to maybe move

(defun nomis/line-at-point-without-newline ()
  (string-remove-suffix "\n" (thing-at-point 'line t)))

;;;; ___________________________________________________________________________
;;;; hide-show utilities

(defun nomis/hs/looking-at-beginning-of-hidden-sexp? ()
  (and (nomis/looking-at-bracketed-sexp-start)
       (hs-already-hidden-p)))

(cl-defmacro nomis/hs/with-only-if-looking-at-bracketed-sexp-start (&body body)
  ;; Use this when the body would move the cursor when not looking at
  ;; bracketed sexp start -- eg for `hs-show-block`, which moves the cursor
  ;; up a level when on an operator.
  (declare (indent 0))
  `(when (nomis/looking-at-bracketed-sexp-start)
     ,@body))

;;;; ___________________________________________________________________________

;;;; The fiddling you are doing with `hs-hide-comments-when-hiding-all` means
;;;; that:
;;;; - When you do a hide-all, comments are not hidden.
;;;; - Subsequent hide-alls toggle the hiding of comments.
;;;; This is reset when you do a show-all.

(make-variable-buffer-local 'hs-hide-comments-when-hiding-all)

(defun nomis/hs/hide-all ()
  (interactive)
  (hs-hide-all)
  (nomis/beginning-of-top-level-form) ; without this, a following show command does not-very-nice positioning of the cursor
  (setq hs-hide-comments-when-hiding-all
        (not hs-hide-comments-when-hiding-all)))

(defun nomis/hs/show-all ()
  (interactive)
  (hs-show-all)
  (setq hs-hide-comments-when-hiding-all nil))

(defun nomis/hs/hide-block ()
  (interactive)
  (nomis/hs/with-only-if-looking-at-bracketed-sexp-start
    (hs-hide-block)
    (backward-char)))

(defun nomis/hs/show-block ()
  (interactive)
  (nomis/hs/with-only-if-looking-at-bracketed-sexp-start
    (hs-show-block)
    (backward-char)))

(defun nomis/hs/toggle-hiding ()
  (interactive)
  (nomis/hs/with-only-if-looking-at-bracketed-sexp-start
    (hs-toggle-hiding)
    (backward-char)))

(define-key prog-mode-map (kbd "H-q H--")  'nomis/hs/hide-all)
(define-key prog-mode-map (kbd "H-q H-[")  'nomis/hs/adjust/set-0)
(define-key prog-mode-map (kbd "H-q H-'")  'nomis/hs/adjust/less)
(define-key prog-mode-map (kbd "H-q H-l")  'nomis/hs/adjust/set-level)
(define-key prog-mode-map (kbd "H-q H-\\") 'nomis/hs/adjust/more)
(define-key prog-mode-map (kbd "H-q H-]")  'nomis/hs/adjust/show-all)
(define-key prog-mode-map (kbd "H-q H-p")  'nomis/hs/adjust/show-all-for-top-level)
(define-key prog-mode-map (kbd "H-q H-=")  'nomis/hs/show-all)
(define-key prog-mode-map (kbd "H-q H-/")  'nomis/hs/toggle-hiding)
(define-key prog-mode-map (kbd "H-q H-0")  'nomis/hs/adjust/set-level/0)
(define-key prog-mode-map (kbd "H-q H-1")  'nomis/hs/adjust/set-level/1)
(define-key prog-mode-map (kbd "H-q H-2")  'nomis/hs/adjust/set-level/2)
(define-key prog-mode-map (kbd "H-q H-3")  'nomis/hs/adjust/set-level/3)
(define-key prog-mode-map (kbd "H-q H-4")  'nomis/hs/adjust/set-level/4)
(define-key prog-mode-map (kbd "H-q H-5")  'nomis/hs/adjust/set-level/5)
(define-key prog-mode-map (kbd "H-q H-6")  'nomis/hs/adjust/set-level/6)
(define-key prog-mode-map (kbd "H-q H-7")  'nomis/hs/adjust/set-level/7)
(define-key prog-mode-map (kbd "H-q H-8")  'nomis/hs/adjust/set-level/8)
(define-key prog-mode-map (kbd "H-q H-9")  'nomis/hs/adjust/set-level/9)

(key-chord-define prog-mode-map "q-"  'nomis/hs/hide-all)
(key-chord-define prog-mode-map "q["  'nomis/hs/adjust/set-0)
(key-chord-define prog-mode-map "q'"  'nomis/hs/adjust/less)
(key-chord-define prog-mode-map "ql"  'nomis/hs/adjust/set-level)
(key-chord-define prog-mode-map "q\\" 'nomis/hs/adjust/more)
(key-chord-define prog-mode-map "q]"  'nomis/hs/adjust/show-all)
(key-chord-define prog-mode-map "q="  'nomis/hs/show-all)
(key-chord-define prog-mode-map "q/"  'nomis/hs/toggle-hiding)
(key-chord-define prog-mode-map "q0"  'nomis/hs/adjust/set-level/0)
(key-chord-define prog-mode-map "q1"  'nomis/hs/adjust/set-level/1)
(key-chord-define prog-mode-map "q2"  'nomis/hs/adjust/set-level/2)
(key-chord-define prog-mode-map "q3"  'nomis/hs/adjust/set-level/3)
(key-chord-define prog-mode-map "q4"  'nomis/hs/adjust/set-level/4)
(key-chord-define prog-mode-map "q5"  'nomis/hs/adjust/set-level/5)
(key-chord-define prog-mode-map "q6"  'nomis/hs/adjust/set-level/6)
(key-chord-define prog-mode-map "q7"  'nomis/hs/adjust/set-level/7)
(key-chord-define prog-mode-map "q8"  'nomis/hs/adjust/set-level/8)
(key-chord-define prog-mode-map "q9"  'nomis/hs/adjust/set-level/9)

;;;; ___________________________________________________________________________
;;;; Set up `hs-set-up-overlay`

(defvar nomis/hs/top-level-separator-comment-regexp
  (concat ";;;;"
          (nomis/rx/or " _*"
                       (concat (nomis/rx/wrap " -") "*"))
          "$"))

(defun nomis/hs/top-level-separator-comment? (s)
  (string-match-p nomis/hs/top-level-separator-comment-regexp
                  s))

(defun nomis/hs/use-simple-ellipsis? (ov)
  (let* ((line (save-excursion
                 (goto-char (overlay-start ov))
                 (nomis/line-at-point-without-newline))))
    (nomis/hs/top-level-separator-comment? line)))

(defun nomis/hs/display-hidden-stuff (ov)
  (overlay-put ov 'help-echo
               (buffer-substring (overlay-start ov)
                                 (overlay-end ov)))
  (overlay-put ov 'display
               (propertize (if (nomis/hs/use-simple-ellipsis? ov)
                               "▶"
                             (format " ▶▶▶/%d"
                                     (count-lines (overlay-start ov)
                                                  (overlay-end ov))))
                           'face 'font-lock-type-face)))

(setq hs-set-up-overlay 'nomis/hs/display-hidden-stuff)

;;;; ___________________________________________________________________________

(defadvice goto-line (after expand-after-goto-line
                            activate compile)
  "hideshow-expand affected block when using goto-line in a collapsed buffer"
  (save-excursion
    (hs-show-block)))

;;;; ___________________________________________________________________________
;;;; nomis/hs/adjust

(defvar nomis/hs/adjust/level 0)

(defun nomis/hs/adjust/set-level (n)
  (interactive "p")
  (nomis/hs/with-only-if-looking-at-bracketed-sexp-start
    (setq nomis/hs/adjust/level n)
    (if (zerop n)
        (nomis/hs/hide-block)
      (hs-hide-level nomis/hs/adjust/level))))

(defun nomis/hs/adjust/set-level/0 ()
  (interactive)
  (nomis/hs/adjust/set-level 0))

(defun nomis/hs/adjust/set-level/1 ()
  (interactive)
  (nomis/hs/adjust/set-level 1))

(defun nomis/hs/adjust/set-level/2 ()
  (interactive)
  (nomis/hs/adjust/set-level 2))

(defun nomis/hs/adjust/set-level/3 ()
  (interactive)
  (nomis/hs/adjust/set-level 3))

(defun nomis/hs/adjust/set-level/4 ()
  (interactive)
  (nomis/hs/adjust/set-level 4))

(defun nomis/hs/adjust/set-level/5 ()
  (interactive)
  (nomis/hs/adjust/set-level 5))

(defun nomis/hs/adjust/set-level/6 ()
  (interactive)
  (nomis/hs/adjust/set-level 6))

(defun nomis/hs/adjust/set-level/7 ()
  (interactive)
  (nomis/hs/adjust/set-level 7))

(defun nomis/hs/adjust/set-level/8 ()
  (interactive)
  (nomis/hs/adjust/set-level 8))

(defun nomis/hs/adjust/set-level/9 ()
  (interactive)
  (nomis/hs/adjust/set-level 9))

(defun nomis/hs/adjust/inc-level (n)
  (setq nomis/hs/adjust/level (max 0
                                   (+ nomis/hs/adjust/level n)))
  (nomis/hs/adjust/set-level nomis/hs/adjust/level))

(defun nomis/hs/adjust/init ()
  (interactive)
  ;; Set to last-set level. Useful when working on the same form a second time.
  (nomis/hs/adjust/set-level nomis/hs/adjust/level))

(defun nomis/hs/adjust/less (n)
  (interactive "p")
  (nomis/hs/with-only-if-looking-at-bracketed-sexp-start
    (nomis/hs/adjust/inc-level (- n))))

(defun nomis/hs/adjust/more (n)
  (interactive "p")
  (nomis/hs/with-only-if-looking-at-bracketed-sexp-start
    (nomis/hs/adjust/inc-level n)))

(defun nomis/hs/adjust/set-0 ()
  (interactive)
  (nomis/hs/adjust/set-level 0))

(defun nomis/hs/adjust/set-0/and-exit ()
  ;; This exists to overcome a bug in Hydra when you have both
  ;;     :exit t
  ;; and
  ;;     :exit nil
  ;; for the same function.
  (interactive)
  (nomis/hs/adjust/set-0))

(defun nomis/hs/adjust/show-all ()
  (interactive)
  ;; This exists to overcome a bug when showing all when level shown is 1,
  ;; whereby the cursor moved weirdly and fucked things up.
  (nomis/hs/with-only-if-looking-at-bracketed-sexp-start
    (nomis/hs/hide-block)
    (nomis/hs/show-block)))

(defun nomis/hs/adjust/show-all-for-top-level ()
  (interactive)
  (save-excursion
    (nomis/beginning-of-top-level-form)
    (nomis/hs/adjust/show-all)))

(defun nomis/hs/adjust/show-all/and-exit ()
  ;; This exists to overcome a bug in Hydra when you have both
  ;;     :exit t
  ;; and
  ;;     :exit nil
  ;; for the same function.
  (interactive)
  (nomis/hs/adjust/show-all))

(define-nomis/hydra nomis/hs/adjust
  :name-as-string "Hide-show incremental"
  :key "H-q H-q"
  :vars (nomis/hs/adjust/saved-level)
  :init-form    (progn
                  (setq nomis/hs/adjust/saved-level nomis/hs/adjust/level)
                  (nomis/hs/adjust/init))
  :cancel-form (nomis/hs/adjust/set-level nomis/hs/adjust/saved-level)
  :hydra-heads
  (("-"  nomis/hs/hide-all         "Hide All")
   ("["  nomis/hs/adjust/set-0     "Hide this form")
   ("'"  nomis/hs/adjust/less      "Less")
   ("\\" nomis/hs/adjust/more      "More")
   ("]"  nomis/hs/adjust/show-all  "Show this form")
   ("p"  nomis/hs/adjust/show-all-for-top-level  "Show top-level form")
   ("="  nomis/hs/show-all         "Show All")
   ("/"  nomis/hs/toggle-hiding    "Toggle")
   ("0"  nomis/hs/adjust/set-level/0)
   ("1"  nomis/hs/adjust/set-level/1)
   ("2"  nomis/hs/adjust/set-level/2)
   ("3"  nomis/hs/adjust/set-level/3)
   ("4"  nomis/hs/adjust/set-level/4)
   ("5"  nomis/hs/adjust/set-level/5)
   ("6"  nomis/hs/adjust/set-level/6)
   ("7"  nomis/hs/adjust/set-level/7)
   ("8"  nomis/hs/adjust/set-level/8)
   ("9"  nomis/hs/adjust/set-level/9)))

;;;; ___________________________________________________________________________
;;;; nomis/hs/step-forward
;;;; nomis/hs/step-backward

(defvar nomis/hs/step-forward-position
  :before-form)

(defun nomis/hs/step-forward ()
  "Roughly: Hide the current form, then move forward a form and show it.

Details:

If we can't move forward (because point is near the end of the
buffer or because we're at or after the last subform of a form),
issue a message saying we can't move forward.

Otherwise, if point is at the beginning of an sexp, do the following:
- If the sexp is hidden, show it.
- Otherwise:
  - If the current sexp can be hidden then hide it.
  - If there is a next sexp at this level, move to its beginning
    and show it. Otherwise move to the end of the current sexp.

Otherwise, go to the beginning of the sexp after point and show it."
  (interactive)
  (cl-flet ((error--cannot-move
             ()
             (error "Can't move forward")))
    (case nomis/hs/step-forward-position
      (:before-form
       (cond ((not (nomis/can-forward-sexp?))
              (error--cannot-move))
             ((nomis/looking-at-beginning-of-sexp/kinda?)
              (if (nomis/hs/looking-at-beginning-of-hidden-sexp?)
                  (nomis/hs/adjust/show-all)
                (progn
                  (nomis/hs/adjust/set-0)
                  (forward-sexp)
                  (nomis/goto-beginning-of-sexp/or-end/forward)
                  (nomis/hs/adjust/show-all))))
             (t
              (nomis/goto-beginning-of-sexp/or-end/forward)
              (nomis/hs/adjust/show-all))))
      (:after-form
       (if (not (nomis/can-forward-sexp?))
           (cond ((not (nomis/can-backward-sexp?))
                  (error--cannot-move))
                 ((save-excursion
                    (backward-sexp)
                    (nomis/hs/looking-at-beginning-of-hidden-sexp?))
                  (error--cannot-move))
                 (t
                  (save-excursion
                    (backward-sexp)
                    (nomis/hs/adjust/set-0)
                    (unless (nomis/hs/looking-at-beginning-of-hidden-sexp?)
                      ;; Hiding had no effect.
                      (error--cannot-move)))))
         (progn
           (forward-sexp)
           (save-excursion
             (backward-sexp)
             (save-excursion
               (when (nomis/can-backward-sexp?)
                 (backward-sexp)
                 (nomis/hs/adjust/set-0)))
             (nomis/hs/adjust/show-all))))))))

(defun nomis/hs/step-backward ()
  "Roughly: Hide the current form, then move backward a form and show it.

Details:

If we can't move backward (because point is near the beginning
of the buffer or because we're at or before the first subform of
a form):
- if the current sexp is being shown and can be hidden, hide it;
- otherwise issue a message saying we can't move backward.

Otherwise, if point is at the beginning of an sexp, do the following:
- If the current sexp can be hidden then hide it.
- Move backward an sexp, and show that sexp.

Otherwise, go to the beginning of the sexp before point and show it."
  (interactive)
  (cond ((not (nomis/can-backward-sexp?))
         (cl-flet ((error--cannot-move
                    ()
                    (error "Can't move backward")))
           (if (nomis/hs/looking-at-beginning-of-hidden-sexp?)
               (error--cannot-move)
             (progn
               (nomis/hs/adjust/set-0)
               (unless (nomis/hs/looking-at-beginning-of-hidden-sexp?)
                 ;; Hiding had no effect.
                 (error--cannot-move))))))
        ((nomis/looking-at-beginning-of-sexp/kinda?)
         (nomis/hs/adjust/set-0)
         (backward-sexp)
         (nomis/hs/adjust/show-all))
        (t
         (nomis/goto-beginning-of-sexp/or-end/backward)
         (nomis/hs/adjust/show-all))))

(define-key prog-mode-map (kbd "H-]") 'nomis/hs/step-forward)
(define-key prog-mode-map (kbd "H-[") 'nomis/hs/step-backward)

;;;; ___________________________________________________________________________
;;;; nomis/hs/step-forward-and-recenter
;;;; nomis/hs/step-backward-and-recenter

(defun nomis/hs/step-forward-and-recenter ()
  (interactive)
  (nomis/with-maintain-line-no-in-window
    (nomis/hs/step-forward)))

(defun nomis/hs/step-backward-and-recenter ()
  (interactive)
  (nomis/with-maintain-line-no-in-window
    (nomis/hs/step-backward)))

(define-key prog-mode-map (kbd "H-C-]") 'nomis/hs/step-forward-and-recenter)
(define-key prog-mode-map (kbd "H-C-[") 'nomis/hs/step-backward-and-recenter)

;;;; Key chords only work for chars whose codes are in the range 32..126 -- see
;;;; limitations in `key-chord`. So you can't use the cursor keys. Annoying!
;;;; If you want key chords, maybe try these:
;;;; (key-chord-define prog-mode-map "qj" 'nomis/hs/step-forward)
;;;; (key-chord-define prog-mode-map "qk" 'nomis/hs/step-backward)

;;;; ___________________________________________________________________________

(defun nomis/turn-on-hs-minor-mode ()
  (hs-minor-mode 1))

(add-hook 'prog-mode-hook 'nomis/turn-on-hs-minor-mode)

;;;; ___________________________________________________________________________

(provide 'nomis-hide-show)
