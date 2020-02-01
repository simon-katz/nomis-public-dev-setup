;;;; Init stuff -- Frame style.

;;;; TODO Don't repeatedly append to `default-frame-alist`.

;;;; ___________________________________________________________________________
;;;; ---- Buffer colours ----

;;;; (set-my-frame-options "light cyan")
;;;; (set-my-frame-options "gray85")
;;;; (set-my-frame-options "grey90")
;;;; (set-my-frame-options "BlanchedAlmond")
;;;; (set-my-frame-options "wheat1")
;;;; (set-my-frame-options "honeydew")
;;;; (set-my-frame-options "mint cream")
;;;; (set-my-frame-options "white smoke")
;;;; (set-my-frame-options "light blue")
;;;; (set-my-frame-options "powder blue")
;;;; (set-my-frame-options "pale turquoise")
;;;; (set-my-frame-options "LemonChiffon1")
;;;; (set-my-frame-options "MistyRose1")
;;;; (set-my-frame-options "lightyellow1")
;;;; (set-my-frame-options "cornsilk2")

(defconst -nomis/buffer-backgrounds/colour-pairs
  '(("#f5f5f5"        "grey88")
    ("BlanchedAlmond" "NavajoWhite")
    ("MistyRose"      "MistyRose2"))
  "A list of 2-tuples. At any given time, one of the 2-tuples is used for
the current set of buffer colours. The first element of each 2-tuple is the
colour for selected buffers and the second element is the colour for
unselected buffers.")

(defvar -nomis/buffer-backgrounds/current-index 0)

(defun -nomis/buffer-backgrounds/next-index ()
  (mod (1+ -nomis/buffer-backgrounds/current-index)
       (length -nomis/buffer-backgrounds/colour-pairs)))

(defun -nomis/buffer-backgrounds/current-pair ()
  (nth -nomis/buffer-backgrounds/current-index
       -nomis/buffer-backgrounds/colour-pairs))

;;;; ___________________________________________________________________________
;;;; ---- nomis/buffer-backgrounds/cycle ----
;;;; ---- nomis/buffer-backgrounds/set-alternative ----
;;;; ---- nomis/buffer-backgrounds/set-default ----

(defun set-my-frame-options (my-bgcolor)
  "Set frame color of all current and future frames.
   Useful to distinguish between two sessions."
  (setq default-frame-alist
        (append `((background-color . ,my-bgcolor))
                default-frame-alist))
  (let ((current-frame (selected-frame)))
    (mapc (lambda (f)
            (select-frame f t)
            (set-background-color my-bgcolor))
          (frame-list))
    (select-frame current-frame t)))

(defun -nomis/buffer-backgrounds/set (n)
  (setq -nomis/buffer-backgrounds/current-index n)
  (set-my-frame-options (first (-nomis/buffer-backgrounds/current-pair)))
  (nomis/grey-out-unselected-buffers))

(defun nomis/buffer-backgrounds/cycle ()
  (interactive)
  (-nomis/buffer-backgrounds/set (-nomis/buffer-backgrounds/next-index)))

(defun nomis/buffer-backgrounds/set-alternative ()
  (interactive)
  (-nomis/buffer-backgrounds/set 1))

(defun nomis/buffer-backgrounds/set-default ()
  (interactive)
  (-nomis/buffer-backgrounds/set 0))

;;;; ___________________________________________________________________________
;;;; ---- Use a different background for unselected buffers ----

;;;; I would like to do this for unselected windows (rather than buffers), but
;;;; I don't think there's a way to do that.

(defun -nomis/unselected-buffer-background ()
  (second (-nomis/buffer-backgrounds/current-pair)))

(defun nomis/grey-out-unselected-buffers ()
  ;; Copied from
  ;; https://stackoverflow.com/questions/33195122/highlight-current-active-window
  "Add a grey background to all buffers other than the current buffer."
  (walk-windows (lambda (w)
                  (unless (eq w (selected-window))
                    (with-current-buffer (window-buffer w)
                      (buffer-face-set
                       `(:background ,(-nomis/unselected-buffer-background))))))
                t
                t)
  (buffer-face-set 'default))

(add-hook 'buffer-list-update-hook 'nomis/grey-out-unselected-buffers)

;;;; ___________________________________________________________________________

(provide 'nomis-frame-style)
