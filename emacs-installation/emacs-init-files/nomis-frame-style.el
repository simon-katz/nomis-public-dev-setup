;;;; Init stuff -- Frame style.

;;;; TODO Don't repeatedly append to `default-frame-alist`.

;;;; ___________________________________________________________________________
;;;; ---- Buffer colours ----

;;;; (-nomis/buffer-backgrounds/set-frame-options "light cyan")
;;;; (-nomis/buffer-backgrounds/set-frame-options "gray85")
;;;; (-nomis/buffer-backgrounds/set-frame-options "grey90")
;;;; (-nomis/buffer-backgrounds/set-frame-options "BlanchedAlmond")
;;;; (-nomis/buffer-backgrounds/set-frame-options "wheat1")
;;;; (-nomis/buffer-backgrounds/set-frame-options "honeydew")
;;;; (-nomis/buffer-backgrounds/set-frame-options "mint cream")
;;;; (-nomis/buffer-backgrounds/set-frame-options "white smoke")
;;;; (-nomis/buffer-backgrounds/set-frame-options "light blue")
;;;; (-nomis/buffer-backgrounds/set-frame-options "powder blue")
;;;; (-nomis/buffer-backgrounds/set-frame-options "pale turquoise")
;;;; (-nomis/buffer-backgrounds/set-frame-options "LemonChiffon1")
;;;; (-nomis/buffer-backgrounds/set-frame-options "MistyRose1")
;;;; (-nomis/buffer-backgrounds/set-frame-options "lightyellow1")
;;;; (-nomis/buffer-backgrounds/set-frame-options "cornsilk2")

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

(defun -nomis/buffer-backgrounds/current/selected-background ()
  (first (-nomis/buffer-backgrounds/current-pair)))

(defun -nomis/buffer-backgrounds/current/unselected-background ()
  (second (-nomis/buffer-backgrounds/current-pair)))

;;;; ___________________________________________________________________________
;;;; ---- nomis/buffer-backgrounds/cycle ----
;;;; ---- nomis/buffer-backgrounds/set-alternative ----
;;;; ---- nomis/buffer-backgrounds/set-default ----

(defun -nomis/buffer-backgrounds/set-frame-options (my-bgcolor)
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
  (-> (-nomis/buffer-backgrounds/current/selected-background)
      -nomis/buffer-backgrounds/set-frame-options)
  (nomis/buffer-backgrounds/grey-out-unselected))

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

(defun nomis/buffer-backgrounds/grey-out-unselected ()
  ;; Copied from
  ;; https://stackoverflow.com/questions/33195122/highlight-current-active-window
  "Add a grey background to all buffers other than the current buffer."
  (walk-windows
   (lambda (w)
     (unless (eq w (selected-window))
       (with-current-buffer (window-buffer w)
         (buffer-face-set
          `(:background
            ,(-nomis/buffer-backgrounds/current/unselected-background))))))
   t
   t)
  (buffer-face-set 'default))

(add-hook 'buffer-list-update-hook 'nomis/buffer-backgrounds/grey-out-unselected)

;;;; ___________________________________________________________________________

(provide 'nomis-frame-style)
