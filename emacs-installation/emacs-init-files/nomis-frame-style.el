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
  (nomis/buffer-backgrounds/remove-all-dimming)
  (setq -nomis/buffer-backgrounds/current-index n)
  (-> (-nomis/buffer-backgrounds/current/selected-background)
      -nomis/buffer-backgrounds/set-frame-options)
  (nomis/buffer-backgrounds/refresh))

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

(defun nomis/turn-on-auto-dim-other-buffers-mode ()
  (when (fboundp 'auto-dim-other-buffers-mode)
    (auto-dim-other-buffers-mode t)))

(add-hook 'after-init-hook
          'nomis/turn-on-auto-dim-other-buffers-mode)

(defun nomis/buffer-backgrounds/remove-all-dimming ()
  (adob--dim-all-buffers nil))

(defun nomis/buffer-backgrounds/refresh ()
  (adob--dim-all-buffers nil) ; remove all face remappings
  (adob--dim-all-buffers t)   ; add face remappings to all buffers
  (adob--undim-buffer)        ; remove face remapping from current buffer
  )

(advice-add 'adob--dim-buffer
            :around
            (lambda (orig-fun &rest args)
              (when (not adob--face-mode-remapping)
                (let* ((face-spec
                        `((:background
                           ,(-nomis/buffer-backgrounds/current/unselected-background)))))
                  (setq adob--face-mode-remapping
                        (face-remap-add-relative 'default
                                                 face-spec)))
                (force-window-update (current-buffer))))
            '((name . nomis/replace-adob--dim-buffer)))

;;;; ___________________________________________________________________________

(provide 'nomis-frame-style)
