;;;; Init stuff -- Frame style.

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

(defun nomis/set-alternative-frame-background ()
  (interactive)
  ;; (set-my-frame-options "light cyan")
  ;; (set-my-frame-options "gray85")
  ;; (set-my-frame-options "grey90")
  (set-my-frame-options "grey92")
  ;; (set-my-frame-options "wheat1")
  ;; (set-my-frame-options "honeydew")
  ;; (set-my-frame-options "mint cream")
  ;; (set-my-frame-options "white smoke")
  ;; (set-my-frame-options "light blue")
  ;; (set-my-frame-options "powder blue")
  ;; (set-my-frame-options "pale turquoise")
  ;; (set-my-frame-options "LemonChiffon1")
  ;; (set-my-frame-options "MistyRose1")
  ;; (set-my-frame-options "lightyellow1")
  ;; (set-my-frame-options "cornsilk2")
  )

(defun nomis/set-default-frame-background ()
  (interactive)
  (set-my-frame-options "#f5f5f5"))

;;;; ___________________________________________________________________________

(provide 'nomis-frame-style)
