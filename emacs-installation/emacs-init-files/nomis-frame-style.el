;;;; Init stuff -- Frame style.

;;;; ___________________________________________________________________________
;;;; ---- nomis/set-alternative-frame-background ----
;;;; ---- nomis/set-default-frame-background ----

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

(defvar *nomis/using-alternative-frame-background?* nil)

(defun nomis/set-alternative-frame-background ()
  (interactive)
  ;; (set-my-frame-options "light cyan")
  ;; (set-my-frame-options "gray85")
  ;; (set-my-frame-options "grey90")
  ;; (set-my-frame-options "BlanchedAlmond")
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
  (setq *nomis/using-alternative-frame-background?* t)
  (set-my-frame-options "BlanchedAlmond")
  (when (functionp 'nomis/grey-out-unselected-buffers)
    (nomis/grey-out-unselected-buffers)))

(defun nomis/set-default-frame-background ()
  (interactive)
  (setq *nomis/using-alternative-frame-background?* nil)
  (set-my-frame-options "#f5f5f5")
  (when (functionp 'nomis/grey-out-unselected-buffers)
    (nomis/grey-out-unselected-buffers)))

;;;; ___________________________________________________________________________
;;;; ---- Use a different background for unselected buffers ----

;;;; I would like to do this for unselected windows (rather than buffers), but
;;;; I don't think there's a way to do that.

(defun -nomis/unselected-buffer-background ()
  (if *nomis/using-alternative-frame-background?*
      "NavajoWhite"
   (case 2
     (1 "grey91")
     (2 "grey88"))))

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
