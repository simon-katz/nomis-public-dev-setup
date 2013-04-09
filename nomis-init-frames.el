;;;; Init stuff -- Frame size.

;;;; ___________________________________________________________________________

(defvar single-window-frame-width 85)
(defvar double-window-frame-width 180)

;;;; ___________________________________________________________________________

(let ((window-height (cond ((string-equal (system-name) "CHIVERS")
                            ;; 1200 pixels
                            72)
                           ((string-equal (system-name) "GILZEAN2")
                            ;; 1050 pixels
                            62)
                           ((string-equal (system-name) "JENNINGS")
                            ;; 800 pixels
                            47)
                           ((string-equal (system-name) "Simon-Katzs-MacBook-Pro.local")
                            ;; 1050 pixels - menu bar
                            66)
                           ((member (system-name) ; this keeps changing --why? Ah! At the time of writing it's "188.28.48.230.threembb.co.uk", which mentions "three" and I'm on my data connection with 3connect
                                    (list "unknown-70-56-81-a2-7a-0f.home"
                                          "Perryman.local"
                                          "perryman.home"))
                            ;; 900 pixels - menu bar
                            56)
                           (t
                            66))))
  (defparameter nomis-frame-prefs `((width  . ,single-window-frame-width)
                                    (height . ,window-height)
                                    (top . 0)
                                    (left . 140)
                                    ;; (font . "4.System VIO")
                                    ;; (foreground-color . "Black")
                                    (background-color . "#f5f5f5")
                                    ;;(cursor-color . "SkyBlue")
                                    ))
  (setq initial-frame-alist (append nomis-frame-prefs initial-frame-alist))
  (setq default-frame-alist (append nomis-frame-prefs default-frame-alist)))

;;;; ___________________________________________________________________________
;;;; ---- Commands to adjust frames ----

(progn
  (defun nomis-set-frame-width (arg)
    (interactive "p")
    (if (= arg 1) (setq arg double-window-frame-width))
    (set-frame-width (selected-frame) arg))

  (defun nomis-w-single (arg)
    (interactive "p")
    (nomis-set-frame-width single-window-frame-width))

  (defun nomis-w-double (arg)
    (interactive "p")
    (nomis-set-frame-width double-window-frame-width))

  (defun nomis-set-frame-height (arg)
    (interactive "p")
    (set-frame-height (selected-frame) arg))

  (defun nomis-h62 (arg)
    (interactive "p")
    (nomis-set-frame-height 62))

  (defun nomis-h29 (arg)
    (interactive "p")
    (nomis-set-frame-height 29)))

(defun nomis-maximize-frame-height (&optional frame)
  "Maximize the selected frame in the vertical direction."
  (interactive)
  (when (null frame)
    (setq frame (selected-frame)))
  (let* ((pixels-per-row (/ (float (frame-pixel-height frame))
                            (frame-height frame)))
         (n-rows (- (floor (/ (x-display-pixel-height frame)
                              pixels-per-row))
                    3)))
    (set-frame-size frame
                    (frame-width frame)
                    n-rows))
  (set-frame-position frame (frame-parameter frame 'left)
                      1) ; 1 to allow pointing at something underneath
                                        ; the frame
  )

(defun nomis-maximize-all-frame-heights ()
  (interactive)
  (mapc 'nomis-maximize-frame-height
        (frame-list)))
