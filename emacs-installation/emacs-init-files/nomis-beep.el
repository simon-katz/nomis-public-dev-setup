;;;; Beeping

;;;; ___________________________________________________________________________
;;;; Various approaches to flashing

(defvar nomis/-flash-face 'mode-line)
;; or (defvar nomis/-flash-face 'default) -- but repeated c-G causes crashes

(defvar nomis/-flash-old-colours
  ;; Note: You need this rather than using lexical bindings to store the
  ;; old values, because if you have two flashes happening at once the
  ;; first parts and second parts can run out of sync, leading to the
  ;; first flash's colours sticking.
  '())

(cl-defun nomis/-flash-fg-and-bg (&key fg bg secs)
  (push `(,(face-foreground nomis/-flash-face)
          ,(face-background nomis/-flash-face))
        nomis/-flash-old-colours)
  (let ((fg (or fg "grey90"))
        (bg (or bg "IndianRed1"))
        (secs (or secs 0.25)))
    (set-face-foreground nomis/-flash-face fg)
    (set-face-background nomis/-flash-face bg)
    (run-at-time secs
                 nil
                 (lambda ()
                   (let* ((fg-and-bg (pop nomis/-flash-old-colours))
                          (fg (first fg-and-bg))
                          (bg (second fg-and-bg)))
                     (set-face-foreground nomis/-flash-face fg)
                     (set-face-background nomis/-flash-face bg))))))

(defun nomis/-flash-mode-line ()
  (invert-face 'mode-line)
  (run-with-timer 1 nil 'invert-face 'mode-line))

(defvar nomis/-flash-attention-grabbing-string
  (make-string 200 ?#))

(defun nomis/-flash-show-dialog ()
  (let ((last-nonmenu-event nil) ; force `y-or-n-p-with-timeout` to use a dialog
        )
    (y-or-n-p-with-timeout nomis/-flash-attention-grabbing-string 0.5 nil)))

(defun nomis/-flash-temp-message ()
  (let ((message-log-max nil))
    (with-temp-message
        nomis/-flash-attention-grabbing-string
      (sleep-for 0.5))))

(cl-defun nomis/flash (&key fg bg secs)
  (ignore-errors
    (case 0
      (0 (nomis/-flash-fg-and-bg :fg fg :bg bg :secs secs))
      (1 (nomis/-flash-mode-line))
      (2 (nomis/-flash-show-dialog))
      (3 (nomis/-flash-temp-message)))))

;;;; ___________________________________________________________________________

(defvar nomis/-nail-warning/check-frequency-secs 10)
(defvar nomis/-nail-warning/n-idle-secs-for-warning 60)
(defvar nomis/-nail-warning/min-secs-between-warnings 60)

(def-nomis/timer-with-relative-repeats
    nomis/nail-warnings-timer
    0
  (let ((idle-time (current-idle-time)))
    ;; (message "idle-time = %s" idle-time)
    (if (and idle-time
             (>= (second idle-time)
                 nomis/-nail-warning/n-idle-secs-for-warning))
        (progn
          (nomis/flash :bg "magenta" :secs 1)
          (message "Don't pick or bite your nails!")
          `(:repeat ,nomis/-nail-warning/min-secs-between-warnings))
      `(:repeat ,nomis/-nail-warning/check-frequency-secs))))

;;;; ___________________________________________________________________________

;;;; `(setq visible-bell t)` is broken, so...

(setq visible-bell t)

(defun nomis/set-ring-bell-function ()
  (setq ring-bell-function 'nomis/flash))

(nomis/set-ring-bell-function)

;;;; ___________________________________________________________________________

(def-nomis/timer-with-fixed-repeats
    nomis/ensure-ring-bell-function-not-nil
    ;; With one of your flash functions, repeated C-g can cause
    ;; `ring-bell-function` to be set to nil. Not sure when this happens.
    ;; This fixes things.
    10
    10
  (when (null ring-bell-function) ; 
    (message "`ring-bell-function` is nil. Resetting.")
    (nomis/set-ring-bell-function)))

;;;; ___________________________________________________________________________

(defun reset-fg-and-bg-if-buggered ()
  ;; useful for recovering from bugs when developing this stuff
  (set-face-foreground 'default "Black")
  (set-face-background 'default "#f5f5f5")
  (set-face-foreground 'mode-line "Black")
  (set-face-background 'mode-line "#ccccff"))

;; (reset-fg-and-bg-if-buggered)

;;;; ___________________________________________________________________________

(provide 'nomis-beep)
