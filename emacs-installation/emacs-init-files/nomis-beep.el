;;;; Beeping

;;;; ___________________________________________________________________________

(defvar *nomis/grab-user-attention/high/background*
  "IndianRed1")

;;;; ___________________________________________________________________________
;;;; Flashing

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
        (bg (or bg *nomis/grab-user-attention/high/background*))
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

(cl-defun nomis/flash (&key fg bg secs)
  (ignore-errors
    (nomis/-flash-fg-and-bg :fg fg :bg bg :secs secs)))

;;;; ___________________________________________________________________________

(defun nomis/beep ()
  (nomis/flash :bg *nomis/grab-user-attention/high/background*
               :secs 0.25))

(defun nomis/grab-user-attention/high ()
  (nomis/flash :bg *nomis/grab-user-attention/high/background*
               :secs 2))

(defun nomis/grab-user-attention/low ()
  (nomis/flash :bg "magenta"
               :secs 1))

(progn
  (defadvice y-or-n-p (before grab-user-attention)
    ;; Would be better to leave highlighting in place until answer is given.
    (nomis/grab-user-attention/high))
  (ad-activate 'y-or-n-p))

(progn
  (defadvice yes-or-no-p (before grab-user-attention)
    ;; Would be better to leave highlighting in place until answer is given.
    (nomis/grab-user-attention/high))
  (ad-activate 'yes-or-no-p))

;;;; ___________________________________________________________________________

(defvar nomis/-nail-warning/check-frequency-secs 10)
(defvar nomis/-nail-warning/n-idle-secs-for-warning 60)
(defvar nomis/-nail-warning/min-secs-between-warnings 60)

(nomis/def-timer-with-relative-repeats
    nomis/nail-warnings-timer
    0
  (let ((idle-time (current-idle-time)))
    ;; (message "idle-time = %s" idle-time)
    (if (and idle-time
             (>= (second idle-time)
                 nomis/-nail-warning/n-idle-secs-for-warning))
        (progn
          (nomis/grab-user-attention/low)
          (message "Don't pick or bite your nails!")
          `(:repeat ,nomis/-nail-warning/min-secs-between-warnings))
      `(:repeat ,nomis/-nail-warning/check-frequency-secs))))

;;;; ___________________________________________________________________________

;;;; `(setq visible-bell t)` is broken, so...

(setq visible-bell t)

(defun nomis/set-ring-bell-function ()
  (setq ring-bell-function 'nomis/beep))

(nomis/set-ring-bell-function)

;;;; ___________________________________________________________________________

(nomis/def-timer-with-fixed-repeats
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
