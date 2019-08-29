;;;; Init stuff -- nomis-msg

;;;; ___________________________________________________________________________

(require 'nomis-timers)

;;;; ___________________________________________________________________________

(defvar *nomis/msg/grab-user-attention/high/background*
  "IndianRed1")

(defvar *nomis/msg/grab-user-attention/low/background*
  "magenta")

(defvar *nomis/msg/grab-user-attention/input-required/background*
  "SeaGreen")

;;;; ___________________________________________________________________________
;;;; Flashing

(defvar nomis/msg/-flash-face 'mode-line)
;; or (defvar nomis/msg/-flash-face 'default) -- but repeated c-G causes crashes

(defvar nomis/-flash-old-colours
  ;; Note: You need this rather than using lexical bindings to store the
  ;; old values, because if you have two flashes happening at once the
  ;; first parts and second parts can run out of sync, leading to the
  ;; first flash's colours sticking.
  '())

(defun nomis/msg/-set-fg-and-bg (fg bg)
  (push `(,(face-foreground nomis/msg/-flash-face)
          ,(face-background nomis/msg/-flash-face))
        nomis/-flash-old-colours)
  (let ((fg (or fg "grey90"))
        (bg (or bg *nomis/msg/grab-user-attention/high/background*)))
    (set-face-foreground nomis/msg/-flash-face fg)
    (set-face-background nomis/msg/-flash-face bg)))

(defun nomis/msg/-clear-fg-and-bg ()
  (let* ((fg-and-bg (pop nomis/-flash-old-colours))
         (fg (first fg-and-bg))
         (bg (second fg-and-bg)))
    (set-face-foreground nomis/msg/-flash-face fg)
    (set-face-background nomis/msg/-flash-face bg)))

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(cl-defun nomis/msg/-flash-fg-and-bg (&key fg bg secs)
  (nomis/msg/-set-fg-and-bg fg bg)
  (let ((secs (or secs 0.25)))
    (run-at-time secs
                 nil
                 'nomis/msg/-clear-fg-and-bg)))

(cl-defun nomis/msg/flash (&key fg bg secs)
  (ignore-errors
    (nomis/msg/-flash-fg-and-bg :fg fg :bg bg :secs secs)))

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defun nomis/msg/-with-fg-and-bg (fg bg f)
  (nomis/msg/-set-fg-and-bg fg bg)
  (unwind-protect (funcall f)
    (nomis/msg/-clear-fg-and-bg)))

(cl-defmacro nomis/msg/with-fg-and-bg ((&rest options) &body body)
  (declare (indent 1))
  (mmt-once-only (options)
    `(nomis/msg/-with-fg-and-bg (plist-get ,options :fg)
                                (plist-get ,options :bg)
                                #'(lambda () ,@body))))

;;;; ___________________________________________________________________________

(defun nomis/msg/beep (&optional secs)
  (let* ((secs (or secs 0.25)))
    (nomis/msg/flash :bg *nomis/msg/grab-user-attention/high/background*
                     :secs secs)))

(defun nomis/msg/grab-user-attention/high (&optional secs)
  (let* ((secs (or secs 2)))
    (nomis/msg/flash :bg *nomis/msg/grab-user-attention/high/background*
                     :secs secs)))

(defun nomis/msg/grab-user-attention/low (&optional secs)
  (let* ((secs (or secs 1)))
    (nomis/msg/flash :bg *nomis/msg/grab-user-attention/low/background*
                     :secs secs)))

(defun nomis/msg/grab-user-attention/input-required (&optional secs)
  (let* ((secs (or secs 1)))
    (nomis/msg/flash :bg *nomis/msg/grab-user-attention/input-required/background*
                     :secs secs)))

(cl-defmacro nomis/msg/with-grab-user-attention/high (() &body body)
  (declare (indent 1))
  `(nomis/msg/with-fg-and-bg
       (list :bg *nomis/msg/grab-user-attention/high/background*)
     ,@body))

(cl-defmacro nomis/msg/with-grab-user-attention/low (() &body body)
  (declare (indent 1))
  `(nomis/msg/with-fg-and-bg
       (list :bg *nomis/msg/grab-user-attention/low/background*)
     ,@body))

(cl-defmacro nomis/msg/with-grab-user-attention/input-required (() &body body)
  (declare (indent 1))
  `(nomis/msg/with-fg-and-bg
       (list :bg *nomis/msg/grab-user-attention/input-required/background*)
     ,@body))


(progn
  (defadvice y-or-n-p (around y-or-n-p/grab-user-attention
                              (&rest args-to-ignore))
    (nomis/msg/with-grab-user-attention/input-required ()
      ad-do-it))
  (ad-activate 'y-or-n-p))

;; (nomis/msg/with-fg-and-bg (list :fg "green" :bg "yellow") (message-box "Hello"))
;; (nomis/msg/beep)
;; (nomis/msg/grab-user-attention/high)
;; (nomis/msg/grab-user-attention/low)
;; (nomis/msg/grab-user-attention/input-required)
;; (nomis/msg/with-grab-user-attention/high () (message-box "hello"))
;; (nomis/msg/with-grab-user-attention/low () (message-box "hello"))
;; (nomis/msg/with-grab-user-attention/input-required () (message-box "hello"))
;; (y-or-n-p "cccccccccccccc")
;; (yes-or-no-p "bbbbbbbbbbb")

;;;; ___________________________________________________________________________

(defvar nomis/msg/-nail-warning/check-frequency-secs 10)
(defvar nomis/msg/-nail-warning/n-idle-secs-for-warning 60)
(defvar nomis/msg/-nail-warning/min-secs-between-warnings 60)

(nomis/def-timer-with-relative-repeats
    nomis/msg/nail-warnings-timer
    0
  (let ((idle-time (current-idle-time)))
    ;; (message "idle-time = %s" idle-time)
    (if (and idle-time
             (>= (second idle-time)
                 nomis/msg/-nail-warning/n-idle-secs-for-warning))
        (progn
          (nomis/msg/grab-user-attention/low)
          (unless (active-minibuffer-window)
            (message "Don't pick or bite your nails!"))
          `(:repeat ,nomis/msg/-nail-warning/min-secs-between-warnings))
      `(:repeat ,nomis/msg/-nail-warning/check-frequency-secs))))

;;;; ___________________________________________________________________________

;;;; `(setq visible-bell t)` is broken, so...

(setq visible-bell t)

(defun nomis/msg/set-ring-bell-function ()
  (setq ring-bell-function 'nomis/msg/beep))

(nomis/msg/set-ring-bell-function)

;;;; ___________________________________________________________________________

(nomis/def-timer-with-fixed-repeats
    nomis/msg/ensure-ring-bell-function-not-nil
    ;; With one of your flash functions, repeated C-g can cause
    ;; `ring-bell-function` to be set to nil. Not sure when this happens.
    ;; This fixes things.
    10
    10
  (when (null ring-bell-function) ;
    (message "`ring-bell-function` is nil. Resetting.")
    (nomis/msg/set-ring-bell-function)))

;;;; ___________________________________________________________________________

(defun nomis/msg/reset-fg-and-bg-if-buggered ()
  ;; useful for recovering from bugs when developing this stuff
  ;; (set-face-foreground 'default "Black")
  ;; (set-face-background 'default "#f5f5f5")
  (nomis/set-mode-line-fgs-and-bgs))

;; (nomis/msg/reset-fg-and-bg-if-buggered)

;;;; ___________________________________________________________________________

(provide 'nomis-msg)
