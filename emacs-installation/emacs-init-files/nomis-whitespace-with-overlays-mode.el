;;;; nomis-whitespace-with-overlays-mode.el --- -*- lexical-binding: t -*-

;;;; Motivation
;;;; `hl-line-mode` blats `whitespace-mode`. So here's a whitespace thing
;;;; that uses overlays over the top of line highlighting.

;;;; ___________________________________________________________________________

(require 'cl)

;;;; ___________________________________________________________________________

(defvar-local nomis/wwo/whitespace-trailing-on? t)

(defvar-local nomis/wwo/beyond-margin-on? t)

(defface nomis/wwo/whitespace-trailing-face
  '((t :background "hotpink"))
  "Face used to visualize trailing whitespace.")

(defface nomis/wwo/beyond-margin-face
  '((t :background "pink"))
  "Face used to visualize text beyond the margin.")

(defvar nomis/wwo/whitespace-trailing-wwo-spec
  (list 'nomis/wwo/whitespace-trailing-on?
        'nomis/wwo/whitespace-trailing-face
        nil
        ;; Copied from `whitespace-trailing-regexp`.
        "\\([\t \u00A0]+\\)$"))

(defvar nomis/wwo/beyond-margin-wwo-spec
  (list 'nomis/wwo/beyond-margin-on?
        'nomis/wwo/beyond-margin-face
        2
        ;; Copied from `whitespace-color-on`, with changes
        (let ((line-column (or (ignore-errors nomis/right-margin-column)
                               (ignore-errors whitespace-line-column)
                               (ignore-errors fill-column)
                               80)))
          (format
           "^\\([^\t\n]\\{%s\\}\\|[^\t\n]\\{0,%s\\}\t\\)\\{%d\\}%s\\(.+\\)$"
           tab-width
           (1- tab-width)
           (/ line-column tab-width)
           (let ((rem (% line-column tab-width)))
             (if (zerop rem)
                 ""
               (format ".\\{%d\\}" rem)))))))

(defvar nomis/wwo/specs
  (list nomis/wwo/beyond-margin-wwo-spec
        nomis/wwo/whitespace-trailing-wwo-spec))

(defun nomis/wwo/highlight-wwo-spec (wwo-spec)
  (cl-destructuring-bind (on?-symbol face nth regexp) wwo-spec
    (when (buffer-local-value on?-symbol
                              (current-buffer))
      (hlt-highlight-regexp-region (point-min)
                                   (point-max)
                                   regexp
                                   face
                                   nil
                                   nil
                                   nth))))

(defun nomis/wwo/unhighlight-wwo-spec (wwo-spec)
  (cl-destructuring-bind (_ face _ _) wwo-spec
    (hlt-unhighlight-region (point-min)
                            (point-max)
                            face)))

(defun nomis/wwo/highlight ()
  ;; There's something a bit odd here. The "priority" of these two specs
  ;; varies.
  ;; I tried running these within `run-at-time`s of 1 second and 2 seconds,
  ;; and still it varied.
  (mapc #'nomis/wwo/highlight-wwo-spec nomis/wwo/specs))

(defun nomis/wwo/unhighlight ()
  (mapc #'nomis/wwo/unhighlight-wwo-spec nomis/wwo/specs))

(defun nomis/wwo/refresh ()
  (when nomis/wwo/mode
    (nomis/wwo/unhighlight)
    (nomis/wwo/highlight)))

(defvar nomis/wwo/idle-seconds-before-refresh 0.5
  "How often to refresh whitespace overlays.")

(defvar nomis/wwo/timer
  (progn ; TODO Add a `nomis/def-idle-timer` in `nomis-timers`
    (when (and (boundp 'nomis/wwo/timer)
               (not (null nomis/wwo/timer)))
      (ignore-errors
        (cancel-timer nomis/wwo/timer)))
    (run-with-idle-timer nomis/wwo/idle-seconds-before-refresh
                         :repeat 'nomis/wwo/refresh))
  "Timer to trigger whitespace highlighting.")

(define-minor-mode nomis/wwo/mode
  "Turn nomis/wwo/mode on or off. This is a minor mode for highlighting certain
whitespace using overlays.
Without a prefix arg, toggle nomis/wwo/mode.
With a positive prefix arg, turn on nomis/wwo/mode.
With a zero or negative prefix arg, turn off nomis/wwo/mode."
  :lighter    " wwo"
  :init-value nil
  :global     nil
  :group      'nomis/wwo
  (if nomis/wwo/mode
      (nomis/wwo/highlight)
    (nomis/wwo/unhighlight)))

(defvar *nomis/wwo/in-with-refresh-when-done?* nil)

(defun nomis/wwo/with-refresh-when-done/fun (fun)
  (let* ((refresh? (not *nomis/wwo/in-with-refresh-when-done?*))
         (*nomis/wwo/in-with-refresh-when-done?* t))
    (prog1
        (funcall fun)
      (when refresh?
        (nomis/wwo/refresh)))))

(cl-defmacro nomis/wwo/with-refresh-when-done (&body body)
  (declare (indent 0))
  `(nomis/wwo/with-refresh-when-done/fun (lambda () ,@body)))

;;;; ___________________________________________________________________________

;;;; Some lines, some long, some with whitespace at the end, some both.
;;;; Useful when testing the stuff here.
;;;; - Oh yes indeed, that is what this is for.      
;;;; - Oh yes indeed, that is what this is for. Oh yes indeed, oh yes indeed, oh yes.
;;;; - Oh yes indeed, that is what this is for. Oh yes indeed, oh yes indeed, oh yes. 
;;;; - Oh yes indeed, that is what this is for. Oh yes indeed, oh yes indeed, oh yes.   
;;;; - Oh yes indeed, that is what this is for. Oh yes indeed, oh yes indeed, oh 
;;;; - Oh yes indeed, that is what this is for. Oh yes indeed, oh yes indeed, oh  
;;;; - Oh yes indeed, that is what this is for. Oh yes indeed, oh yes indeed, oh   
;;;; - Oh yes indeed, that is what this is for. Oh yes indeed, oh yes indeed,    
;;;; - Oh yes indeed, that is what this is for. Oh yes indeed, oh yes indeed,     
;;;; - Oh yes indeed, that is what this is for. Oh yes indeed, oh yes indeed,      
;;;; - Here is a tab char -> 	 <- There.

(provide 'nomis-whitespace-with-overlays-mode)
