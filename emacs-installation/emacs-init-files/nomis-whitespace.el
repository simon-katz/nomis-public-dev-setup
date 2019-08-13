;;;; Init stuff -- whitespace

(require 'nomis-right-margin-column)
(require 'nomis-whitespace-with-overlays-mode)

(defun nomis/wwo/error-if-not-wwo-mode ()
  (when (not (bound-and-true-p nomis/wwo/mode))
    (error "nomis/wwo/mode is not on")))

(defun nomis/wwo/beyond-margin/turn-on ()
  (interactive)
  (nomis/wwo/error-if-not-wwo-mode)
  (nomis/wwo/with-refresh-when-done
    (setq nomis/wwo/beyond-margin-on? t)))

(defun nomis/wwo/beyond-margin/turn-off ()
  (interactive)
  (nomis/wwo/error-if-not-wwo-mode)
  (nomis/wwo/with-refresh-when-done
    (setq nomis/wwo/beyond-margin-on? nil)))

(defun nomis/wwo/trailing/turn-on ()
  (interactive)
  (nomis/wwo/error-if-not-wwo-mode)
  (nomis/wwo/with-refresh-when-done
    (setq nomis/wwo/whitespace-trailing-on? t)))

(defun nomis/wwo/trailing/turn-off ()
  (interactive)
  (nomis/wwo/error-if-not-wwo-mode)
  (nomis/wwo/with-refresh-when-done
    (setq nomis/wwo/whitespace-trailing-on? nil)))

(defun nomis/wwo/get-binary-encoding ()
  (nomis/wwo/error-if-not-wwo-mode)
  (cond ((and (not nomis/wwo/beyond-margin-on?)
              (not nomis/wwo/whitespace-trailing-on?))
         0)
        ((and nomis/wwo/beyond-margin-on?
              (not nomis/wwo/whitespace-trailing-on?))
         1)
        ((and (not nomis/wwo/beyond-margin-on?)
              nomis/wwo/whitespace-trailing-on?)
         2)
        (t
         3)))

(defun nomis/wwo/set-binary-encoding (n)
  (interactive "p")
  (nomis/wwo/error-if-not-wwo-mode)
  (message "nomis/wwo/set-binary-encoding: setting value to %s" n)
  (nomis/wwo/with-refresh-when-done
    (case n
      (0 (nomis/wwo/beyond-margin/turn-off)
         (nomis/wwo/trailing/turn-off))
      (1 (nomis/wwo/beyond-margin/turn-on)
         (nomis/wwo/trailing/turn-off))
      (2 (nomis/wwo/beyond-margin/turn-off)
         (nomis/wwo/trailing/turn-on))
      (3 (nomis/wwo/beyond-margin/turn-on)
         (nomis/wwo/trailing/turn-on)))))

(defun nomis/wwo/set-binary-encoding-0 ()
  (interactive)
  (nomis/wwo/set-binary-encoding 0))

(defun nomis/wwo/set-binary-encoding-1 ()
  (interactive)
  (nomis/wwo/set-binary-encoding 1))

(defun nomis/wwo/set-binary-encoding-2 ()
  (interactive)
  (nomis/wwo/set-binary-encoding 2))

(defun nomis/wwo/set-binary-encoding-3 ()
  (interactive)
  (nomis/wwo/set-binary-encoding 3))

(defun nomis/wwo/cycle-binary-encoding ()
  (interactive)
  (nomis/wwo/error-if-not-wwo-mode)
  (nomis/wwo/set-binary-encoding (mod (1+ (nomis/wwo/get-binary-encoding))
                                      4)))

;;;; ___________________________________________________________________________

(defun nomis/whitespace-mode-only-trailing ()
  (nomis/wwo/mode 1)
  (nomis/wwo/beyond-margin/turn-off))

(add-hook 'text-mode-hook 'nomis/wwo/mode)
(add-hook 'prog-mode-hook 'nomis/wwo/mode)
(add-hook 'org-mode-hook  'nomis/whitespace-mode-only-trailing)

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

(provide 'nomis-whitespace)
