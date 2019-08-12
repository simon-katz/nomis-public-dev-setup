;;;; Init stuff -- whitespace

(require 'nomis-right-margin-column)
(require 'nomis-whitespace-with-overlays-mode)

(defun error-if-not-whitespace-mode ()
  (when (not (bound-and-true-p nomis/wwo/mode))
    (error "nomis/wwo/mode is not on")))

(defun nomis/whitespace-line-over-80-on ()
  (interactive)
  (error-if-not-whitespace-mode)
  (nomis/wwo/with-refresh-when-done
    (setq nomis/wwo/beyond-margin-on? t)))

(defun nomis/whitespace-line-over-80-off ()
  (interactive)
  (error-if-not-whitespace-mode)
  (nomis/wwo/with-refresh-when-done
    (setq nomis/wwo/beyond-margin-on? nil)))

(defun nomis/whitespace-trailing-on ()
  (interactive)
  (error-if-not-whitespace-mode)
  (nomis/wwo/with-refresh-when-done
    (setq nomis/wwo/whitespace-trailing-on? t)))

(defun nomis/whitespace-trailing-off ()
  (interactive)
  (error-if-not-whitespace-mode)
  (nomis/wwo/with-refresh-when-done
    (setq nomis/wwo/whitespace-trailing-on? nil)))

(defun nomis/get-whitespace-value ()
  (error-if-not-whitespace-mode)
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

(defun nomis/set-whitespace-value (n)
  (interactive "p")
  (error-if-not-whitespace-mode)
  (message "nomis/set-whitespace-value setting approach to %s" n)
  (nomis/wwo/with-refresh-when-done
    (case n
      (0 (nomis/whitespace-line-over-80-off)
         (nomis/whitespace-trailing-off))
      (1 (nomis/whitespace-line-over-80-on)
         (nomis/whitespace-trailing-off))
      (2 (nomis/whitespace-line-over-80-off)
         (nomis/whitespace-trailing-on))
      (3 (nomis/whitespace-line-over-80-on)
         (nomis/whitespace-trailing-on)))))

(defun nomis/set-whitespace-value-0 ()
  (interactive)
  (nomis/set-whitespace-value 0))

(defun nomis/set-whitespace-value-1 ()
  (interactive)
  (nomis/set-whitespace-value 1))

(defun nomis/set-whitespace-value-2 ()
  (interactive)
  (nomis/set-whitespace-value 2))

(defun nomis/set-whitespace-value-3 ()
  (interactive)
  (nomis/set-whitespace-value 3))

(defun nomis/cycle-whitespace-value ()
  (interactive)
  (error-if-not-whitespace-mode)
  (nomis/set-whitespace-value (mod (1+ (nomis/get-whitespace-value))
                                   4)))

;;;; ___________________________________________________________________________

(defun nomis/whitespace-mode-only-trailing ()
  (nomis/wwo/mode 1)
  (nomis/whitespace-line-over-80-off))

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
