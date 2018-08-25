;;;; Init stuff -- whitespace

(require 'whitespace)
(require 'nomis-right-margin-column)

(setq whitespace-line-column nomis/right-margin-column)

(setq whitespace-style '(face trailing lines-tail tabs))

(progn
  ;; Set these to do nothing; we will turn things on and off ourselves.
  (set-face-attribute 'whitespace-line nil
                      :background 'unspecified
                      :foreground 'unspecified)
  (set-face-attribute 'whitespace-trailing nil
                      :box nil
                      :background 'unspecified
                      :foreground 'unspecified))

(defvar nomis/whitespace-line-over-80-cookie)
(make-variable-buffer-local 'nomis/whitespace-line-over-80-cookie) ; Buffer local in all buffers 

(defvar nomis/whitespace-trailing-cookie)
(make-variable-buffer-local 'nomis/whitespace-trailing-cookie) ; Buffer local in all buffers.

(defun error-if-not-whitespace-mode ()
  (when (not whitespace-mode)
    (error "whitespace-mode is not on")))

(defun nomis/whitespace-line-over-80-on ()
  (interactive)
  (error-if-not-whitespace-mode)
  (unless (bound-and-true-p nomis/whitespace-line-over-80-cookie)
    (setq nomis/whitespace-line-over-80-cookie
          (face-remap-add-relative 'whitespace-line
                                   (list (list :background "pink"
                                               :foreground 'unspecified))))))

(defun nomis/whitespace-line-over-80-off ()
  (interactive)
  (error-if-not-whitespace-mode)
  (when nomis/whitespace-line-over-80-cookie
    (face-remap-remove-relative nomis/whitespace-line-over-80-cookie)
    (setq nomis/whitespace-line-over-80-cookie nil)))

(defun nomis/whitespace-trailing-on ()
  (interactive)
  (error-if-not-whitespace-mode)
  (unless (bound-and-true-p nomis/whitespace-trailing-cookie)
    (setq nomis/whitespace-trailing-cookie
          (face-remap-add-relative 'whitespace-trailing
                                   (list (list :box (list :line-width -10
                                                          :color "hotpink"
                                                          :style nil)))))))

(defun nomis/whitespace-trailing-off ()
  (interactive)
  (error-if-not-whitespace-mode)
  (when nomis/whitespace-trailing-cookie
    (face-remap-remove-relative nomis/whitespace-trailing-cookie)
    (setq nomis/whitespace-trailing-cookie nil)))

(defun nomis/get-whitespace-value ()
  (error-if-not-whitespace-mode)
  (cond ((and (not nomis/whitespace-line-over-80-cookie)
              (not nomis/whitespace-trailing-cookie))
         0)
        ((and nomis/whitespace-line-over-80-cookie
              (not nomis/whitespace-trailing-cookie))
         1)
        ((and (not nomis/whitespace-line-over-80-cookie)
              nomis/whitespace-trailing-cookie)
         2)
        (t
         3)))

(defun nomis/set-whitespace-value (n)
  (interactive "p")
  (error-if-not-whitespace-mode)
  (message "nomis/set-whitespace-value setting approach to %s" n)
  (case n
    (0 (nomis/whitespace-line-over-80-off)
       (nomis/whitespace-trailing-off))
    (1 (nomis/whitespace-line-over-80-on)
       (nomis/whitespace-trailing-off))
    (2 (nomis/whitespace-line-over-80-off)
       (nomis/whitespace-trailing-on))
    (3 (nomis/whitespace-line-over-80-on)
       (nomis/whitespace-trailing-on))))

(defun nomis/cycle-whitespace-value ()
  (interactive)
  (error-if-not-whitespace-mode)
  (nomis/set-whitespace-value (mod (1+ (nomis/get-whitespace-value))
                                   4)))

(defun nomis/whitespace-faces ()
  ;; Less-garish-than-default highlighting for > 80 (or whatever)
  ;; characters.
  (nomis/whitespace-line-over-80-on)
  (nomis/whitespace-trailing-on))

(progn
  ;; For some reason my whitespace face definitions get blatted, even
  ;; if this file is the last thing that gets loaded by my init.
  (defadvice whitespace-mode (after nomis/whitespace-faces (&rest args))
    (nomis/whitespace-faces))
  (ad-activate 'whitespace-mode))

;;;; ___________________________________________________________________________

(defun nomis/whitespace-mode-off ()
  (whitespace-mode 0))

(add-hook 'text-mode-hook 'whitespace-mode)
(add-hook 'prog-mode-hook 'whitespace-mode)
(add-hook 'org-mode-hook  'nomis/whitespace-mode-off)

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
