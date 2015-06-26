;;;; Init stuff -- nomis-text-size

;;;; ___________________________________________________________________________
;;;; ---- Globally change the font size. ----

;;;; Largely copied from http://emacs.stackexchange.com/questions/7583/transiently-adjust-text-size-in-mode-line-and-minibuffer/7584#7584 .

(defvar nomis/initial-point-size (face-attribute 'default :height))

(defvar nomis/point-size nomis/initial-point-size)

(defun nomis/font-size-adjust (&optional arg)
  "Globally change the font size.
M-<NUM> M-x nomis/font-size-adjust:
- increases font size by (10% ^ NUM) if NUM is +ve
- decreases font size by (10% ^ NUM) if NUM is -ve
- resets    font size if NUM is 0."
  (interactive "p")
  (setq nomis/point-size
        (if (= arg 0)
            nomis/initial-point-size
          (round (* nomis/point-size
                    (expt 1.1 arg)))))
  (set-face-attribute 'default nil :height nomis/point-size))

(defun nomis/font-size-incr  () (interactive) (nomis/font-size-adjust +1)) 
(defun nomis/font-size-decr  () (interactive) (nomis/font-size-adjust -1)) 
(defun nomis/font-size-reset () (interactive) (nomis/font-size-adjust  0))

(require 'hydra)

(defhydra hydra-font-resize
  (global-map "M-+")
  "font-resize"
  ("-"   nomis/font-size-decr  "Decrease")
  ("="   nomis/font-size-incr  "Increase")
  ("0"   nomis/font-size-reset "Reset to default size"))

;;;; ___________________________________________________________________________

(provide 'nomis-text-size)
