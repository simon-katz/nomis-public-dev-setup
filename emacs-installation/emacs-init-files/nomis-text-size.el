;;;; Init stuff -- nomis-text-size

;;;; ___________________________________________________________________________
;;;; ---- Globally change the font size. ----

;;;; Underlying stuff copied from http://emacs.stackexchange.com/questions/7583/transiently-adjust-text-size-in-mode-line-and-minibuffer/7584#7584 .

(defun nomis/get-default-point-size ()
  (face-attribute 'default :height))

(defun nomis/set-default-point-size (size)
  (set-face-attribute 'default nil :height size))

(defvar nomis/default-default-point-size (nomis/get-default-point-size))

(defun nomis/font-size-adjust (&optional arg)
  "Globally change the font size.
M-<NUM> M-x nomis/font-size-adjust:
- increases font size by (10% ^ NUM) if NUM is +ve
- decreases font size by (10% ^ NUM) if NUM is -ve
- resets    font size if NUM is 0."
  (interactive "p")
  (let ((new-size (if (= arg 0)
                      nomis/default-default-point-size
                    (round (* (nomis/get-default-point-size)
                              (expt 1.1 arg))))))
    (nomis/set-default-point-size new-size)))

(defun nomis/font-size-incr  () (interactive) (nomis/font-size-adjust +1)) 
(defun nomis/font-size-decr  () (interactive) (nomis/font-size-adjust -1)) 
(defun nomis/font-size-reset () (interactive) (nomis/font-size-adjust  0))

(defvar nomis/resize-font/initial-size)

(define-nomis-hydra nomis/resize-font
  :name-as-string "Resize font"
  :key "M-+"
  :init-form (setq nomis/resize-font/initial-size
                   (nomis/get-default-point-size))
  :cancel-form (nomis/set-default-point-size nomis/resize-font/initial-size)
  :hydra-heads (("-" nomis/font-size-decr  "Decrease")
                ("=" nomis/font-size-incr  "Increase")
                ("0" nomis/font-size-reset "Reset to default size" :exit t)))

;;;; ___________________________________________________________________________

(provide 'nomis-text-size)
