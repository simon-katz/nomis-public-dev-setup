;;;; Init stuff -- nomis-text-size

;;;; ___________________________________________________________________________
;;;; ---- Globally change the font size. ----

;;;; Largely copied from http://emacs.stackexchange.com/questions/7583/transiently-adjust-text-size-in-mode-line-and-minibuffer/7584#7584 .

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

(require 'hydra)

(defvar nomis/font-resize-initial-size)

(defun nomis/font-resize-init ()
  (interactive)
  (setq nomis/font-resize-initial-size (nomis/get-default-point-size)))

(defun nomis/font-resize-cancel ()
  (interactive)
  (nomis/set-default-point-size nomis/font-resize-initial-size))

(defun nomis/font-resize-quit ()
  (interactive)
  (nomis-no-op))

(defvar nomis/font-resize-initialised-p nil)

(defmacro define-nomis/font-resize (key)
  ;; The hacking with `key` and binding it to `nomis/font-resize/body`
  ;; is to get the hints to appear when the command is first activated.
  `(progn
     (defhydra nomis/font-resize
       (global-map ,key
                   :pre (unless nomis/font-resize-initialised-p
                          (nomis/font-resize-init)
                          (setq nomis/font-resize-initialised-p t))
                   :post (setq nomis/font-resize-initialised-p nil))
       "font-resize"
       ("-"        nomis/font-size-decr     "Decrease")
       ("="        nomis/font-size-incr     "Increase")
       ("0"        nomis/font-size-reset    "Reset to default size")
       ("<escape>" nomis/font-resize-cancel "Cancel" :exit t)
       ("<return>" nomis/font-resize-quit   "Quit"   :exit t)
       ("q"        nomis/font-resize-quit   "Quit"   :exit t))
     (define-key global-map (kbd ,key) 'nomis/font-resize/body)))

(define-nomis/font-resize "M-+")

;;;; ___________________________________________________________________________

(provide 'nomis-text-size)
