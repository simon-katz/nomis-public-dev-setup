;;;; Init stuff -- nomis-text-size

;;;; ___________________________________________________________________________

;;;; Pretty-much copied from http://emacs.stackexchange.com/questions/7583/transiently-adjust-text-size-in-mode-line-and-minibuffer/7584#7584

(defvar default-font-size-pt 12)

(defvar font-size-pt default-font-size-pt)

(defun modi/font-size-adj (&optional arg)
  "The default C-x C-0/-/= bindings do an excellent job of font resizing.
They, though, do not change the font sizes for the text outside the buffer,
example in mode-line. Below function changes the font size in those areas too.

M-<NUM> M-x modi/font-size-adj increases font size by NUM points if NUM is +ve,
                               decreases font size by NUM points if NUM is -ve
                               resets    font size if NUM is 0."
  (interactive "p")
  (if (= arg 0)
      (setq font-size-pt default-font-size-pt)
    (setq font-size-pt (+ font-size-pt arg)))
  ;; The internal font size value is 10x the font size in points unit.
  ;; So a 10pt font size is equal to 100 in internal font size value.
  (set-face-attribute 'default nil :height (* font-size-pt 10)))

(defun modi/font-size-incr ()  (interactive) (modi/font-size-adj +1))
(defun modi/font-size-decr ()  (interactive) (modi/font-size-adj -1))
(defun modi/font-size-reset () (interactive) (modi/font-size-adj 0))

(require 'hydra)

(defhydra hydra-font-resize
  (global-map "C-M-=")
  "font-resize"
  ("-"   modi/font-size-decr  "Decrease")
  ("="   modi/font-size-incr  "Increase")
  ("0"   modi/font-size-reset "Reset to default size"))

;;;; ___________________________________________________________________________

(provide 'nomis-text-size)
