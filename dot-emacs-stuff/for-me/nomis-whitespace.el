;;;; Init stuff -- whitespace

(require 'whitespace)

(setq whitespace-line-column nomis-right-margin)

(setq whitespace-style '(face trailing lines-tail tabs))

(defun nomis-whitespace-faces ()
  ;; Less-garish-than-default highlighting for > 80 (or whatever)
  ;; characters.
  (set-face-attribute 'whitespace-line nil
                      :background "grey90"
                      ;; :foreground "black"
                      ;; :weight 'bold
                      )
  ;; I'm ok with trailing spaces. When they are in empty lines as part
  ;; of code or comments, I often /want/ trailing spaces.
  (set-face-attribute 'whitespace-trailing nil
                      :background (face-attribute 'default :background)
                      :foreground (face-attribute 'default :foreground)
                      ;; :underline nil
                      ))

(progn
  ;; For some reason my whitespace face definitions get blatted, even
  ;; if this file is the last thing that gets loaded by my init.
  (defadvice whitespace-mode (after nomis-whitespace-faces (&rest args))
    (nomis-whitespace-faces))
  (ad-activate 'whitespace-mode))

(defun whitespace-mode-off () (whitespace-mode -1))

(provide 'nomis-whitespace)
