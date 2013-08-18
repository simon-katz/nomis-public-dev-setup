;;;; Init stuff -- whitespace

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

(eval-after-load 'whitespace
  '(nomis-whitespace-faces))

(defun nomis-whitespace-mode-reinstating-blatted-faces ()
  "Use this instead of `whitespace-mode'.
For some reason my whitespace face definitions get blatted, even
if this file is the last thing that gets loaded by my init."
  (whitespace-mode)
  (nomis-whitespace-faces))

(provide 'nomis-whitespace)
