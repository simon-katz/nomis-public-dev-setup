;;;; Init stuff -- Stuff for Lispy modes

(defun nomis-whitespace-mode ()
  (whitespace-mode)
  ;; Less-garish-that-default highlighting for > 80 (or whatever) characters.
  (set-face-attribute 'whitespace-line nil
                      :background "grey90"
                      ;; :foreground "black"
                      ;; :weight 'bold
                      )
  ;; I'm ok with trailing spaces. When they are in empty lines as part of
  ;; code or comments, I often /want/ trailing spaces.
  (set-face-attribute 'whitespace-trailing nil
                      :background (face-attribute 'default :background)
                      :foreground (face-attribute 'default :foreground)
                      ;; :underline nil
                      ))

(defun generic-lispy-stuff ()
  (rainbow-delimiters-mode)
  (nomis-whitespace-mode))

;;;; ___________________________________________________________________________

(provide 'nomis-define-lispy-modes)
