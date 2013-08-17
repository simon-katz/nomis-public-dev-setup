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

(defun _generic-lispy-stuff-for-both-repls-and-non-repls ()
  (rainbow-delimiters-mode)
  (paredit-mode))

(defun generic-lispy-stuff-for-repls ()
  (_generic-lispy-stuff-for-both-repls-and-non-repls))

(defun generic-lispy-stuff-for-non-repls ()
  (_generic-lispy-stuff-for-both-repls-and-non-repls)
  (nomis-whitespace-mode))

;;;; ___________________________________________________________________________

(provide 'nomis-define-lispy-modes)
