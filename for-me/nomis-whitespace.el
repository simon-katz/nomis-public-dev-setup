;;;; Init stuff -- whitespace

(require 'whitespace)

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
                    )

(provide 'nomis-whitespace)
