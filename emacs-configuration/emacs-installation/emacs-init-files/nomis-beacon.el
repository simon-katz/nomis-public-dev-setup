;;;; Init stuff -- Nomis beacon tailoring --  -*- lexical-binding: t -*-

;;;; ___________________________________________________________________________

(require 'beacon)

;;;; ___________________________________________________________________________

(beacon-mode 1)
(setopt beacon-blink-when-point-moves-horizontally 10)
(setopt beacon-blink-when-point-moves-vertically 10)
(setopt beacon-blink-duration 0.5)
(setopt beacon-blink-delay 0.1)

(define-key global-map (kbd "H-b") 'beacon-blink)

(defun nomis/beacon/set-beacon-color ()
  (setopt beacon-color (if (nomis/dark-background-mode?)
                           "Yellow"
                         "Tomato")))

(add-hook 'emacs-startup-hook
          'nomis/beacon/set-beacon-color)

(add-hook 'nomis/themes/theme-changed-hook
          'nomis/beacon/set-beacon-color)

;;;; ___________________________________________________________________________

(provide 'nomis-beacon)
