;;;; Init stuff -- Nomis beacon tailoring --  -*- lexical-binding: t -*-

;;;; ___________________________________________________________________________

(require 'beacon)

;;;; ___________________________________________________________________________

(beacon-mode 1)
(setq beacon-blink-when-point-moves-horizontally 10)
(setq beacon-blink-when-point-moves-vertically 10)
(setq beacon-blink-duration 0.5)
(setq beacon-blink-delay 0.1)

(define-key global-map (kbd "H-b") 'beacon-blink)

(defun nomis/beacon/set-beacon-color ()
  (setq beacon-color (if (nomis/dark-background-mode?)
                         "Yellow"
                       "Tomato")))

(add-hook 'emacs-startup-hook
          'nomis/beacon/set-beacon-color)

(add-hook 'nomis/themes/theme-changed-hook
          'nomis/beacon/set-beacon-color)

;;;; ___________________________________________________________________________

(provide 'nomis-beacon)
