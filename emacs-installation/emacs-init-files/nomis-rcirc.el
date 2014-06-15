;;;; Init stuff -- rcirc

(eval-after-load 'rcirc '(require 'rcirc-notify))

(rcirc-notify-add-hooks)

(setq rcirc-notify-timeout -1)

(defun rcirc-notify-allowed (nick &optional delay)
  ;; TODO: This is a gross hack.
  ;;       But, actually, I think it gives me what I want, and I think
  ;;       the tailoring options don't allow for this.
  t)

;; (add-hook 'rcirc-mode-hook
;;           (lambda ()
;;             (rcirc-track-minor-mode 1)))

(add-hook 'rcirc-mode-hook
          (lambda ()
            (flyspell-mode 1)))

(provide 'nomis-rcirc)
