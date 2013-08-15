;;;; Init stuff -- Make windows and some window commands more "normal".

;;;; TODO: Rename -> "nomis-normal-frames.el".

(if (fboundp 'menu-bar-mode) (menu-bar-mode +1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode +1))

(global-unset-key "\C-z") ; default was suspend-frame

(define-key ctl-x-5-map "1" (lambda (arg) (interactive "p") (message "No, I won't close lots of windows."))) ; default was delete-other-frames

(when (fboundp 'winner-mode)
  (winner-mode 1))

;; Should do this for Windows only, I guess.
;; (define-key global-map [(meta f4)] 'delete-frame)

;; (defun other-frame-backwards ()
;;   (interactive)
;;   (other-frame -1))
(define-key global-map [(control tab)] 'next-multiframe-window)
(define-key global-map [(control shift tab)] 'previous-multiframe-window)

;;;; ___________________________________________________________________________

(provide 'nomis-normal-window-commands)
