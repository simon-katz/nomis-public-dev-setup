;;;; Init stuff -- Avoid window stealing.

;;;; ___________________________________________________________________________
;;;; ---- Avoiding window stealing ----

;; (setq pop-up-frames t)

;;;; From
;;;; http://stackoverflow.com/questions/5151620/how-do-i-make-this-emacs-frame-keep-its-buffer-and-not-get-resized

;; (defadvice pop-to-buffer (before cancel-other-window first)
;;   (ad-set-arg 1 nil))

;; (ad-activate 'pop-to-buffer)

;; ;; Toggle window dedication
;; (defun toggle-window-dedicated ()
;;   "Toggle whether the current active window is dedicated or not"
;;   (interactive)
;;   (message
;;    (if (let (window (get-buffer-window (current-buffer)))
;;          (set-window-dedicated-p window 
;;                                  (not (window-dedicated-p window))))
;;        "Window '%s' is dedicated"
;;      "Window '%s' is normal")
;;    (current-buffer)))

;; (setq pop-up-windows nil)

;; ;; Press [pause] key in each window you want to "freeze"
;; (global-set-key [pause] 'toggle-window-dedicated)


;;;; ___________________________________________________________________________
;;;; ---- Open buffers in current window ----

(setq ido-default-file-method 'selected-window)
(setq ido-default-buffer-method 'selected-window)
