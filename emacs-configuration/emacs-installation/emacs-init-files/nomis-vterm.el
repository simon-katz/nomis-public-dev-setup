;;; nomis-vterm.el --- vterm tailoring -*- lexical-binding: t; -*-

;;; Code

;;;; Standard vterm config

;; Emacs libvterm README says
;;   And add this to your init.el:
;;     (add-to-list 'load-path "path/to/emacs-libvterm")
;;     (require 'vterm)

(add-to-list 'load-path "/Users/simonkatz/development-100/repositories/software-installations/emacs-libvterm")

;; (require 'vterm) ; Not needed. (But see abovedocumentation.)

(with-eval-after-load 'vterm

  (setq vterm-shell "/opt/homebrew/bin/bash")
  (setq vterm-max-scrollback 100000)
  (setq vterm-clear-scrollback-when-clearing t)
  (setq vterm-copy-mode-remove-fake-newlines t)

  (define-key vterm-mode-map (kbd "H-k") 'vterm-clear)

  (unbind-key "M-`" vterm-mode-map))

;;;; Resize windows

;; I think this happens automatically, and that with shadow-cljs the width is
;; not respected.

;; (cond
;;  ((member (pkg-info-package-version 'vterm)
;;           '((20260706 1652)))
;;   (defun nomis/vterm-set-size (h w)
;;     ;; `vterm--set-size' is a C module function that updates the terminal's
;;     ;; row/column internal count.
;;     ;; (let* ((inhibit-message t))
;;     ;;   (message "Resizing vterm size for %s to %s x %s" (buffer-name) w h))
;;     (vterm--set-size vterm--term h w)))

;;  (t
;;   (defun nomis/vterm-set-size (_h _w)
;;     ;; no op
;;     )
;;   (message-box
;;    "You need to fix `nomis/vterm-set-size` for this version of vterm.")))

;; (defun nomis/vterm-window-resize-hook (frame)
;;   ;; When a frame is resized, notify each vterm terminal in that frame of its
;;   ;; new dimensions. `inhibit-read-only' is needed because vterm buffers are
;;   ;; read-only.
;;   (let* ((bufs (mapcar #'window-buffer (window-list frame))))
;;     (dolist (buf (delete-dups bufs))
;;       (when (eq (buffer-local-value 'major-mode buf) 'vterm-mode)
;;         (with-current-buffer buf
;;           (let* ((h (floor (window-screen-lines)))
;;                  (w (window-body-width))
;;                  (inhibit-read-only t))
;;             (nomis/vterm-set-size h w)))))))

;; (add-hook 'window-size-change-functions
;;           'nomis/vterm-window-resize-hook)

;;; End

(provide 'nomis-vterm)
