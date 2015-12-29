;;;; Init stuff -- magit

(add-hook 'magit-mode-hook (lambda () (company-mode 0)))

(add-to-list 'same-window-regexps "\*magit: .*\*") ; was: (setq magit-status-buffer-switch-function 'switch-to-buffer) -- no longer works

(setq magit-completing-read-function 'magit-ido-completing-read)

;; (eval-after-load 'magit
;;   '(progn
;;      (set-face-foreground 'magit-diff-added "green4")
;;      (set-face-foreground 'magit-diff-removed "orangered2")
;;      ;; This has gone: (set-face-background 'magit-item-highlight "palegoldenrod")
;;      ))

(global-set-key (kbd "C-c g") 'magit-status)

;;;; ___________________________________________________________________________

(provide 'nomis-magit)
