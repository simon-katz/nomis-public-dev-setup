;;;; Init stuff -- magit

(defun nomis-init-magit ()

  (company-mode 0)

  (add-to-list 'same-window-regexps "\*magit: .*\*") ; was: (setq magit-status-buffer-switch-function 'switch-to-buffer) -- no longer works

  (setq magit-completing-read-function 'magit-ido-completing-read)

  (setq magit-revert-buffers nil)
  (setq magit-push-always-verify nil)
  (setq magit-diff-refine-hunk t))


(add-hook 'magit-mode-hook 'nomis-init-magit)

(global-set-key (kbd "C-c g") 'magit-status)



;; (eval-after-load 'magit
;;   '(progn
;;      (set-face-foreground 'magit-diff-added "green4")
;;      (set-face-foreground 'magit-diff-removed "orangered2")
;;      ;; This has gone: (set-face-background 'magit-item-highlight "palegoldenrod")
;;      ))

;;;; ___________________________________________________________________________

(provide 'nomis-magit)
