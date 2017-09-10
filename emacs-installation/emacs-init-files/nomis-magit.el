;;;; Init stuff -- magit

;;;; ___________________________________________________________________________

(defun nomis-init-magit ()
  (company-mode 0)
  (hl-line-mode)
  (add-to-list 'same-window-regexps "\*magit: .*\*") ; was: (setq magit-status-buffer-switch-function 'switch-to-buffer) -- no longer works
  (setq magit-completing-read-function 'magit-ido-completing-read)
  ;; (setq magit-revert-buffers 'silent) obsolete
  ;; (setq magit-push-always-verify nil) ; no longer exists
  (setq magit-diff-refine-hunk 'all)
  (setq magit-diff-highlight-trailing nil)
  (setq git-commit-summary-max-length 999))


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
