;;;; Init stuff -- magit

;;;; ___________________________________________________________________________

(defun nomis-init-magit ()
  (company-mode 0)
  (set-face-background 'magit-section-highlight
                       ;; default is "grey95", which is pretty much the same as
                       ;; my default background colour ("#f5f5f5")
                       (case 5
                         (1 "palegoldenrod")
                         (2 "skyblue")
                         (3 "lightblue")
                         (4 "lightcyan")
                         (5 "lavender")
                         (6 "white")))
  (setq magit-log-margin
        ;; (INIT STYLE WIDTH AUTHOR AUTHOR-WIDTH)
        '(t "%Y-%m-%d %H:%M:%S" magit-log-margin-width t 18)) 
  (add-to-list 'same-window-regexps "\*magit: .*\*") ; was: (setq magit-status-buffer-switch-function 'switch-to-buffer) -- no longer works
  (setq magit-completing-read-function 'magit-ido-completing-read)
  (progn ; Old auto-revert stuff
    ;; (setq magit-revert-buffers 'silent) obsolete
    )
  (progn ; New auto-revert stuff
    ;; Magit now sets up auto-revert-mode on buffers it knows about (or something).
    ;; Sheesh!
    ;; See https://emacs.stackexchange.com/questions/35701/magit-sets-auto-revert-mode-annoying
    (with-eval-after-load 'magit-autorevert
      (when (equal magit-version "2.10.3") ; FIXME Move this to a "fixes" file
        ;; I want reverting only when I do a Magit operation, but that has been
        ;; broken. Better to have auto-revert completely off.
        (magit-auto-revert-mode 0)
        ;; Maybe good to revert buffers when Magit makes changes:
        (defalias 'magit-auto-revert-buffers 'revert-all-unmodified-buffers))))
  ;; (setq magit-push-always-verify nil) ; no longer exists
  (setq magit-diff-refine-hunk 'all)
  (setq magit-diff-highlight-trailing nil)
  ;; (setq git-commit-summary-max-length 999)
  )

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
