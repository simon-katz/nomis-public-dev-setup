;;;; Init stuff -- magit

;;;; ___________________________________________________________________________

(defun nomis/magit-display-buffer-function (buffer)
  ;; Have Magit status buffer open in same window
  ;; See https://github.com/magit/magit/issues/2541
  (display-buffer buffer
                  (if (and (derived-mode-p 'magit-mode)
                           (memq (with-current-buffer buffer major-mode)
                                 '(magit-process-mode
                                   magit-revision-mode
                                   magit-diff-mode
                                   magit-stash-mode
                                   magit-status-mode)))
                      nil
                    '(display-buffer-same-window))))

(defun nomis-init-magit ()

  ;; FIXME: Much of this is no longer A Thing.

  (company-mode 0)
  (hl-line-mode)

  (add-to-list 'same-window-regexps "\*magit: .*\*") ; was: (setq magit-status-buffer-switch-function 'switch-to-buffer) -- no longer works

  (setq magit-completing-read-function 'magit-ido-completing-read)

  (setq magit-revert-buffers 'silent)
  (setq magit-push-always-verify nil)
  (setq magit-diff-refine-hunk 'all)
  (setq magit-diff-highlight-trailing nil)
  (setq git-commit-summary-max-length 999)

  (when (equal magit-version "2.10.3")
    (setq magit-display-buffer-function
          'nomis/magit-display-buffer-function)))


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
