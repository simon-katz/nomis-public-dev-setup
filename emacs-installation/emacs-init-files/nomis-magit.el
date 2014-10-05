;;;; Init stuff -- magit

(add-hook 'magit-mode-hook (lambda () (company-mode 0)))

(setq magit-status-buffer-switch-function 'switch-to-buffer)

(setq magit-completing-read-function 'magit-ido-completing-read)

(eval-after-load 'magit
  '(progn
     (set-face-foreground 'magit-diff-add "green3")
     (set-face-foreground 'magit-diff-del "red3")
     (unless nil ;window-system
       (set-face-background 'magit-item-highlight "palegoldenrod"))))
(global-set-key (kbd "C-c g") 'magit-status)

;;;; ___________________________________________________________________________

(provide 'nomis-magit)
