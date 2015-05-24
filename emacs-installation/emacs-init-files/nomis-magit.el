;;;; Init stuff -- magit

(add-hook 'magit-mode-hook (lambda () (company-mode 0)))

(setq magit-status-buffer-switch-function 'switch-to-buffer)

(setq magit-completing-read-function 'magit-ido-completing-read)

;; (eval-after-load 'magit
;;   '(progn
;;      (set-face-foreground 'magit-diff-add "green4")
;;      (set-face-foreground 'magit-diff-del "orangered2")
;;      (set-face-background 'magit-item-highlight "palegoldenrod")))

(global-set-key (kbd "C-c g") 'magit-status)

;;;; ___________________________________________________________________________

;;;; From http://whattheemacsd.com/setup-magit.el-02.html

(eval-after-load 'magit
  '(progn
     
     (defun magit-toggle-whitespace ()
       (interactive)
       (if (member "-w" magit-diff-options)
           (magit-dont-ignore-whitespace)
         (magit-ignore-whitespace)))

     (defun magit-ignore-whitespace ()
       (interactive)
       (add-to-list 'magit-diff-options "-w")
       (magit-refresh))

     (defun magit-dont-ignore-whitespace ()
       (interactive)
       (setq magit-diff-options (remove "-w" magit-diff-options))
       (magit-refresh))

     (define-key magit-status-mode-map (kbd "W") 'magit-toggle-whitespace)))

;;;; ___________________________________________________________________________

(provide 'nomis-magit)
