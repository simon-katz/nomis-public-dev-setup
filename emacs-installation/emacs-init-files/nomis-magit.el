;;;; Init stuff -- magit

;;;; ___________________________________________________________________________

(require 'nomis-magit-fixes)

(magit-auto-revert-mode 0)

(defun nomis/magit-insert-pushed-commits ()
  "Insert section showing pushed commits.
Show the last `magit-log-section-commit-count' commits."
  (when (magit-git-success "rev-parse" "@{upstream}")
    (let* ((pushed (magit-rev-parse "@{upstream}"))
           (start (format "%s~%s" pushed magit-log-section-commit-count
                          ))
           (range (and (magit-rev-verify start)
                       (concat start ".." "@{upstream}"))))
      (magit-insert-section (recent
                             range)
        (magit-insert-heading
          (format (propertize "Recently merged into %s:" 'face 'magit-section-heading)
                  (magit-get-upstream-branch)))
        (magit-insert-log range
                          (cons (format "-n%d" magit-log-section-commit-count
                                        )
                                (--remove (string-prefix-p "-n" it)
                                          magit-log-section-arguments)))))))

(defun nomis/init-magit-mode ()
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
  ;; (setq magit-push-always-verify nil) ; no longer exists
  (setq magit-diff-refine-hunk 'all)
  (setq magit-diff-highlight-trailing t)
  ;; (setq git-commit-summary-max-length 999)
  (setq magit-display-buffer-function
        'magit-display-buffer-same-window-except-diff-v1)
  (progn
    ;; The behaviour of only showing unpushed commits is annoying with my
    ;; "apply-local-formatting" stuff, so show all recent commits instead:
    (magit-add-section-hook 'magit-status-sections-hook
                            'nomis/temp-to-delete
                            'magit-insert-unpushed-to-upstream-or-recent
                            'replace)
    (magit-add-section-hook 'magit-status-sections-hook
                            'magit-insert-unpushed-to-upstream
                            'nomis/temp-to-delete
                            nil)
    (magit-add-section-hook 'magit-status-sections-hook
                            'nomis/magit-insert-pushed-commits
                            'nomis/temp-to-delete
                            'replace)))

(add-hook 'magit-mode-hook 'nomis/init-magit-mode)

(defun nomis/init-magit-status-mode ()
  ;; (visual-line-mode 1) ; -- No -- the status buffer gets confused when you expand and collapse sections
  )

(add-hook 'magit-status-mode-hook 'nomis/init-magit-status-mode)

(defun nomis/magit/status (arg)
  (interactive "P")
  (when (equal arg '(4))
    (make-frame)
    (nomis/w-double))
  (magit-status))

(global-set-key (kbd "C-c g") 'nomis/magit/status)



;; (eval-after-load 'magit
;;   '(progn
;;      (set-face-foreground 'magit-diff-added "green4")
;;      (set-face-foreground 'magit-diff-removed "orangered2")
;;      ;; This has gone: (set-face-background 'magit-item-highlight "palegoldenrod")
;;      ))

;;;; ___________________________________________________________________________

(provide 'nomis-magit)
