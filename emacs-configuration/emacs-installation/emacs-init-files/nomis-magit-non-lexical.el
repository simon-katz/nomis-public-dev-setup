;;;; Init stuff -- nomis-magit-non-lexical

;;;; ___________________________________________________________________________
;;;; ---- Faces ----

(defun nomis/set-magit-faces ()
  (set-face-background 'magit-diff-context-highlight "lavender")
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
  (progn
    ;; These faces are used for the headline of the commit message, and for the
    ;; heading line of each section of diffs. Change it to something that stands
    ;; out a bit.
    (set-face-background 'magit-diff-hunk-heading "LightBlue2")
    (set-face-foreground 'magit-diff-hunk-heading "gray20")
    (set-face-background 'magit-diff-hunk-heading-highlight "CadetBlue3")
    (set-face-foreground 'magit-diff-hunk-heading-highlight "gray10")))

;;;; ___________________________________________________________________________
;;;; ---- Status sections ----

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
                          (cons (format "-n%d" magit-log-section-commit-count)
                                (--remove (string-prefix-p "-n" it)
                                          magit-buffer-log-args)))))))

(defun nomis/-magic-init-sections ()
  ;; Change what's in Magit status buffers:
  ;; - By default, if there are unpushed commits Magit status doesn't show
  ;;   pushed commits.
  ;;   That's not good.
  ;;   Fix this by replacing `magit-insert-unpushed-to-upstream-or-recent`, with
  ;;   `magit-insert-unpushed-to-upstream` and
  ;;   `nomis/magit-insert-pushed-commits`.
  (dolist (f '(magit-insert-unpushed-to-upstream
               nomis/magit-insert-pushed-commits))
    (magit-add-section-hook 'magit-status-sections-hook
                            f
                            'magit-insert-unpushed-to-upstream-or-recent
                            nil ; before
                            t))
  (remove-hook 'magit-status-sections-hook
               'magit-insert-unpushed-to-upstream-or-recent
               t))

;;;; ___________________________________________________________________________
;;;; ---- Section visibility ----

;;;; Make all sections initially expanded.

(defvar nomis/-magit-section-visibility/seens '())
(make-variable-buffer-local 'nomis/-magit-section-visibility/seens)
(put 'nomis/-magit-section-visibility/seens 'permanent-local t)

(defvar nomis/-magit-sections-to-always-expand
  '((unpulled status) ; because the visibility of this gets queried when it has no commits, and Magit decides that we don't want to expand it
    ))

(defun nomis/-magit-section-visibility (section)
  (let* ((item (magit-section-lineage section)))
    (if (member item nomis/-magit-sections-to-always-expand)
        'show
      (let* ((seen? (member item nomis/-magit-section-visibility/seens)))
        (if seen?
            nil
          (progn (push item nomis/-magit-section-visibility/seens)
                 'show))))))

(defun nomis/-magit-init-section-visibility ()
  (add-hook 'magit-section-set-visibility-hook
            'nomis/-magit-section-visibility))

;;;; ___________________________________________________________________________

(defun nomis/init-magit-mode ()
  (company-mode 0)
  (nomis/set-magit-faces)
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
  (nomis/-magic-init-sections)
  (nomis/-magit-init-section-visibility))

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
  (magit-status-setup-buffer))

(global-set-key (kbd "C-c g") 'nomis/magit/status)

;;;; ___________________________________________________________________________

(provide 'nomis-magit-non-lexical)
