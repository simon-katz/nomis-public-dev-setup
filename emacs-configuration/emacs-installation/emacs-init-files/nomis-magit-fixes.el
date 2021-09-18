;;;; Init stuff -- Fix problems with Magit.

;;;; ___________________________________________________________________________
;;;; ---- nomis/revert-all-unmodified-buffers-in-git-repo ----

(defun nomis/vc/buffer-in-current-repo? (b)
  (s-starts-with? (nomis/dirtree/vc-root-dir)
                  (buffer-file-name b)))

(defun nomis/revert-all-unmodified-buffers-in-git-repo (inhibit-message?)
  "Refreshes all open unmodified buffers in current buffer's Git repo
 from their files."
  (interactive "P")
  (let* ((this-buffer (current-buffer)))
    (cl-flet ((repository-buffer?
               (b)
               (with-current-buffer this-buffer
                 (case 2
                   (1 (magit-auto-revert-repository-buffer-p b))
                   (2 (nomis/vc/buffer-in-current-repo? b))))))
      (-nomis/revert-all-buffers (lambda (b)
                                   (and (not (buffer-modified-p b))
                                        (repository-buffer? b)))
                                 inhibit-message?))))

;;;; ___________________________________________________________________________
;;;; ---- -nomis/hack-magit-auto-revert ----

;;;; See https://emacs.stackexchange.com/questions/35701/magit-sets-auto-revert-mode-annoying
;;;;
;;;; Don't globally set auto-revert-mode (that's very rude!). Instead, revert
;;;; buffers in the repo after each Magit operation.

(magit-auto-revert-mode 0)

(with-eval-after-load 'magit-mode
  (cond
   ((member (magit-version) '("20210913.1931"))
    (advice-add
     'magit-auto-revert-buffers
     :around
     (lambda (_orig-fun &rest _args)
       (unless (member this-command '(magit-refresh))
         (nomis/revert-all-unmodified-buffers-in-git-repo t)))
     '((name . -nomis/hack-magit-auto-revert))))
   (t
    (message-box (s-join " "
                         '("Revisit `-nomis/hack-magit-auto-revert`"
                           "for this version of Magit."))))))

;;;; ___________________________________________________________________________
;;;; ---- nomis/hack-magit-diff ----
;;;; Fix diffs when amending a pushed commit.
;;;; See https://github.com/magit/magit/discussions/4507

(when (equal emacs-version "27.1")
  (with-eval-after-load 'magit-diff
    (cond
     ((member (magit-version)
              '("20210913.1931"))

      (defvar nomis/-magit-assume-last-was-amend? nil
        "Note that we can't use special binding to get what we
        want, because the commands `magit-commit-create` and
        `magit-commit-amend` are no longer running when the diffs
        are computed. Hence the \"assumption\".")

      (defvar nomis/-magit-diff/commit+staged?)
      (defvar *nomis/-hack-magit-diff?* t)

      (advice-add
       'magit-commit-create
       :around
       (lambda (orig-fun &rest args)
         (message "==== magit-commit-create")
         (setq nomis/-magit-assume-last-was-amend? nil)
         (apply orig-fun args))
       '((name . nomis/hack-magit-diff)))

      (advice-add
       'magit-commit-amend
       :around
       (lambda (orig-fun &rest args)
         (message "==== magit-commit-amend")
         (setq nomis/-magit-assume-last-was-amend? t)
         (apply orig-fun args))
       '((name . nomis/hack-magit-diff)))

      (advice-add
       'magit-diff-staged ; shows only the staged changes
       :around
       (lambda (orig-fun &rest args)
         (cl-flet ((do-it () (apply orig-fun args)))
           (if (and *nomis/-hack-magit-diff?*
                    nomis/-magit-assume-last-was-amend?)
               (magit-diff-while-amending)
             (progn
               (setq nomis/-magit-diff/commit+staged? nil)
               (do-it)))))
       '((name . nomis/hack-magit-diff)))

      (advice-add
       'magit-diff-while-amending ; shows commit + staged changes
       :around
       (lambda (orig-fun &rest args)
         (cl-flet ((do-it () (apply orig-fun args)))
           (when *nomis/-hack-magit-diff?*
             (setq nomis/-magit-diff/commit+staged? t))
           (do-it)))
       '((name . nomis/hack-magit-diff)))

      (defun nomis/-toggle-magit-diff (&optional args)
        (interactive (list (car (magit-diff-arguments))))
        (setq nomis/-magit-diff/commit+staged?
              (not nomis/-magit-diff/commit+staged?))
        (let* ((w (get-buffer-window))
               (*nomis/-hack-magit-diff?* nil))
          (if nomis/-magit-diff/commit+staged?
              (magit-diff-while-amending args)
            (magit-diff-staged nil args))
          (select-window w)))

      (define-key magit-diff-mode-map (kbd "C-c C-d") 'nomis/-toggle-magit-diff)
      (define-key git-commit-mode-map (kbd "C-c C-d") 'nomis/-toggle-magit-diff))

     (t
      (message-box (s-join " "
                           '("Revisit `nomis/hack-magit-diff`"
                             "for this version of Magit.")))))))

;;;; ___________________________________________________________________________
;;;; ---- nomis/hack-magit-diff ----
;;;; Fix `C-c C-d`.

(when (equal emacs-version "27.2")
  (with-eval-after-load 'magit-diff
    (cond
     ((member (magit-version)
              '("20210913.1931"))

      (defvar nomis/-magit-diff/commit+staged?)

      (advice-add
       'magit-diff-staged ; shows only the staged changes
       :around
       (lambda (orig-fun &rest args)
         (cl-flet ((do-it () (apply orig-fun args)))
           (setq nomis/-magit-diff/commit+staged? nil)
           (do-it)))
       '((name . nomis/hack-magit-diff)))

      (advice-add
       'magit-diff-while-amending ; shows commit + staged changes
       :around
       (lambda (orig-fun &rest args)
         (cl-flet ((do-it () (apply orig-fun args)))
           (setq nomis/-magit-diff/commit+staged? t)
           (do-it)))
       '((name . nomis/hack-magit-diff)))

      (defun nomis/-toggle-magit-diff (&optional args)
        (interactive (list (car (magit-diff-arguments))))
        (setq nomis/-magit-diff/commit+staged?
              (not nomis/-magit-diff/commit+staged?))
        (let* ((w (get-buffer-window)))
          (if nomis/-magit-diff/commit+staged?
              (magit-diff-while-amending args)
            (magit-diff-staged nil args))
          (select-window w)))

      (define-key magit-diff-mode-map (kbd "C-c C-d") 'nomis/-toggle-magit-diff)
      (define-key git-commit-mode-map (kbd "C-c C-d") 'nomis/-toggle-magit-diff))

     (t
      (message-box (s-join " "
                           '("Revisit `nomis/hack-magit-diff`"
                             "for this version of Magit.")))))))

;;;; ___________________________________________________________________________

(provide 'nomis-magit-fixes)
