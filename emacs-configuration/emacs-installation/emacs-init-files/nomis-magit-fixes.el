;;;; Init stuff -- Fix problems with Magit.  -*- lexical-binding: t; -*-

;;;; ___________________________________________________________________________
;;;; ---- nomis/hack-magit-diff ----
;;;; Fix diffs when amending a pushed commit.
;;;; See https://github.com/magit/magit/discussions/4507

(when (member emacs-version
              '("27.1"
                "28.1"))
  (with-eval-after-load 'magit-diff
    (cond
     ((member (magit-version)
              '("20210913.1931"
                "20211101.1824"))

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
              '("20210913.1931"
                "20211101.1824"))

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
                           '("You need to fix `nomis/hack-magit-diff`"
                             "for this version of Magit.")))))))

;;;; ___________________________________________________________________________

(provide 'nomis-magit-fixes)
