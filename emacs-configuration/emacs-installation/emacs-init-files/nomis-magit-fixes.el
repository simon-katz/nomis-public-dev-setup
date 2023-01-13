;;;; Init stuff -- Fix problems with Magit.  -*- lexical-binding: t; -*-

;;;; +-------------------------------------------------------------------------+
;;;; | I'm confused.                                                           |
;;;; |                                                                         |
;;;; | There are two Hacks below one for Emacs 27.1 and 28.1, and              |
;;;; | another for Emacs 27.2. (And each of these covers the same Magit        |
;;;; | versions.) There's some overlap between the two Hacks.                  |
;;;; |                                                                         |
;;;; | So we have:                                                             |
;;;; | - Hack A for Emacs 27.1.                                                |
;;;; | - Hack B for Emacs 27.2.                                                |
;;;; | - Hack A for Emacs 28.1.                                                |
;;;; |                                                                         |
;;;; | There are two weird things:                                             |
;;;; |                                                                         |
;;;; | 1. According to https://github.com/magit/magit/discussions/4507         |
;;;; |    (which the prompt for all this), there's no problem with Emacs       |
;;;; |    27.2, so why do we have Hack B for Emacs 27.2? Was it ever needed?   |
;;;; |                                                                         |
;;;; | 2. I suspect we screwed up in commit 1ba923c3: I suspect that when we   |
;;;; |    upgraded to Emacs 28.1 we incorporated Hack A rather than Hack B.    |
;;;; |    (But everything worked.)                                             |
;;;; |                                                                         |
;;;; | Anyway, here we are today with Emacs 28.1 and Magit 20230107.2134, and  |
;;;; | there's no need for either hack.                                        |
;;;; |                                                                         |
;;;; | -- Simon Katz, 2023-01-13                                               |
;;;; +-------------------------------------------------------------------------+

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
      (case 2
        ;; See comments at top of file. It seems we don't need this any more.
        ;; -- Simon Katz, 2023-01-13
        (1 (message-box (s-join " "
                                '("Revisit `nomis/hack-magit-diff`"
                                  "for this version of Magit."))))
        (2 nil))))))

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
