;;;; Init stuff -- Searching.

;;;; ___________________________________________________________________________

(require 'grep)
(require 'nomis-searching-directory-stuff)
(require 'nomis-searching-file-stuff)

;;;; ___________________________________________________________________________

(defun nomis/init-grep-mode ()
  (setq truncate-lines t))

(add-hook 'grep-mode-hook 'nomis/init-grep-mode)

;;;; ___________________________________________________________________________

(progn
  ;; M-> and M-< are by default bound to end-of-buffer and
  ;; beginning-of-buffer, which are also bound to C-end and C-home.
  ;; Rebind M-> and M-< without much lossage.
  (define-key global-map (kbd "M->") 'next-error)
  (define-key global-map (kbd "M-<") 'previous-error))

;;;; ___________________________________________________________________________
;;;; Better grepping.

(defun -nomis/hack-to-avoid-calling-shell-quote-argument ()
  ;; `shell-quote-argument` seems to be broken.
  ;;
  ;; `shell-quote-argument` is called in `grep-expand-keywords` in `grep.el`:
  ;;
  ;;     ("<R>" . (shell-quote-argument (or regexp "") grep-quoting-style))
  ;;
  ;; This expression is evaluated by `grep-expand-template` when it processes
  ;; the `<R>` placeholder.
  ;;
  ;; With our advice in place, we replace `<R>` in the template before calling
  ;; `orig-fun`, so when `grep-expand-template` iterates over
  ;; `grep-expand-keywords` looking for `<R>` in the command string, it's
  ;; already gone — and `shell-quote-argument` is never called for the regexp.
  ;;
  ;; Claude says: `shell-quote-argument' on macOS ignores its `posix' argument
  ;; and always backslash-escapes. This causes `$' to reach grep unescaped (the
  ;; shell strips the backslash), where it is interpreted as end-of-line rather
  ;; than a literal `$'. Fix by doing our own single-quote wrapping for <R>,
  ;; bypassing `shell-quote-argument' entirely. Note: inside single quotes, what
  ;; you type is what grep sees -- so type `\$' for a literal `$'.
  (advice-add 'grep-expand-template :around
              (lambda (orig-fun template &optional regexp &rest args)
                (let* ((quoted (concat "'"
                                       (replace-regexp-in-string "'"
                                                                 "'\\''"
                                                                 (or regexp "")
                                                                 t
                                                                 t)
                                       "'"))
                       (new-template (replace-regexp-in-string "<R>"
                                                               quoted
                                                               template
                                                               t
                                                               t)))
                  (apply orig-fun new-template regexp args)))
              '((name . nomis/single-quote-grep-regexp))))

(with-eval-after-load 'grep
  (-nomis/hack-to-avoid-calling-shell-quote-argument)
  (when (equal system-type 'darwin)
    (grep-apply-setting
     'grep-find-template
     (cl-case 2
       (1
        ;; The default computed on Sheringham
        "find <D> <X> -type f <F> -exec grep <C> -nH --null -e <R> \\{\\} +")
       (2
        ;; What I'm trying -- sort the files
        "find <D> <X> -type f <F> -print0 | sort -z | xargs -0 grep -I <C> -nH --null -e <R>")))))

;;;; Based on code found at
;;;; https://github.com/magnars/.emacs.d/blob/master/setup-rgrep.el.
;;;; Note:
;;;;   I had tried something that advised `grep-read-files`, but that didn't
;;;;   work. Looks like `grep-read-files` does some weird side-effecty stuff.
;;;;   In any case, I don't understand and I gave up on that approach.

(defun -nomis/rgrep-interactive-stuff (all-p)
  (progn
    (grep-compute-defaults)
    (cond
     ((and grep-find-command (equal current-prefix-arg '(16)))
      (list (read-from-minibuffer "Run: " grep-find-command
                                  nil nil 'grep-find-history)))
     ((not grep-find-template)
      (error "grep.el: No `grep-find-template' available"))
     (t (let* ((regexp (grep-read-regexp))
               (files (if all-p
                          "*"
                        (grep-read-files regexp)))
               (dir (ido-read-directory-name "Base directory: "
                                             nil default-directory t))
               (confirm (equal current-prefix-arg '(4))))
          (list regexp files dir confirm))))))

(defun nomis/rgrep (regexp &optional files dir confirm)
  "A variation of `rgrep` that:
- uses `ido-read-directory-name` for nicer directory navigation."
  (interactive (-nomis/rgrep-interactive-stuff nil))
  (rgrep regexp files dir confirm))

(defun nomis/rgrep-all-unignored-files/TODO-CHECK-WHETHER-BROKEN (regexp &optional files dir confirm)
  "A variation of `rgrep` that:
- uses `ido-read-directory-name` for nicer directory navigation
- searches all (unignored) files."
  (interactive (-nomis/rgrep-interactive-stuff t))
  (rgrep regexp files dir confirm))

;; (define-key global-map (kbd "H-q g a") 'nomis/rgrep)
;; (define-key global-map (kbd "H-q g g") 'nomis/rgrep-all-unignored-files)

;;;; ___________________________________________________________________________
;;;; ---- Stuff for rgrep and lgrep ----

;; (defadvice grep-tag-default (around change-space-to-todo ()) ; TODO: Huh? What's this for?
;;   (flet ((do-it () ad-do-it))
;;     (when (string-equal (do-it) "")
;;       (setq ad-return-value
;;             ;; use \ below so this doesn't show up in searches
;;             "TOD\O"))))
;; (ad-activate 'grep-tag-default)

;; (define-key global-map (kbd "C-c C-v C-s") 'rgrep)

;;;; ___________________________________________________________________________
;;;; ---- Window position for hits ----

(defun nomis/next-error-stuff ()
  ;; (recenter 2) ; For now, uncomment this when you want it.
  )

(add-hook 'next-error-hook 'nomis/next-error-stuff)

;;;; ___________________________________________________________________________

(provide 'nomis-searching)
