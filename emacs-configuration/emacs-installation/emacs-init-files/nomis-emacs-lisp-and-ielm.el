;;; Init stuff -- emacs-lisp and ielm  -*- lexical-binding: t; -*-

;;; Describe symbol at point

;; This is a replacement for `elisp-slime-nav`'s describe feature.

;; `elisp-slime-nav` used to bind `C-c C-d d` / `C-c C-d C-d` to describe the
;; symbol at point. Emacs's built-in `describe-symbol` covers the same ground.

(dolist (map (list emacs-lisp-mode-map
                   lisp-interaction-mode-map))
  (define-key map (kbd "C-c C-d d") #'describe-symbol)
  (define-key map (kbd "C-c C-d C-d") #'describe-symbol))

(with-eval-after-load 'ielm
  (define-key ielm-map (kbd "C-c C-d d") #'describe-symbol)
  (define-key ielm-map (kbd "C-c C-d C-d") #'describe-symbol))

;;; xref

;;;; Make xref work in ielm

(add-hook 'ielm-mode-hook
          (lambda ()
            (add-hook 'xref-backend-functions #'elisp--xref-backend nil t)))

;;;; Make xref buffers use same window

;; This affects all modes, not just emacs-lisp and ielm. Maybe that's OK.

(add-to-list 'display-buffer-alist
             '("\\*xref\\*"
               (display-buffer-same-window)))

;;;; On `M-.` for package name, return to previous position in file

;; By default it goes to the `(provide ...)` line. Yeuch!

;;;;; Update `save-place-alist` on every command

;; By default, Emacs only updates `save-place-alist` when we kill a buffer or
;; exit Emacs. If we are jumping back and forth between open buffers, the
;; "saved" position stays stale. So:

(defun nomis/save-place-to-alist ()
  (when (and (fboundp 'save-place-to-alist)
             buffer-file-name)
    (save-place-to-alist)))

(add-hook 'post-command-hook 'nomis/save-place-to-alist)

;;;;; Add advice to `xref-find-definitions`

;; Restore position if we landed on '(provide ...)'.

(defun nomis/xref-restore-pos-if-stupid-place (&rest _)
  "Restore save-place if `xref` lands on `;;;; Code:` or `(provide ...)`."
  (save-excursion
    (beginning-of-line)
    (when (or (looking-at ";;; Code")
              (looking-at "[[:space:]]*(provide[[:space:]]+'"))
      (let* ((true-name (file-truename buffer-file-name))
             (saved-pos (or (and (boundp 'save-place-alist)
                                 (cdr (assoc true-name save-place-alist)))
                            1 ; `save-place` removes entries when we go to BOF
                            )))
        (when saved-pos
          ;; It seems that `xref` does its own saving of positions after
          ;; `xref-find-definitions` finishes, so do this in a timer:
          (run-with-timer 0 nil
                          (lambda (buf pos)
                            (with-current-buffer buf
                              (goto-char pos)
                              ;; Emacs will have blatted the saved place, so:
                              (nomis/save-place-to-alist)))
                          (current-buffer)
                          saved-pos))))))

(advice-add 'xref-find-definitions :after 'nomis/xref-restore-pos-if-stupid-place)

;;; Other stuff

(defvar nomis/lisp-and-ielm-mode-hook-functions
  `(rainbow-delimiters-mode
    paredit-mode
    paxedit-mode ; some commands (at least) don't work in ielm mode
    ;; See https://github.com/clojure-emacs/clojure-mode/issues/516#issuecomment-569336063
    ,(lambda () (set (make-local-variable 'comment-column) 0))
    turn-on-eldoc-mode
    ;; aggressive-indent-mode
    ))

(dolist (hook '(emacs-lisp-mode-hook
                ielm-mode-hook
                lisp-data-mode-hook))
  (dolist (hook-fun nomis/lisp-and-ielm-mode-hook-functions)
    (add-hook hook hook-fun)))

(with-eval-after-load 'paredit
  ;; As of me upgrading
  ;;   from Emacs 28.1.3 to 28.2
  ;;   from Paredit 20191121.2328 to 20221127.1452
  ;; there's a conflict between Paredit and ielm: Pressing RETURN in an ielm
  ;; buffer invokes `paredit-RET`, which inserts a newline and does not
  ;; evaluate. There's no way to invoke `ielm-return` from the keyboard.
  ;;
  ;; Maybe take a look at `cider-repl-setup-paredit` for other ideas, but this
  ;; seems to fix things:
  (define-key paredit-mode-map (kbd "RET") nil)
  (define-key paredit-mode-map (kbd "C-j") 'paredit-newline)
  (with-eval-after-load 'ielm
    (define-key ielm-map (kbd "RET") 'ielm-return)))

;;; End

(provide 'nomis-emacs-lisp-and-ielm)
