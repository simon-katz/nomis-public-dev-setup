;;; nomis-outline.el ---  -*- lexical-binding: t; -*-

;;; Code:

;;;; Requires

(require 'nomis-msg)
(require 'outline)

;;;; Ellipses

;; Copy-and-hack from
;; https://www.reddit.com/r/emacs/comments/e2u5n9/code_folding_with_outlineminormode/
(set-display-table-slot standard-display-table
                        'selective-display
                        (let ((face-offset (* (face-id 'shadow) (ash 1 22))))
                          (vconcat (mapcar (lambda (c) (+ face-offset c))
                                           " ▶▶▶"))))

;;;; outline-minor-mode

(add-hook 'prog-mode-hook 'outline-minor-mode)

(setopt outline-minor-mode-use-buttons 'in-margins)

;;;; outline-minor-mode-cycle

;; (setopt outline-minor-mode-cycle t)
;;
;; ^^ Don't set this. It's no good for top-level forms, where tab completion
;; gets lost in favour of the cycling. For example, with cursor at the "|" in
;; `(def|)` at top level, we get cycling instead of being offered completions of
;; "def".
;;
;; Instead:
;;
;; - Initially we used the `bicycle` commands. In any case, they're better
;;   because it cycles through all levels, not just the top two levels and
;;   then all.
;;
;; - Now we are in the process of implementing `nomis-tree`.
;;   - We will no longer need `bicycle` when we have implemented `:outline`
;;     versions of
;;     `nomis/tree/show-children-from-all-roots/incremental/less--aux` and
;;     `nomis/tree/show-children-from-all-roots/incremental/more--aux`
;;
;; If we change our mind, we need to go back to setting `outline-regexp` to
;; exclude top-level forms, which needs to be done separateley for Emacs Lisp,
;; Clojure and other languages. (There are historical commits we can look at.
;; See commit 2bb138fb "Don't make changes to `outline-regexp`".)

;;;; outline-minor-faces

(use-package outline-minor-faces
  :after outline
  ;; We now call `outline-minor-faces-mode`, if we want it, in sidecar-locals.
  ;; :config (add-hook 'outline-minor-mode-hook
  ;;                   #'outline-minor-faces-mode)
  )

(defun -nomis/hack-outline-minor-faces ()
  (set-face-attribute 'outline-minor-0 nil
                      :weight 'normal
                      :overline t
                      :background (face-background 'default)))

;; If we decide that the extra vertical space for overlines is bad, we can do:
;; `(setq overline-margin 0)`.

(add-hook 'outline-minor-faces-mode-hook '-nomis/hack-outline-minor-faces)

;;;; Outline colors

(defun nomis/outline/colors/set-default ()
  (set-face-attribute 'outline-1 nil :inherit 'font-lock-function-name-face)
  (set-face-attribute 'outline-2 nil :inherit 'font-lock-variable-name-face)
  (set-face-attribute 'outline-3 nil :inherit 'font-lock-keyword-face)
  (set-face-attribute 'outline-4 nil :inherit 'font-lock-comment-face)
  (set-face-attribute 'outline-5 nil :inherit 'font-lock-type-face)
  (set-face-attribute 'outline-6 nil :inherit 'font-lock-constant-face)
  (set-face-attribute 'outline-7 nil :inherit 'font-lock-builtin-face)
  (set-face-attribute 'outline-8 nil :inherit 'font-lock-string-face))

(defun nomis/outline/colors/set-rainbow-8 ()
  ;; 8 colors in rainbow order, at least when using a dark theme.
  (set-face-attribute 'outline-1 nil :inherit 'font-lock-string-face)
  (set-face-attribute 'outline-2 nil :inherit 'font-lock-variable-name-face)
  (set-face-attribute 'outline-3 nil :inherit 'font-lock-type-face)
  (set-face-attribute 'outline-4 nil :inherit 'font-lock-constant-face)
  (set-face-attribute 'outline-5 nil :inherit 'font-lock-keyword-face)
  (set-face-attribute 'outline-6 nil :inherit 'font-lock-function-name-face)
  (set-face-attribute 'outline-7 nil :inherit 'font-lock-comment-face)
  (set-face-attribute 'outline-8 nil :inherit 'font-lock-builtin-face))

(defun nomis/outline/colors/set-rainbow-4 ()
  ;; 4 colors in rainbow order, at least when using a dark theme.
  ;;
  ;; "Of York Gave Battle"
  (set-face-attribute 'outline-1 nil :inherit 'font-lock-string-face)
  (set-face-attribute 'outline-2 nil :inherit 'font-lock-variable-name-face)
  (set-face-attribute 'outline-3 nil :inherit 'font-lock-constant-face)
  (set-face-attribute 'outline-4 nil :inherit 'font-lock-function-name-face)
  (set-face-attribute 'outline-5 nil :inherit 'outline-1)
  (set-face-attribute 'outline-6 nil :inherit 'outline-2)
  (set-face-attribute 'outline-7 nil :inherit 'outline-3)
  (set-face-attribute 'outline-8 nil :inherit 'outline-4))

;; (nomis/outline/colors/set-default)
;; (nomis/outline/colors/set-rainbow-8)
(nomis/outline/colors/set-rainbow-4)

;;;; backline

;; I'm not sure I want this with my heading styling, but it might be useful if
;; I change things.

;; (use-package backline
;;   :after outline
;;   :config (advice-add 'outline-flag-region :after 'backline-update))

;;;; Show point and entry when jumping to grep results

(advice-add 'compilation-next-error-function
            :after
            (lambda (&rest _)
              (when (or (derived-mode-p 'outline-mode)
                        (bound-and-true-p outline-minor-mode))
                (outline-show-entry)))
            '((name . nomis/outline-show-entry-when-going-to-grep-results)))

;;; End

(provide 'nomis-outline)
