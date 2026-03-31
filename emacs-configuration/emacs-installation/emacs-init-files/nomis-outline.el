;;; nomis-outline.el ---  -*- lexical-binding: t; -*-

;;; Code:

;;;; Requires

(require 'nomis-msg)
(require 'nomis-tree-lineage-specs)
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
  (set-face-attribute 'outline-1 nil :inherit 'font-lock-variable-name-face)
  (set-face-attribute 'outline-2 nil :inherit 'font-lock-string-face)
  (set-face-attribute 'outline-3 nil :inherit 'font-lock-constant-face)
  (set-face-attribute 'outline-4 nil :inherit 'font-lock-function-name-face)
  (set-face-attribute 'outline-5 nil :inherit 'outline-1)
  (set-face-attribute 'outline-6 nil :inherit 'outline-2)
  (set-face-attribute 'outline-7 nil :inherit 'outline-3)
  (set-face-attribute 'outline-8 nil :inherit 'outline-4))

;; (nomis/outline/colors/set-default)
;; (nomis/outline/colors/set-rainbow-8)
(nomis/outline/colors/set-rainbow-4)

;;;; Show entry when jumping from other places

(defun -nomis/outline/show-after-find/after-advice (&rest _)
  (when (or (derived-mode-p 'outline-mode)
            (bound-and-true-p outline-minor-mode))
    (nomis/tree/ls/show-after-find)))

(advice-add 'compilation-next-error-function
            :after
            #'-nomis/outline/show-after-find/after-advice)

(advice-add 'xref-find-definitions
            :after
            #'-nomis/outline/show-after-find/after-advice)

(eval-after-load 'nomis-clojure-lsp
  (progn
    (advice-add 'nomis/clojure-lsp-and-cider/find-definition
                :after
                #'-nomis/outline/show-after-find/after-advice)
    (advice-add 'nomis/clojure-lsp-and-cider/find-definition-v2
                :after
                #'-nomis/outline/show-after-find/after-advice)))

;;;; `outline-regexp`

;;;;; Emacs Lisp

(defun -nomis/set-emacs-lisp-outline ()
  ;; A hacked version of the `(setq-local outline-regexp ...)` in `lisp-mode`
  ;; that doesn't include top-level forms.
  (setq-local outline-regexp ";;;;*"))

(add-hook 'emacs-lisp-mode-hook '-nomis/set-emacs-lisp-outline)

;;;;; Clojure

(defun -nomis/set-clojure-outline ()
  ;; Note that we are changing the value of `outline-regexp` set in
  ;; `clojure-mode-variables`.
  ;;
  ;; Set `outline-regexp` to `;;;;` (and more semicolons) comments only, not
  ;; top-level forms.
  (setq-local outline-regexp ";;;;;*")
  (setq-local outline-level (lambda ()
                              (save-excursion
                                (looking-at outline-regexp)
                                (- (match-end 0)
                                   (match-beginning 0)
                                   3 ; 4 semicolons is level 1, so subtract 3
                                   )))))

(add-hook 'clojure-mode-hook '-nomis/set-clojure-outline)

;;;; Fix `outline-mark-subtree` so that selection goes to next heading

(defun nomis/outline/mark-subtree/extend (&rest _)
  "If appropriate, extend region if mark is not on a heading."
  (when (use-region-p)
    (save-excursion
      (goto-char (mark))
      (while (and (not (outline-on-heading-p))
                  (not (eobp)))
        (forward-char 1))
      (set-mark (point)))))

(advice-add 'outline-mark-subtree
            :after
            #'nomis/outline/mark-subtree/extend
            '((name . -nomis/outline/mark-subtree/extend)))

;; (advice-remove 'outline-mark-subtree '-nomis/outline/mark-subtree/extend)

;;; End

(provide 'nomis-outline)
