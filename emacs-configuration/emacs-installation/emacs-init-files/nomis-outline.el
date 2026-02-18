;;; Init stuff -- nomis-outline --  -*- lexical-binding: t -*-

(require 'nomis-msg)

;;; Utilities

;;;; -nomis/outline-pulse-current-section

(defun -nomis/outline-pulse-current-section ()
  (let ((start (point)))
    (cl-flet ((next-same-level-heading ()
                (save-excursion (ignore-errors
                                  (outline-forward-same-level 1)
                                  (point))))
              (next-up-one-level-heading ()
                (save-excursion (ignore-errors
                                  (outline-up-heading 1)
                                  (outline-forward-same-level 1)
                                  (unless (= (point) start)
                                    ;; We have this guard because
                                    ;; `outline-up-heading` is broken when
                                    ;; there's no up-one-level heading.
                                    (point))))))
      (let* ((end (or (next-same-level-heading)
                      (next-up-one-level-heading)
                      (point-max))))
        (pulse-momentary-highlight-region start end)))))

;;; Ellipses

;; Copy-and-hack from
;; https://www.reddit.com/r/emacs/comments/e2u5n9/code_folding_with_outlineminormode/
(set-display-table-slot standard-display-table
                        'selective-display
                        (let ((face-offset (* (face-id 'shadow) (ash 1 22))))
                          (vconcat (mapcar (lambda (c) (+ face-offset c))
                                           " ▶▶▶"))))

;;; outline-minor-mode

(add-hook 'prog-mode-hook 'outline-minor-mode)

(setopt outline-minor-mode-use-buttons 'in-margins)

;;; outline-minor-mode-cycle

;; (setopt outline-minor-mode-cycle t)
;;
;; ^^ Don't set this. It's no good for top-level forms, where tab completion
;; gets lost in favour of the cycling. For example, with cursor at the "|" in
;; `(def|)` at top level, we get cycling instead of being offered completions of
;; "def".
;;
;; Instead, use the `bicycle` commands. In any case, they're better because it
;; cycles through all levels, not just the top two levels and then all.
;;
;; If we change our mind, we need to go back to setting `outline-regexp` to
;; exclude top-level forms, which needs to be done separateley for Emacs Lisp,
;; Clojure and other languages. (There are historical commits we can look at.
;; See commit 2bb138fb "Don't make changes to `outline-regexp`".)

;; Update: See `nomis/outline-cycle-or-indent-or-complete`.

;;; outline-minor-faces

(use-package outline-minor-faces
  :after outline
  ;; We now call `outline-minor-faces-mode`, if we want it, in sidecar-locals.
  ;; :config (add-hook 'outline-minor-mode-hook
  ;;                   #'outline-minor-faces-mode)
  )

(defun -nomis/hack-outline-minor-faces ()
  (set-face-attribute 'outline-minor-0 nil
                      :overline t
                      :background (face-background 'default)))

(add-hook 'outline-minor-faces-mode-hook '-nomis/hack-outline-minor-faces)

;;; Outline colors

(defun nomis/outline-colors/set-default ()
  (set-face-attribute 'outline-1 nil :inherit 'font-lock-function-name-face)
  (set-face-attribute 'outline-2 nil :inherit 'font-lock-variable-name-face)
  (set-face-attribute 'outline-3 nil :inherit 'font-lock-keyword-face)
  (set-face-attribute 'outline-4 nil :inherit 'font-lock-comment-face)
  (set-face-attribute 'outline-5 nil :inherit 'font-lock-type-face)
  (set-face-attribute 'outline-6 nil :inherit 'font-lock-constant-face)
  (set-face-attribute 'outline-7 nil :inherit 'font-lock-builtin-face)
  (set-face-attribute 'outline-8 nil :inherit 'font-lock-string-face))

(defun nomis/outline-colors/set-rainbow-8 ()
  ;; 8 colors in rainbow order, at least when using a dark theme.
  (set-face-attribute 'outline-1 nil :inherit 'font-lock-string-face)
  (set-face-attribute 'outline-2 nil :inherit 'font-lock-variable-name-face)
  (set-face-attribute 'outline-3 nil :inherit 'font-lock-type-face)
  (set-face-attribute 'outline-4 nil :inherit 'font-lock-constant-face)
  (set-face-attribute 'outline-5 nil :inherit 'font-lock-keyword-face)
  (set-face-attribute 'outline-6 nil :inherit 'font-lock-function-name-face)
  (set-face-attribute 'outline-7 nil :inherit 'font-lock-comment-face)
  (set-face-attribute 'outline-8 nil :inherit 'font-lock-builtin-face))

(defun nomis/outline-colors/set-rainbow-4 ()
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

;; (nomis/outline-colors/set-default)
;; (nomis/outline-colors/set-rainbow-8)
(nomis/outline-colors/set-rainbow-4)

;;; backline

;; I'm not sure I want this with my heading styling, but it might be useful if
;; I change things.

;; (use-package backline
;;   :after outline
;;   :config (advice-add 'outline-flag-region :after 'backline-update))

;;; bicycle

;;;; bicycle basics

;; `bicycle` combines `outline` and `hideshow`.

(require 'bicycle)

;;;; Provide feedback in bicycle-cycle-local

(defvar *nomis/outline-in-bicycle-cycle-local?* nil)

(advice-add 'bicycle-cycle-local
            :around
            (lambda (orig-fun &rest args)
              (let* ((*nomis/outline-in-bicycle-cycle-local?* t))
                (apply orig-fun args)))
            '((name . nomis/outline-bicycle-feedback)))

(advice-add 'outline-show-subtree
            :around
            (lambda (orig-fun &rest args)
              (when *nomis/outline-in-bicycle-cycle-local?*
                (-nomis/outline-pulse-current-section))
              (apply orig-fun args))
            '((name . nomis/outline-bicycle-feedback)))

;;;; Provide feedback in bicycle-cycle-global

(defvar *nomis/outline-in-bicycle-cycle-global?* nil)

(advice-add 'bicycle-cycle-global
            :around
            (lambda (orig-fun &rest args)
              (let* ((*nomis/outline-in-bicycle-cycle-global?* t))
                (apply orig-fun args)))
            '((name . nomis/outline-bicycle-feedback)))

(advice-add 'outline-show-all
            :around
            (lambda (orig-fun &rest args)
              (when *nomis/outline-in-bicycle-cycle-global?*
                (nomis/msg/pulse-buffer))
              (apply orig-fun args))
            '((name . nomis/outline-bicycle-feedback)))

;;; End

(provide 'nomis-outline)
