;;; Init stuff -- nomis-outline --  -*- lexical-binding: t -*-

;;; Ellipses

;; Copy-and-hack from
;; https://www.reddit.com/r/emacs/comments/e2u5n9/code_folding_with_outlineminormode/
(set-display-table-slot standard-display-table
                        'selective-display
                        (let ((face-offset (* (face-id 'shadow) (ash 1 22))))
                          (vconcat (mapcar (lambda (c) (+ face-offset c))
                                           " ▶▶▶"))))

;;; outline-minor-mode

(setopt outline-minor-mode-prefix (kbd "C-S-o"))

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

;;; backline

;; I'm not sure I want this with my heading styling, but it might be useful if
;; I change things.

;; (use-package backline
;;   :after outline
;;   :config (advice-add 'outline-flag-region :after 'backline-update))

;;; bicycle

;; `bicycle` combines `outline` and `hideshow`. We don't use the `hideshow`
;; part, because, we are ignoring code (top-level forms), but `bicycle-cycle`
;; and `bicycle-cycle-global` are useful.

(require 'bicycle)

(with-eval-after-load 'bicycle
  (define-key outline-minor-mode-map [C-tab] 'bicycle-cycle)
  (define-key outline-minor-mode-map [S-tab] 'bicycle-cycle-global))

;; TODO: Add visual feedback when calling:
;;       - `outline-show-subtree` from `bicycle-cycle-local`.
;;       - `outline-show-all` from `bicycle-cycle-global`

;;; End

(provide 'nomis-outline)
