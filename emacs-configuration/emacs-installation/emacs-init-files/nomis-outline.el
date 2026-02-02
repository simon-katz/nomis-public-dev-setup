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

;; Allow visibility cycling on headings. We can do this because we don't count
;; code as headings (where we need Tab for code completion).
(setopt outline-minor-mode-cycle t)

;;; outline-minor-faces

(use-package outline-minor-faces
  :after outline
  :config (add-hook 'outline-minor-mode-hook
                    #'outline-minor-faces-mode))

(defun -nomis/hack-outline-minor-faces ()
  (set-face-attribute 'outline-minor-0 nil
                      :overline t
                      :background (face-background 'default)))

(add-hook 'outline-minor-faces-mode-hook '-nomis/hack-outline-minor-faces)

;;; bicycle

;; `bicycle` combines `outline` and `hideshow`. We don't use the `hideshow`
;; part, because, we are ignoring code (top-level forms), but `bicycle-cycle`
;; and `bicycle-cycle-global` are useful.

(require 'bicycle)

(with-eval-after-load 'bicycle
  (define-key outline-minor-mode-map [C-tab] 'bicycle-cycle)
  (define-key outline-minor-mode-map [S-tab] 'bicycle-cycle-global))

;;; Emacs Lisp

(defun -nomis/set-emacs-lisp-outline ()
  ;; Note that we are severely simplifying the value of `outline-regexp`, which
  ;; excludes autoload comments amongst other things. I'm not sure what else we
  ;; lose with our simplification.
  ;;
  ;; See the `(setq-local outline-regexp ...)` in `lisp-mode`.
  ;;
  ;; See also "The default outline-regexp for elisp-mode..." in
  ;; `my/elisp-outline-regexp-setup` at
  ;; https://www.reddit.com/r/emacs/comments/e2u5n9/comment/f924040/?utm_source=share&utm_medium=web3x&utm_name=web3xcss&utm_term=1&utm_content=share_button
  ;;
  ;; Set `outline-regexp` to `;;;` (and more semicolons) comments only, not
  ;; top-level forms.
  (setq-local outline-regexp ";;;;*") ; comments only
  )

(add-hook 'emacs-lisp-mode-hook '-nomis/set-emacs-lisp-outline)

;;; Clojure

(defun -nomis/set-clojure-outline ()
  ;; Note that we are changing the value of `outline-regexp` set in
  ;; `clojure-mode-variables`.
  ;;
  ;; Set `outline-regexp` to `;;;;` (and more semicolons) comments only, not
  ;; top-level forms.
  (setq-local outline-regexp ";;;;;*")
  )

(add-hook 'clojure-mode-hook '-nomis/set-clojure-outline)

;;; End

(provide 'nomis-outline)
