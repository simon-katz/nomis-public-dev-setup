;;; Init stuff -- nomis-outline --  -*- lexical-binding: t -*-

;;; To-dos

;; TODO: See https://www.reddit.com/r/emacs/comments/e2u5n9/code_folding_with_outlineminormode/

;;; `outline` ellipses

(defun -nomis/outline-set-buffer-local-ellipsis* (ellipsis)
  ;; Copy-and-hack from
  ;; https://www.jamescherti.com/emacs-customize-ellipsis-outline-minor-mode/
  "Set a buffer-local ellipsis string ELLIPSIS for outline folding display.

This function configures the current buffer to use a custom ellipsis string for
selective display, typically in `outline-mode' or `outline-minor-mode'.

The string ELLIPSIS is trimmed of trailing whitespace before use, as such
whitespace can be misleading when lines are truncated or visually wrapped. In
those cases, the trailing space may appear on a new visual line, creating the
false impression of an additional line. Deleting this apparent line can
inadvertently remove the entire folded logical line."
  (let* ((display-table (or buffer-display-table (make-display-table)))
         (face-offset (* (face-id 'shadow) (ash 1 22)))
         (value (vconcat (mapcar (lambda (c)
                                   (+ face-offset c))
                                 (string-trim-right ellipsis)))))
    (set-display-table-slot display-table 'selective-display value)
    (setq buffer-display-table display-table)))

(defun -nomis/outline-set-buffer-local-ellipsis ()
  (-nomis/outline-set-buffer-local-ellipsis* " ▶▶▶"))

(add-hook 'outline-mode-hook '-nomis/outline-set-buffer-local-ellipsis)
(add-hook 'outline-minor-mode-hook '-nomis/outline-set-buffer-local-ellipsis)

;;; `outline-minor-mode`

(setopt outline-minor-mode-prefix (kbd "C-S-o"))

(add-hook 'prog-mode-hook 'outline-minor-mode)

(setopt outline-minor-mode-use-buttons 'in-margins)

;;; `outline-minor-faces`

(with-eval-after-load 'outline
  (require 'outline-minor-faces))

(add-hook 'outline-minor-mode-hook 'outline-minor-faces-mode)

(defun -nomis/hack-outline-minor-faces ()
  (set-face-attribute 'outline-minor-0 nil
                      :overline t
                      :background (face-background 'default)))

(add-hook 'outline-minor-faces-mode-hook '-nomis/hack-outline-minor-faces)

;;; `bicycle`

;; `bicycle` combines `outline` and `hideshow`. We don't use the `hideshow`
;; part, because, we are ignoring code (top-level forms), but `bicycle-cycle`
;; and `bicycle-cycle-global` are useful.

(require 'bicycle)

(with-eval-after-load 'bicycle
  (define-key outline-minor-mode-map [C-tab] 'bicycle-cycle)
  (define-key outline-minor-mode-map [S-tab] 'bicycle-cycle-global))

;;; `-nomis/set-lispy-outline`

;; Make `outline-next-heading` etc ignore code (top-level forms).

(defun -nomis/set-lispy-outline (min-n-semicolons)
  ;; Don't count code (top-level forms) as headings.
  (setq-local outline-regexp
              ;; We include only comments; not top-level forms.
              (s-concat (make-string min-n-semicolons ?\;)
                        ";*" ; with a space at end, `outline-demote` breaks
                        ))
  ;; Allow visibility cycling on headings. We can do this because we are not
  ;; counting code as headings (where we need Tab for code completion).
  (setq-local outline-minor-mode-cycle t))

;;;; Emacs Lisp

;; Hmmmm. Maybe look at the (non-trivial) value for `outline-regexp` in
;; `emacs-lisp-mode`. What's that all about? (Look at the default value; not the
;; simple value set in this file.)

(defun -nomis/set-emacs-lisp-outline ()
  ;; Note that we are severely simplifying the value of `outline-regexp`. I'm
  ;; not sure what we will lose.
  (-nomis/set-lispy-outline 3))

(add-hook 'emacs-lisp-mode-hook '-nomis/set-emacs-lisp-outline)

;;;; Clojure

(defun -nomis/set-clojure-outline ()
  ;; Note that we are changing the value of `outline-regexp` set in
  ;; `clojure-mode-variables`.
  (-nomis/set-lispy-outline 4))

(add-hook 'clojure-mode-hook '-nomis/set-clojure-outline)

;;; End

(provide 'nomis-outline)
