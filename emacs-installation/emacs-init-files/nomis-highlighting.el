;;;; Init stuff -- Misc highlighting.

;;;; ___________________________________________________________________________
;;;; Mode line

;; Defaults:
;; (set-face-background 'mode-line          "grey75")
;; (set-face-background 'mode-line-inactive "grey90")
;; (set-face-foreground 'mode-line          "black")
;; (set-face-foreground 'mode-line-inactive "grey20")

;; Tailoring
(progn
  (set-face-background 'mode-line "#ccccff")
  (set-face-background 'mode-line-inactive "grey75"))

;;;; ___________________________________________________________________________
;;;; Highlighting of the current line

(global-hl-line-mode 1)
;; (set-face-background 'hl-line "lightcyan")
;; (set-face-background 'hl-line "lightcyan1")
;; (set-face-background 'hl-line "azure2")
;; (set-face-background 'hl-line "slategray1")
;; (set-face-background 'hl-line "palegreen")
;; (set-face-background 'hl-line "palegreen1")
(set-face-background 'hl-line "palegoldenrod") ; was "darkseagreen1", but that doesn't work nicely with magit, and the magit-item-highlight face has disappeared
;; (set-face-background 'hl-line "grey97")
;; (set-face-background 'hl-line "white")
;; (set-face-background 'hl-line "RGB:9999/9999/9999")
;; (set-face-background 'hl-line "lightyellow")
;; (set-face-background 'hl-line "LightGoldenrodYellow")

;;;; ___________________________________________________________________________
;;;; Highlighting of text (usually symbol names) inside `` and inside `'.
;;;; e.g. `foo` and `foo'.

;;; How is it done in Lisp mode?
;;; See:
;;; - `lisp-mode-variables' in "lisp-mode.el"
;;;       (setq font-lock-defaults
;;;         (... lisp-font-lock-keywords-1 lisp-font-lock-keywords-2 ...)
;;; - `lisp-font-lock-keywords-2' in "font-lock.el"
;;;       (...
;;;        ;; Words inside `' tend to be symbol names.
;;;        ("`\\(\\sw\\sw+\\)'" 1 font-lock-constant-face prepend)
;;;        ...)

;;; I want Lisp mode's highlighting of `foo' in other modes, and I
;;; want similar highlighting for `foo`.

(defun nomis-highlight-symbol-quoting-in-text ()
  (dolist (regex '("`\\(.+?\\)[`']"))
    (font-lock-add-keywords nil `((,regex 1 font-lock-constant-face prepend)))))

(add-hook 'text-mode-hook 'nomis-highlight-symbol-quoting-in-text)
(add-hook 'prog-mode-hook 'nomis-highlight-symbol-quoting-in-text)

;;;; ___________________________________________________________________________
;;;; Symbol highlighting

(progn
  (require 'idle-highlight-mode)
  (defun nomis-turn-on-idle-highlight-mode ()
    (idle-highlight-mode t))
  (add-hook 'prog-mode-hook 'nomis-turn-on-idle-highlight-mode))

(provide 'nomis-highlighting)
