;;;; Init stuff -- Misc highlighting.

;;;; ___________________________________________________________________________

(require 'nav-flash)

;;;; ___________________________________________________________________________
;;;; Mode line

;;; Defaults:

(progn
  (defvar *nomis/untailored-mode-line-face-foreground*          (face-foreground 'mode-line))           ; "black"
  (defvar *nomis/untailored-mode-line-face-background*          (face-background 'mode-line))           ; "grey75"
  (defvar *nomis/untailored-mode-line-inactive-face-foreground* (face-foreground 'mode-line-inactive))  ; "grey20"
  (defvar *nomis/untailored-mode-line-inactive-face-background* (face-background 'mode-line-inactive))) ; "grey90"

(defun nomis/reset-untailored-mode-line-fgs-and-bgs ()
  (set-face-foreground 'mode-line          *nomis/untailored-mode-line-face-foreground*)
  (set-face-background 'mode-line          *nomis/untailored-mode-line-face-background*)
  (set-face-foreground 'mode-line-inactive *nomis/untailored-mode-line-inactive-face-foreground*)
  (set-face-background 'mode-line-inactive *nomis/untailored-mode-line-inactive-face-background*))

;;; Tailoring

(progn
  (defvar *nomis/mode-line-face-foreground*          *nomis/untailored-mode-line-face-foreground*)
  (defvar *nomis/mode-line-face-background*          "#ccccff")
  (defvar *nomis/mode-line-inactive-face-foreground* *nomis/untailored-mode-line-inactive-face-foreground*)
  (defvar *nomis/mode-line-inactive-face-background* "grey75"))

(defun nomis/set-mode-line-fgs-and-bgs ()
  (set-face-foreground 'mode-line          *nomis/mode-line-face-foreground*)
  (set-face-background 'mode-line          *nomis/mode-line-face-background*)
  (set-face-foreground 'mode-line-inactive *nomis/mode-line-inactive-face-foreground*)
  (set-face-background 'mode-line-inactive *nomis/mode-line-inactive-face-background*))

(nomis/set-mode-line-fgs-and-bgs)

;;;; ___________________________________________________________________________
;;;; Highlighting of the current line

(require 'hl-line)

(progn
  ;; I might almost want global-hl-line-mode, but with that magit-status-mode's
  ;; diff highlighting gets blatted.
  ;; So:
  (dolist (h '(text-mode-hook
               prog-mode-hook
               grep-mode-hook
               dired-mode-hook
               org-mode-hook))
    (add-hook h 'hl-line-mode)))

;; (set-face-background 'hl-line "lightcyan")
;; (set-face-background 'hl-line "lightcyan1")
;; (set-face-background 'hl-line "azure2")
;; (set-face-background 'hl-line "slategray1")
;; (set-face-background 'hl-line "palegreen")
;; (set-face-background 'hl-line "palegreen1")
;; (set-face-background 'hl-line "grey97")
;; (set-face-background 'hl-line "white")
;; (set-face-background 'hl-line "RGB:9999/9999/9999")
;; (set-face-background 'hl-line "lightyellow")
;; (set-face-background 'hl-line "LightGoldenrodYellow")

;;;; ___________________________________________________________________________
;;;; Highlighting of the current line -- Fix `next-error` and `prev-error`.

;;;; Without this `hl-line-mode` hack, I have this problem:
;;;; - I use `next-error` and `prev-error` in a grep buffer to navigate to the
;;;;   next/prev hit.
;;;; - The hit is in a not-yet-opened file.
;;;; - If the newly-created buffer has `hl-line-mode` enabled, I don't get
;;;;   `hl-line-mode` highlighting.
;;;; (Moving the cursor or doing other things causes the highlighting to be
;;;; shown, but that's not good enough.)

;;;; Also use `nav-flash-show`, because:
;;;; - The built-in flashing of the hit appears to be unreliable.
;;;; - Sometimes, even when it works, the built-in flashing can be hard
;;;;   to notice.

(advice-add 'next-error
            :after
            (lambda (&rest args)
              (run-at-time 0.1
                           nil
                           (lambda ()
                             (when (bound-and-true-p hl-line-mode)
                               (hl-line-mode 0)
                               (hl-line-mode 1))
                             (nav-flash-show nil nil 'region)
                             (beacon-blink))))
            '((name . nomis/hl-line/next-error-error-extras)))

;; (advice-remove 'next-error 'nomis/hl-line/next-error-error-extras)


;;;; ___________________________________________________________________________
;;;; Highlighting of text (usually symbol names) like `xxxx` and `xxxx'.

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
;;; So the first two of these have highlighting and the second two do not:
;;; - plop `plop` plop
;;; - plop `plop' plop
;;; - plop 'plop' plop
;;; - plop 'plop` plop

(defun nomis/highlight-symbol-quoting-in-text ()
  (dolist (regex '("`\\(.+?\\)[`']"))
    (font-lock-add-keywords nil
                            `((,regex 1 font-lock-constant-face prepend))
                            t ; add at end -- otherwise breaks highlighting in org mode
                            )))

(add-hook 'text-mode-hook 'nomis/highlight-symbol-quoting-in-text)
(add-hook 'prog-mode-hook 'nomis/highlight-symbol-quoting-in-text)

;;;; ___________________________________________________________________________
;;;; Symbol highlighting

(progn
  (require 'nomis-idle-highlight-mode)
  (defun nomis/turn-on-idle-highlight-mode ()
    (nomis/idle-highlight-mode t))
  (add-hook 'prog-mode-hook 'nomis/turn-on-idle-highlight-mode)
  (add-hook 'ielm-mode-hook 'nomis/turn-on-idle-highlight-mode)
  (setq nomis/idle-highlight-exceptions
        (append '("-")
                nomis/idle-highlight-exceptions)))

(provide 'nomis-highlighting)
