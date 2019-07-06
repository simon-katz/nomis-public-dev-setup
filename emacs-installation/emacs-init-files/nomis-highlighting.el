;;;; Init stuff -- Misc highlighting.

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

;; (global-hl-line-mode 1)

(progn
  (defvar nomis/hl-line/approach 0)
  (set-face-attribute 'hl-line nil
                      :inherit nil
                      :background (case nomis/hl-line/approach
                                    (0 "darkseagreen1")
                                    (1 "palegoldenrod")
                                    (2 'unspecified))
                      :box (case nomis/hl-line/approach
                             (0 nil)
                             (1 nil)
                             (2 (list :line-width -1
                                      :color "grey25"
                                      :style nil)))))

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
  (add-hook 'prog-mode-hook 'nomis/turn-on-idle-highlight-mode))

(provide 'nomis-highlighting)
