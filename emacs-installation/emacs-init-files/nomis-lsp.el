;;; nomis-lsp.el --- LSP tailoring -*- lexical-binding: t; -*-

;;;; ___________________________________________________________________________
;;;; General setup.

;;;; TODO: Where are `lsp` and `lsp-clojure` options and commands listed?
;;;;       I haven't found documentation yet.
;;;;       Is there a better way to learn than looking at the source code in
;;;;       the following?...
;;;;       - `lsp-mode` etc
;;;;       - `lsp-clojure`

;;;; Clojure snippets
;;;; - https://clojure-lsp.github.io/clojure-lsp/features/#snippets

;;;; Useful commands:
;;;; - `lsp-workspace-restart`
;;;; - `lsp-treemacs-errors-list`
;;;; - `lsp-clojure-xxxx` (eg `lsp-clojure-move-to-let`)

(setq lsp-keymap-prefix "H-q H-s")

;; 2021-08-20 Suddenly this isn't needed. (Why?)
;; (defun nomis/lsp-eldoc ()
;;   ;; Don't blat signatures from CIDER.
;;   (unless (ignore-errors (cider-repls))
;;     (lsp-hover)))

(defun nomis/lsp-init ()
  (setq
   lsp-lens-enable                   t
   lsp-enable-symbol-highlighting    t
   lsp-ui-doc-enable                 nil ; Don't show horrible big grey boxes.
   ;; lsp-eldoc-hook                    '(nomis/lsp-eldoc)
   lsp-enable-indentation            nil ; Use CIDER indentation.
   lsp-ui-sideline-show-code-actions nil ; Don't show clutter! But see `nomis/lsp-toggle-lsp-ui-sideline-show-code-actions`.

   ;; lsp-completion-enable            nil ; Uncomment to use CIDER completion
   ;; gc-cons-threshold (* 100 1024 1024)
   ;; read-process-output-max (* 1024 1024)
   ;; treemacs-space-between-root-nodes nil
   ;; lsp-signature-auto-activate      nil ; TODO: What is this?
   ;; lsp-enable-dap-auto-configure    nil ; => t
   ;; lsp-enable-file-watchers         nil ; => t
   ;; lsp-enable-folding               nil ; => t
   ;; lsp-enable-imenu                 nil ; => t
   ;; lsp-enable-links                 nil ; => t
   ;; lsp-enable-on-type-formatting    nil ; => t
   ;; lsp-enable-snippet               nil ; => t
   ;; lsp-enable-text-document-color   nil ; => t
   ;; lsp-enable-xref                  nil ; => t
   ))

(add-hook 'lsp-mode-hook 'nomis/lsp-init)

;;;; ___________________________________________________________________________
;;;; Tailoring of faces, needed especially for light themes.

(defconst nomis/lsp-symbol-highlight-color "#ffff00")

(face-spec-set
 'lsp-face-highlight-textual
 ;; c.f. tailoring of `nomis/idle-highlight-muted`.
 `((((min-colors 88) (background dark))
    (:background ,nomis/lsp-symbol-highlight-color :foreground "black"))
   (((background dark)) (:background ,nomis/lsp-symbol-highlight-color :foreground "black"))
   (((min-colors 88)) (:background ,nomis/lsp-symbol-highlight-color))
   (t (:background ,nomis/lsp-symbol-highlight-color))))

(face-spec-set
 'lsp-ui-sideline-code-action
 '((t
    :foreground "gray95"
    :background "springgreen4")))

(face-spec-set
 'lsp-ui-sideline-global ; this is used for Flycheck errors
 '((t
    :foreground "black"
    :background "yellow2")))

(with-eval-after-load 'lsp-diagnostics
  (setf (alist-get 'unnecessary lsp-diagnostics-attributes)
        '(:foreground "grey45") ; was "gray"
        ))

;;;; ___________________________________________________________________________
;;;; Don't stop showing flycheck info in the echo area. Useful, and also
;;;; sometimes diagnostics are not shown in the LSP UI.

(advice-add
 'lsp-ui-sideline-mode
 :after
 (lambda (&rest _)
   (when lsp-ui-sideline-mode
     (kill-local-variable 'flycheck-display-errors-function)))
 '((name . nomis/show-flycheck-info)))

;;;; ___________________________________________________________________________
;;;; Additional functionality

(defun nomis/lsp-toggle-lsp-ui-sideline-show-code-actions ()
  (interactive)
  (cl-flet ((force-update-sideline-with-a-slegehammer
             ()
             (let* ((inhibit-message t))
               (lsp-ui-sideline-mode 0)
               (lsp-ui-sideline-mode 1))))
    (setq lsp-ui-sideline-show-code-actions
          (not lsp-ui-sideline-show-code-actions))
    (force-update-sideline-with-a-slegehammer))
  (message "Set `lsp-ui-sideline-show-code-actions` to `%s`"
           lsp-ui-sideline-show-code-actions))

;;;; ___________________________________________________________________________
;;;; Key bindings

(defun nomis/-lsp-key (s)
  (kbd (concat lsp-keymap-prefix " H-s " s)))

(defun nomis/lsp-add-key-bindings ()
  (define-key lsp-mode-map (nomis/-lsp-key "H-c")
    'nomis/lsp-toggle-lsp-ui-sideline-show-code-actions))

(add-hook 'lsp-mode-hook 'nomis/lsp-add-key-bindings)

;;;; ___________________________________________________________________________

(provide 'nomis-lsp)
