;;; nomis-lsp.el --- LSP tailoring -*- lexical-binding: t; -*-

;;;; ___________________________________________________________________________
;;;; General setup.

(setq lsp-keymap-prefix "H-q H-s")

(defun nomis/lsp-init ()
  (setq
   lsp-lens-enable                   t
   lsp-enable-symbol-highlighting    t
   lsp-ui-doc-enable                 nil ; Don't show horrible big grey boxes.
   lsp-eldoc-hook                    nil ; Don't blat signatures from CIDER.
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

(advice-add 'lsp-ui-sideline-mode
            :after
            (lambda (&rest _)
              (when lsp-ui-sideline-mode
                (kill-local-variable 'flycheck-display-errors-function)))
            '((name . nomis/show-flycheck-info)))

(advice-add 'flycheck-display-error-messages
            :around
            (lambda (orig-fun &rest args)
              (run-at-time 0
                           nil
                           (lambda () (apply orig-fun args))))
            '((name . nomis/lsp-avoid-slow-sideline-update)))

;;;; ___________________________________________________________________________
;;;; Additional functionality.

(defun nomis/lsp-toggle-lsp-ui-sideline-show-code-actions ()
  (interactive)
  (cl-flet ((force-update-sideline-with-a-slegehammer
             ()
             (let* ((inhibit-message t))
               (lsp-ui-sideline-mode 0)
               (lsp-ui-sideline-mode 1))
             ;; Why does this take so long too update the UI with M-x, but is
             ;; fast when invoked using a keyboard binding?
             ;; Note that `M-x lsp-ui-sideline-mode` is slow to update too.
             ))
    (setq lsp-ui-sideline-show-code-actions
          (not lsp-ui-sideline-show-code-actions))
    (force-update-sideline-with-a-slegehammer))
  (message "Set `lsp-ui-sideline-show-code-actions` to `%s` -- Use the keyboard binding for faster UI update!"
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
