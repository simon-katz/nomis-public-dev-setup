;;; nomis-lsp.el --- LSP tailoring -*- lexical-binding: t; -*-

;;;; ___________________________________________________________________________
;;;; General setup.

(setq lsp-keymap-prefix "H-q H-s")

(defun nomis/lsp-init ()
  (setq
   lsp-lens-enable                   t
   lsp-enable-symbol-highlighting    nil ; I prefer `nomis/idle-highlight`.
   lsp-ui-doc-enable                 nil ; Horrible big grey doc boxes.
   lsp-enable-indentation            nil ; Use CIDER indentation.
   lsp-ui-sideline-show-code-actions nil ; Don't show clutter! But see `nomis/lsp-toggle-lsp-ui-sideline-show-code-actions`.

   ;; lsp-completion-enable            nil ; Uncomment to use Use CIDER completion
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

(defcustom lsp-diagnostics-attributes
  `((unnecessary :foreground "grey45") ; was "gray"
    (deprecated  :strike-through t))
  "The Attributes used on the diagnostics.
List containing (tag attributes) where tag is the LSP diagnostic tag and
attributes is a `plist' containing face attributes which will be applied
on top the flycheck face for that error level."
  :type '(repeat list)
  :group 'lsp-diagnostics)

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
