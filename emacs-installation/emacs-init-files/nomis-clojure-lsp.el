;;; nomis-clojure-lsp.el --- Clojure LSP setup -*- lexical-binding: t; -*-

;;;; See https://emacs-lsp.github.io/lsp-mode/tutorials/clojure-guide/

(setq lsp-keymap-prefix "H-q H-s")

(defun nomis/lsp-init ()
  (setq
   lsp-lens-enable                  t
   lsp-enable-symbol-highlighting   nil ; I prefer `nomis/idle-highlight`.
   lsp-ui-doc-enable                nil ; Horrible big grey doc boxes.
   lsp-enable-indentation           nil ; Use CIDER indentation.
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

(add-hook 'clojure-mode-hook 'lsp)
(add-hook 'clojurescript-mode-hook 'lsp)
(add-hook 'clojurec-mode-hook 'lsp)

(defface lsp-ui-sideline-code-action
  '((((background light))
     :foreground "gray95"
     :background "springgreen4")
    (t :foreground "yellow"))
  "Face used to highlight code action text."
  :group 'lsp-ui-sideline)

(provide 'nomis-clojure-lsp)