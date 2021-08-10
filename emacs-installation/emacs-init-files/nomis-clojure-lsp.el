;;; nomis-clojure-lsp.el --- Clojure LSP setup -*- lexical-binding: t; -*-

;;;; See https://emacs-lsp.github.io/lsp-mode/tutorials/clojure-guide/

(setq lsp-keymap-prefix "H-q H-s")

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
