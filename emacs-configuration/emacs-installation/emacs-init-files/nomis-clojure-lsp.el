;;; nomis-clojure-lsp.el --- Clojure LSP setup -*- lexical-binding: t; -*-

;;;; See https://emacs-lsp.github.io/lsp-mode/tutorials/clojure-guide/

;;;; ___________________________________________________________________________

(require 'nomis-lsp)

;;;; ___________________________________________________________________________

(add-hook 'clojure-mode-hook 'lsp)
(add-hook 'clojurescript-mode-hook 'lsp)
(add-hook 'clojurec-mode-hook 'lsp)

;;;; ___________________________________________________________________________

(defun nomis/clojure-lsp-init ()
  (setq lsp-clojure-workspace-dir
        (expand-file-name "~/.emacs-d-stuff/lsp-clojure-workspace"))
  (setq lsp-clojure-workspace-cache-dir
        (expand-file-name "~/.emacs-d-stuff/lsp-clojure-workspace/cache")))

(add-hook 'lsp-mode-hook 'nomis/clojure-lsp-init)

;;;; ___________________________________________________________________________

(provide 'nomis-clojure-lsp)
