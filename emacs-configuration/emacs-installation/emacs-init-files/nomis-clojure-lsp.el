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
;;;; ---- nomis/clojure-lsp-and-cider/find-definition ----

;; Not needed now that `cider-xref-fn-depth` has been introduced.

;; (defun nomis/clojure-lsp-and-cider/find-definition ()
;;   "Try to find definition of thing at point using CIDER; if no luck try using LSP."
;;   (interactive)
;;   (cl-flet ((buffer-and-point () (list (current-buffer) (point))))
;;     (let* ((old-bap (buffer-and-point)))
;;       (when (cider-repls)
;;         (cider-find-var))
;;       (let* ((new-bap (buffer-and-point)))
;;         (when (equal old-bap new-bap)
;;           (beep)
;;           (message "Couldn't find definition using CIDER -- trying LSP")
;;           (lsp-find-definition))))))

;; (with-eval-after-load 'cider
;;   (with-eval-after-load 'lsp-mode
;;     (cond
;;      ((member (list (pkg-info-package-version 'cider)
;;                     (pkg-info-package-version 'lsp-mode) ; Hmmm, I guess you really want to depend on the version of `clojure-lsp` executable installed on the system.
;;                     )
;;               '(((20210909 1011) (20210821 1359))))

;;       (dolist (m (list clojure-mode-map
;;                        cider-mode-map
;;                        clojurec-mode-map
;;                        clojurescript-mode-map))
;;         (define-key m (kbd "M-.") 'nomis/clojure-lsp-and-cider/find-definition)))

;;      (t
;;       (message-box
;;        "You need to fix `nomis/clojure-lsp-and-cider/find-definition` for this version of CIDER and LSP.")))))

;;;; ___________________________________________________________________________

(provide 'nomis-clojure-lsp)
