;;; nomis-clojure-lsp.el --- Clojure LSP setup -*- lexical-binding: t; -*-

;;;; See https://emacs-lsp.github.io/lsp-mode/tutorials/clojure-guide/

;;;; ___________________________________________________________________________

(require 'nomis-lsp)

;;;; ___________________________________________________________________________

(add-hook 'clojure-mode-hook 'lsp)
;; Might also want (see https://emacs-lsp.github.io/lsp-mode/tutorials/clojure-guide/):
;; (add-hook 'clojurescript-mode-hook 'lsp)
;; (add-hook 'clojurec-mode-hook 'lsp)

;;;; ___________________________________________________________________________

(defun nomis/clojure-lsp-init ()
  (setq lsp-clojure-workspace-dir
        (expand-file-name "~/.emacs-d-stuff/lsp-clojure-workspace"))
  (setq lsp-clojure-workspace-cache-dir
        (expand-file-name "~/.emacs-d-stuff/lsp-clojure-workspace/cache")))

(add-hook 'lsp-mode-hook 'nomis/clojure-lsp-init)

;;;; ___________________________________________________________________________
;;;; ---- nomis/clojure-lsp-and-cider/find-definition ----

;; We had commented this out because `cider-xref-fn-depth` has been introduced
;; (meaning CIDER is used in preference to LSP), but we've re-introduced it
;; because LSP lets us find clojure.spec definitions for symbols.

(defvar nomis/clojure-lsp-and-cider/find-definition/use-lsp? nil)

(defun nomis/clojure-lsp-and-cider/find-definition ()
  "Try to find definition of thing at point.
 If the thing at point is a keyword, use LSP. If the thing at
 point is a not a keyword, try using CIDER; if no luck with
 CIDER, use LSP."
  (interactive)
  (if (or nomis/clojure-lsp-and-cider/find-definition/use-lsp?
          (ignore-errors (s-starts-with? ":" (symbol-name (symbol-at-point)))))
      (lsp-find-definition)
    (cl-flet ((buffer-and-point () (list (current-buffer) (point))))
      (let* ((old-bap (buffer-and-point)))
        (when (cider-repls)
          (cider-find-var))
        (let* ((new-bap (buffer-and-point)))
          (when (equal old-bap new-bap)
            (beep)
            (message "Couldn't find definition using CIDER -- trying LSP")
            (lsp-find-definition)))))))

(defun nomis/clojure-lsp-and-cider/find-definition-v2 ()
  "Try to find definition of thing at point.
 First try lsp; if that gives an error try CIDER."
  (interactive)
  (cl-flet ((buffer-and-point () (list (current-buffer) (point))))
    (let* ((initial-bap (buffer-and-point)))
      (unwind-protect
          ;; Try lsp:
          (lsp-find-definition)
        (when (equal initial-bap (buffer-and-point))
          ;; lsp didn't find the definition, so try CIDER:
          (when (cider-repls)
            ;; Use `run-at-time` so that any lsp-produced exception is dealt
            ;; with first.
            (run-at-time 0 nil (lambda () (cider-find-var nil)))))))))

(with-eval-after-load 'cider
  (with-eval-after-load 'lsp-mode
    (cond
     ((member (list (pkg-info-package-version 'cider)
                    (pkg-info-package-version 'lsp-mode) ; Hmmm, I guess you really want to depend on the version of `clojure-lsp` executable installed on the system.
                    )
              '(((20210909 1011) (20210821 1359))
                ((20210929 1032) (20210821 1359))
                ((20211105 708)  (20211103 1331))
                ((20220405 1216) (20211103 1331))
                ((20220830 500)  (20211103 1331))))

      (dolist (m (list clojure-mode-map
                       cider-mode-map
                       clojurec-mode-map
                       clojurescript-mode-map))
        (define-key m (kbd "M-.") 'nomis/clojure-lsp-and-cider/find-definition)))

     ((and (version-list-<= '(20230518 55)
                            (pkg-info-package-version 'cider))
           (version-list-<= (pkg-info-package-version 'cider)
                            '(20250429 0)))
      ;; Works now (it finds a Clojure Spec keyword).
      )

     (t
      (dolist (m (list clojure-mode-map
                       cider-mode-map
                       clojurec-mode-map
                       clojurescript-mode-map))
        (define-key m (kbd "M-.") 'nomis/clojure-lsp-and-cider/find-definition-v2))))))

;;;; ___________________________________________________________________________

(provide 'nomis-clojure-lsp)
