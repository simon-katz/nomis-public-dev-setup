;;;; Init stuff -- Clojure mode.

;;;; ___________________________________________________________________________

(require 'clojure-mode)

(progn
  (require 'cider)
  (unless (featurep 'cider-macroexpansion)
    ;; Needed in:
    ;; - 0.8.2
    ;; - 0.9.0-snapshot (2015-02-23)
    ;; Maybe a bug.
    (require 'cider-macroexpansion)))

(require 'cider-grimoire)

(require 'nomis-clojure-indentation)
(require 'nomis-cider-extras)

(require 'align-cljlet)

;;;; ___________________________________________________________________________
;;;; clj-refactor

(require 'clj-refactor)

(defun nomis-setup-clj-refactor-mode ()
  (clj-refactor-mode 1)
  (cljr-add-keybindings-with-prefix "M-R"))

(add-hook 'clojure-mode-hook (lambda () (yas/minor-mode 1)))

;;;; ___________________________________________________________________________

(defvar nomis-clojure-mode-hook-functions
  `(rainbow-delimiters-mode
    paredit-mode
    ,(lambda () (set (make-local-variable 'comment-column) 0))
    subword-mode
    nomis-setup-clj-refactor-mode))

(dolist (hook '(clojure-mode-hook
                cider-repl-mode-hook))
  (dolist (hook-fun nomis-clojure-mode-hook-functions)
    (add-hook hook hook-fun)))

(define-key clojure-mode-map (kbd "RET") 'newline-and-indent)

;;;; ___________________________________________________________________________
;;;; Cider
;;;; See https://github.com/clojure-emacs/cider.

(setq nrepl-buffer-name-separator "--")

;; (setq nrepl-buffer-name-show-port t)

(setq cider-repl-display-in-current-window t)
(setq cider-repl-pop-to-buffer-on-connect nil)

(setq cider-repl-history-file "~/.cider-repl-history")
(setq cider-repl-history-size 5000) ; the default is 500

(setq cider-repl-use-clojure-font-lock t)

(add-hook 'cider-mode-hook 'eldoc-mode)

(setq cider-interactive-eval-result-prefix ";; => ")

(setq cider-font-lock-dynamically t)

(when (equal (cider-version) "CIDER 0.10.0")
  ;; Fix curly braces bug.
  (add-hook 'cider-repl-mode-hook
            '(lambda ()
               (define-key cider-repl-mode-map "{" #'paredit-open-curly)
               (define-key cider-repl-mode-map "}" #'paredit-close-curly))))

;;;; ___________________________________________________________________________
;;;; cider-eval-sexp-fu

(require 'cider-eval-sexp-fu)

(setq eval-sexp-fu-flash-duration 0.5)
(setq eval-sexp-fu-flash-error-duration 0.5)

;;;; ___________________________________________________________________________

(when (equal system-type 'windows-nt)
  (setq nrepl-sync-request-timeout 30))

;;;; ___________________________________________________________________________

(provide 'nomis-clojure)
