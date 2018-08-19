;;;; Init stuff -- Clojure mode.

;;;; ___________________________________________________________________________
;;;; clojure-mode

(require 'clojure-mode)
(require 'nomis-clojure-mode-fixes)
(require 'nomis-clojure-mode-extras)

(define-key clojure-mode-map (kbd "RET") 'newline-and-indent)

(setq clojure-use-metadata-for-privacy t)

;;;; ___________________________________________________________________________
;;;; Cider
;;;; See https://github.com/clojure-emacs/cider.

(progn
  (require 'cider)
  (unless (featurep 'cider-macroexpansion)
    ;; Needed in:
    ;; - 0.8.2
    ;; - 0.9.0-snapshot (2015-02-23)
    ;; Maybe a bug.
    (require 'cider-macroexpansion)))
(require 'nomis-cider-extras)

(setq nrepl-buffer-name-separator "--")

(setq nrepl-buffer-name-show-port t)

(progn
  ;; Make C-c C-z (`cider-switch-to-repl-buffer`) display buffer in the
  ;; current window.
  (setq cider-repl-display-in-current-window t))

(setq cider-repl-pop-to-buffer-on-connect nil)


(progn
  ;; In projects, you can create a directory-local variable for
  ;; `cider-repl-history-file` in `cider-repl-mode`, as follows:
  ;; - create a file called ".dir-locals.el" with this content:
  ;;     ((cider-repl-mode
  ;;       (cider-repl-history-file . ".cider-repl-history")))
  (setq cider-repl-history-file "~/.cider-repl-history")
  (setq cider-repl-history-size 5000) ; the default is 500
  )

(setq cider-repl-history-highlight-current-entry t)
(setq cider-repl-history-highlight-inserted-item t)

(setq cider-eval-result-prefix ";; => ")

(setq cider-repl-result-prefix ";; =>\n")

(setq cider-repl-use-pretty-printing t)

(setq cider-repl-use-content-types nil) ; without this I get unhandled REPL handler exceptions when the result is an image file (2018-07-02, on upgrade to CIDER 0.17.0 (Andalucía)) -- eg `(clojure.java.io/file "/Users/simonkatz/Desktop/brave-clojure/ch00b/ch00b_Acknowledgments _ Clojure for the Brave and True_files/book-cover.jpg")` gives an exception

;; (setq cider-font-lock-dynamically t)

(when (equal (cider-version) "CIDER 0.10.0")
  ;; Fix curly braces bug.
  (add-hook 'cider-repl-mode-hook
            '(lambda ()
               (define-key cider-repl-mode-map "{" #'paredit-open-curly)
               (define-key cider-repl-mode-map "}" #'paredit-close-curly))))


(defun nomis/cider-repl-mode-c-c-c-r-replacement (arg)
  (interactive "p")
  (beep)
  (message "No, when you try to refactor I won't evaluate some huge region."))

(define-key cider-repl-mode-map (kbd "C-c C-r")
  'nomis/cider-repl-mode-c-c-c-r-replacement) ; default was `cider-eval-region` or something

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;;; Company mode for Cider

(setq cider-repl-tab-command 'company-indent-or-complete-common)

(add-hook 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
(add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion)

;;;; ___________________________________________________________________________
;;;; CIDER debug broken

;;;; See https://github.com/clojure-emacs/cider-nrepl/issues/460
;;;; - Workaround:
;;;;   `(binding [cider.nrepl.middleware.debug/*skip-breaks* (atom nil)] ...)`

;;;; ___________________________________________________________________________
;;;; Misc

(require 'cider-grimoire)

(require 'nomis-clojure-indentation)

(require 'align-cljlet)

;;;; ___________________________________________________________________________
;;;; clj-refactor

(require 'clj-refactor)
(require 'nomis-clj-refactor-deduce-hydras)

(defun nomis/cljr-add-keybindings ()
  (cljr-add-keybindings-with-prefix "C-c C-m")
  ;; (cljr-add-keybindings-with-prefix "M-R") ; keep this until I stop using it
  )

(require 'nomis-clj-refactor-fixes)
(require 'cljr-helm)

(defun nomis/setup-clj-refactor-mode ()
  (clj-refactor-mode 1)
  (yas-minor-mode 1) ; for adding require/use/import statements
  (nomis/cljr-add-keybindings))

(define-key clj-refactor-map (kbd "C-c m") 'cljr-helm)

;; (setq cljr-use-multiple-cursors nil) ; t is broken with hydra and helm -- ah, I think I have fixed it with helm at least

;; cljr-auto-sort-ns is t, but doesn't work when I type "set/"
;; - Ah, I think sorting isn't invoked, because cleaning ns requires that
;;   the file is syntactically good (and it isn't when you type that slash).

(setq cljr-magic-requires :prompt)

;;;; ___________________________________________________________________________
;;;; Hooks

(defun nomis/set-comment-column-to-zero ()
  (set (make-local-variable 'comment-column)
       0))

(let ((hook-funs-when-repl-exists `(eldoc-mode))
      (hook-funs-always `(rainbow-delimiters-mode
                          paredit-mode
                          paxedit-mode
                          nomis/set-comment-column-to-zero
                          subword-mode
                          nomis/setup-clj-refactor-mode
                          ;; aggressive-indent-mode
                          ))
      (hooks-when-repl-exists '(cider-mode-hook
                                cider-repl-mode-hook))
      (hooks-always '(clojure-mode-hook
                      cider-repl-mode-hook)))
  (cl-labels ((add-hook** (hooks functions)
                          (dolist (h hooks)
                            (dolist (f functions)
                              (add-hook h f)))))
    (add-hook** hooks-when-repl-exists
                hook-funs-when-repl-exists)
    (add-hook** hooks-always
                hook-funs-always)))

;;;; ___________________________________________________________________________
;;;; cider-eval-sexp-fu

(require 'cider-eval-sexp-fu)

(setq eval-sexp-fu-flash-duration 0.5)
(setq eval-sexp-fu-flash-error-duration 0.5)

;;;; ___________________________________________________________________________
;;;; Windows nrepl timeout

(when (equal system-type 'windows-nt)
  (setq nrepl-sync-request-timeout 30))

;;;; ___________________________________________________________________________
;;;; cljs

(setq cider-cljs-lein-repl
      "(do (use 'figwheel-sidecar.repl-api) (start-figwheel!) (cljs-repl))")

;;;; ___________________________________________________________________________

(provide 'nomis-clojure)
