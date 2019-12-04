;;;; Init stuff -- Clojure mode.

;;;; ___________________________________________________________________________
;;;; clojure-mode

(require 'clojure-mode)
(require 'nomis-clojure-mode-fixes)
(require 'nomis-clojure-mode-extras)

(define-key clojure-mode-map (kbd "RET") 'newline-and-indent)

(setq clojure-use-metadata-for-privacy t)

;;;; ___________________________________________________________________________
;;;; Linting

;; (require 'flycheck-joker)
(require 'flycheck-clj-kondo)

;;;; https://github.com/borkdude/flycheck-clj-kondo says "To set up multiple
;;;; linters, e.g. in combination with flycheck-joker, add this after you
;;;; required the linter packages:"

;; (dolist (checker '(clj-kondo-clj
;;                    clj-kondo-cljs
;;                    clj-kondo-cljc
;;                    clj-kondo-edn))
;;   (setq flycheck-checkers (cons checker (delq checker flycheck-checkers))))

;;;; https://github.com/borkdude/flycheck-clj-kondo says "This ensures that the
;;;; clj-kondo checkers are the first ones in the flycheck-checkers list. This
;;;; is needed to make the chain work. To create the chain, also add the
;;;; following code:"

;; (dolist (checkers '((clj-kondo-clj . clojure-joker)
;;                     (clj-kondo-cljs . clojurescript-joker)
;;                     (clj-kondo-cljc . clojure-joker)
;;                     (clj-kondo-edn . edn-joker)))
;;   (flycheck-add-next-checker (car checkers) (cons 'error (cdr checkers))))

(progn ; Include checker name in flycheck messages in echo area.
  (cond
   ((member (flycheck-version)
            '("31"))
    (defvar *-nomis/add-checker-name-to-flycheck-message?* nil)
    (advice-add 'flycheck-display-error-messages
                :around
                (lambda (orig-fun &rest args)
                  (let* ((*-nomis/add-checker-name-to-flycheck-message?* t))
                    (apply orig-fun args)))
                '((name . nomis/add-checker-name-to-flycheck-message*)))
    (advice-add 'flycheck-error-format-message-and-id
                :around
                (lambda (orig-fun err)
                  (let* ((raw-value (funcall orig-fun err)))
                    (if *-nomis/add-checker-name-to-flycheck-message?*
                        (let* ((error-checker-info
                                (concat "("
                                        (symbol-name (flycheck-error-checker err))
                                        ")")))
                          (put-text-property 0
                                             (length error-checker-info)
                                             'face
                                             '(foreground-color . "blue")
                                             error-checker-info)
                          (concat error-checker-info
                                  " "
                                  raw-value))
                      raw-value)))
                '((name . nomis/add-checker-name-to-flycheck-message*))))
   (t
    (message-box
     "You need to fix our 'include checker name in flycheck messages' for this version of flycheck."))))


;;;; ___________________________________________________________________________
;;;; Cider
;;;; See https://github.com/clojure-emacs/cider.

(progn
  (require 'cider)
  (require 'cider-autoloads) ; needed when you have your own local CIDER repo
  (unless (featurep 'cider-macroexpansion)
    ;; Needed in:
    ;; - 0.8.2
    ;; - 0.9.0-snapshot (2015-02-23)
    ;; Maybe a bug.
    (require 'cider-macroexpansion)))

(require 'nomis-cider-extras)
(require 'nomis-cider-eval-clj-and-cljs)
(require 'nomis-cider-post-interactive-eval)

(setq nrepl-buffer-name-separator "--")

(setq nrepl-buffer-name-show-port t)

(progn
  ;; Make C-c C-z (`cider-switch-to-repl-buffer`) display buffer in the
  ;; current window.
  (setq cider-repl-display-in-current-window t))

(setq cider-repl-pop-to-buffer-on-connect nil)


(progn
  (progn ; set `cider-repl-history-file`
    ;; Old notes:
    ;;   In projects, you can create a directory-local variable for
    ;;   `cider-repl-history-file` in `cider-repl-mode`, as follows:
    ;;   - create a file called ".dir-locals.el" with this content:
    ;;       ((cider-repl-mode
    ;;         (cider-repl-history-file . ".cider-repl-history")))
    ;; New thoughts on 2019-10-03:
    ;; - If you use ".cider-repl-history" rather than "~/.cider-repl-history"
    ;;   all seems OK, despite confusing (perhaps outdated) comments about using
    ;;   dir locals at the following places:
    ;;   http://blogish.nomistech.com/separate-cider-repl-histories/
    ;;   https://github.com/clojure-emacs/cider/issues/2353#issuecomment-401126801
    ;;   https://github.com/clojure-emacs/cider/issues/37#issuecomment-7312083
    ;; - So not this:
    ;;   (setq cider-repl-history-file "~/.cider-repl-history")
    ;; But this:
    (setq cider-repl-history-file ".cider-repl-history"))
  (setq cider-repl-history-size 5000) ; the default is 500
  )

(setq cider-repl-history-highlight-current-entry t)
(setq cider-repl-history-highlight-inserted-item t)

(setq clojure-toplevel-inside-comment-form t)

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
;;;; CIDER Connections

(defvar nomis/cider/cljs-dev-host nil)
(defvar nomis/cider/cljs-dev-port nil)

(defun nomis/cider-connect-cljs ()
  (interactive)
  (if (and nomis/cider/cljs-dev-host
           nomis/cider/cljs-dev-port)
      (cider-connect-cljs (list :host nomis/cider/cljs-dev-host
                                :port nomis/cider/cljs-dev-port))
    (cider-connect-cljs)))

;;;; ___________________________________________________________________________
;;;; Misc

(require 'nomis-clojure-indentation)

(require 'align-cljlet)

;;;; ___________________________________________________________________________
;;;; clj-refactor

(require 'clj-refactor)

(defun nomis/setup-clj-refactor-mode ()
  (clj-refactor-mode 1)
  (yas-minor-mode 1) ; for adding require/use/import statements
  ;; This choice of keybinding leaves cider-macroexpand-1 unbound
  (cljr-add-keybindings-with-prefix "C-c C-m"))

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
                          flycheck-mode))
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

;;;; ___________________________________________________________________________

(provide 'nomis-clojure)
