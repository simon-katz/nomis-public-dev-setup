;;;; Init stuff -- Clojure mode.

;;;; ___________________________________________________________________________
;;;; clojure-mode

(require 'dash)
(require 's)
(require 'clojure-mode)
(require 'nomis-clojure-mode-fixes)
(require 'nomis-clojure-mode-extras)
(require 'nomis-clojure-lsp)

(define-key clojure-mode-map (kbd "RET") 'newline-and-indent)

(setq clojure-use-metadata-for-privacy t)

;;;; ___________________________________________________________________________
;;;; Linting

;; (require 'flycheck-joker)
;; (require 'flycheck-clj-kondo)

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

(setq cider-jump-to-pop-to-buffer-actions
      '((display-buffer-same-window)))

(require 'nomis-cider-extras-non-lexical)
(require 'nomis-cider-extras)
(require 'nomis-cider-ns-refresh)
(require 'nomis-cider-eval-clj-and-cljs)
(require 'nomis-cider-post-interactive-eval)
(require 'nomis-cider-repl-history)
(require 'nomis-cider-repl-history-hacks)

(setq nrepl-buffer-name-separator "--")

(setq nrepl-buffer-name-show-port t)

(progn
  ;; Make C-c C-z (`cider-switch-to-repl-buffer`) display buffer in the
  ;; current window.
  (setq cider-repl-display-in-current-window t))

(setq cider-repl-pop-to-buffer-on-connect nil)

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

(setq cider-ns-refresh-show-log-buffer t
      ;; I don't understand why, but the messages displayed in the echo area by
      ;; `cider-ns-refresh--handle-response` often disappear after a short time.
      ;; -- Maybe this only happens when you define one or both of
      ;;    `cider-ns-refresh-before-fn` and `cider-ns-refresh-after-fn`, as you
      ;;    do in `nomis-kafka-clj-examples`.
      ;;    -- Nope, that doesn't seem to be it.
      ;; So, we set `cider-ns-refresh-show-log-buffer`.
      )


;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;;; Company mode for Cider

(setq cider-repl-tab-command
      (lambda ()
        (let ((args (cond
                     ((version<= (company-version) "0.9.6")
                      '())
                     ((version<= "0.9.12" (company-version))
                      '(nil))
                     (t
                      (error
                       "You need to fix your `cider-repl-tab-command` tailoring.")))))
          (apply #'company-indent-or-complete-common args))))

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

(defun -nomis/cider-connect-cljs/message ()
  (let* ((msgs (list (when nomis/cider/cljs-dev-host
                       (format "use nomis/cider/cljs-dev-host (%s)"
                               nomis/cider/cljs-dev-host))
                     (when nomis/cider/cljs-dev-port
                       (format "use nomis/cider/cljs-dev-port (%s)"
                               nomis/cider/cljs-dev-port))))
         (msg (->> msgs
                   (remove nil)
                   (s-join " and "))))
    (if (string-empty-p msg)
        nil
      (format "%s?" (s-capitalize msg)))))

(defun -nomis/cider-connect-cljs/hack-params (orig-params)
  (cl-flet* ((maybe-add-param (params key value)
                              (if (or (null value)
                                      (plist-get params key))
                                  params
                                (plist-put params key value))))
    (-> (-clone orig-params)
        (maybe-add-param :host nomis/cider/cljs-dev-host)
        (maybe-add-param :port nomis/cider/cljs-dev-port))))

(defun -nomis/cider-connect-cljs/advice (orig-fun orig-params)
  (cl-flet* ((do-it (params) (funcall orig-fun params)))
    (let* ((msg (-nomis/cider-connect-cljs/message))
           (params (if (not (and msg (y-or-n-p msg)))
                       orig-params
                     (-nomis/cider-connect-cljs/hack-params orig-params))))
      (do-it params))))

(advice-add
 'cider-connect-cljs
 :around
 (lambda (orig-fun orig-params)
   (-nomis/cider-connect-cljs/advice orig-fun orig-params))
 '((name . nomis/cider-connect-cljs/hack-args)))

(advice-add
 'cider-connect-clj&cljs
 :around
 (lambda (orig-fun orig-params)
   (-nomis/cider-connect-cljs/advice orig-fun orig-params))
 '((name . nomis/cider-connect-cljs/hack-args)))

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

;; I don't like `cljr-raise-sexp` and `cljr-splice-sexp-killing-backward`, so
;; get rid of the key binding remapping:
(define-key clj-refactor-map [remap paredit-raise-sexp] nil)
(define-key clj-refactor-map [remap paredit-splice-sexp-killing-backward] nil)

;;;; ___________________________________________________________________________
;;;; Hooks

(defun nomis/set-comment-column-to-zero ()
  ;; See https://github.com/clojure-emacs/clojure-mode/issues/516#issuecomment-569336063
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
;;;; nomis/cider-clear-compilation-highlights/forced

(defun nomis/cider-clear-compilation-highlights/forced ()
  (interactive)
  (cider-clear-compilation-highlights t))

(define-key clojure-mode-map (kbd "C-M-z")
  'nomis/cider-clear-compilation-highlights/forced)

;;;; ___________________________________________________________________________
;;;; Windows nrepl timeout

(when (equal system-type 'windows-nt)
  (setq nrepl-sync-request-timeout 30))

;;;; ___________________________________________________________________________
;;;; cljs

;;;; ___________________________________________________________________________

(provide 'nomis-clojure)
