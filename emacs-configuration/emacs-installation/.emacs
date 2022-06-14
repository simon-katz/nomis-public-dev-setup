
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)

(defun load-file-relative-to-this-file (file-name)
  (let* ((directory (file-name-directory
                     (file-truename
                      (or load-file-name
                          (buffer-file-name))))))
    (load (concat directory file-name))))

(load-file-relative-to-this-file "dot-emacs")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(aggressive-indent
     auto-complete
     auto-dim-other-buffers
     cider
     cider-eval-sexp-fu
     cl-format
     cl-lib
     clj-refactor
     clojure-mode
     company
     counsel
     dash-functional
     diff-hl
     discover
     dumb-jump
     edn
     elisp-slime-nav
     exec-path-from-shell
     fill-column-indicator
     flycheck
     flycheck-clj-kondo
     flycheck-joker
     flycheck-kotlin
     fuzzy
     gnu-elpa-keyring-update
     helm
     highlight-indentation
     ht
     hydra
     ido-completing-read+
     ido-vertical-mode
     iedit
     inflections
     js2-mode
     json-mode
     kaocha-runner
     key-chord
     keycast
     kotlin-mode
     loccur
     logview
     lsp-mode
     lsp-treemacs
     lsp-ui
     magit
     markdown-mode
     multiple-cursors
     org
     org-bullets
     paredit
     parseedn
     paxedit
     pos-tip
     projectile
     queue
     rainbow-delimiters
     rainbow-blocks
     saveplace
     smartrep
     smex
     spinner
     terraform-mode
     treepy
     undo-tree
     vterm
     which-key
     yafolding
     yaml-mode
     yasnippet))
 '(safe-local-variable-values
   '((cider-clojure-cli-aliases . "-A:test")
     (cider-font-lock-max-length . 1000)
     (cider-ns-refresh-after-fn . "dev/go")
     (cider-ns-refresh-after-fn . "integrant.repl/resume")
     (cider-ns-refresh-after-fn . "user/cider-refresh--start!")
     (cider-ns-refresh-after-fn . "user/start")
     (cider-ns-refresh-before-fn . "dev/stop")
     (cider-ns-refresh-before-fn . "integrant.repl/suspend")
     (cider-ns-refresh-before-fn . "user/cider-refresh--stop!")
     (cider-ns-refresh-before-fn . "user/stop")
     (cider-show-eval-spinner)
     (checkdoc-package-keywords-flag)
     (elisp-lint-indent-specs
      (if-let* . 2)
      (when-let* . 1)
      (let* . defun)
      (nrepl-dbind-response . 2)
      (cider-save-marker . 1)
      (cider-propertize-region . 1)
      (cider-map-repls . 1)
      (cider--jack-in . 1)
      (cider--make-result-overlay . 1)
      (insert-label . defun)
      (insert-align-label . defun)
      (insert-rect . defun)
      (cl-defun . 2)
      (with-parsed-tramp-file-name . 2)
      (thread-first . 1)
      (thread-last . 1))
     (nomis/cider-cljs-offer-to-open-app-in-browser\?)
     (nomis/cider-forbid-refresh-all\? . t)
     (nomis/cider/cljs-dev-host . "localhost")
     (nomis/cider/cljs-dev-port . 3333)
     (nomis/cider/cljs-dev-port . 9000)
     (nomis/cider/cljs-dev-port . 9001)
     (nomis/cider/cljs-dev-port . 9002)
     (nomis/cider/post-interactive-eval/clj-function-name . "dev/post-interactive-eval-hook")
     (nomis/cider/post-interactive-eval/cljs-function-name . "cljs.user/post-interactive-eval-hook")
     (nomis/grep/local-ignored-directories "compiler-output" "resources/dev-and-build/icons/bootstrap-icons-1.1.0" "resources/pub-prod/cross-env")
     (nomis/grep/local-ignored-directories "_exported-site" "_pretty-printed-pages")
     (nomis/grep/local-ignored-directories "compiler-output" "resources/pub-dev/assets")
     (nomis/grep/local-ignored-directories "compiler-output")
     (nomis/grep/local-ignored-directories "giggin/public/js" "increments")
     (nomis/grep/local-ignored-directories "resources/public/js")
     (nomis/grep/local-ignored-directories)
     (nomis/grep/local-ignored-files "package-lock.json" "main.js")
     (nomis/grep/local-ignored-files "package-lock.json")
     (nomis/grep/local-ignored-files)
     (nomis/projectile/project-type . lein-test))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
