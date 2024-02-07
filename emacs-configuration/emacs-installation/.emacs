
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
 '(custom-enabled-themes '(nomis-extras-dark-laptop nomis-common-dark dark-laptop))
 '(custom-safe-themes
   '("9ccdf4569b55df252d5061dcf430f0dee9bb29292087e5440809c6d4a63724ce"
     "0b00a0ffad2022f05a9ff39baa79713a26a733ff40b06d71b8be5b47ed3a4909"
     "9ac11c78f208abf58e5b313a33147cbf209ad9dc9cb169bf82464b043b45ad7a"))
 '(package-selected-packages
   '(aggressive-indent
     applescript-mode
     auto-complete
     auto-dim-other-buffers
     beacon
     bind-key
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
     hide-lines
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
     lua-mode
     magit
     markdown-mode
     multiple-cursors
     nav-flash
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
     sidecar-locals
     smartrep
     smex
     spinner
     sql-indent
     sqlup-mode
     terraform-mode
     tldr
     treepy
     undo-tree
     vterm
     which-key
     yafolding
     yaml-mode
     yasnippet
     zenburn-theme))
 '(safe-local-variable-values
   '((cider-clojure-cli-aliases . "-A:test")
     (cider-font-lock-max-length . 1000)
     (cider-ns-refresh-after-fn . "dev/cider-refresh--start!")
     (cider-ns-refresh-after-fn . "dev/go")
     (cider-ns-refresh-after-fn . "integrant.repl/resume")
     (cider-ns-refresh-after-fn . "user/cider-refresh--start!")
     (cider-ns-refresh-after-fn . "user/go")
     (cider-ns-refresh-after-fn . "user/start")
     (cider-ns-refresh-before-fn . "dev/cider-refresh--stop!")
     (cider-ns-refresh-before-fn . "dev/stop")
     (cider-ns-refresh-before-fn . "integrant.repl/suspend")
     (cider-ns-refresh-before-fn . "user/cider-refresh--stop!")
     (cider-ns-refresh-before-fn . "user/halt")
     (cider-ns-refresh-before-fn . "user/stop")
     (cider-show-eval-spinner)
     (cider-test-default-exclude-selectors "with-test-containers" "needs-internet-connection")
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
     (eval define-clojure-indent
           (for-all 1))
     (eval nomis/add-to-list-local 'cider-jack-in-nrepl-middlewares "shadow.cljs.devtools.server.nrepl/middleware" t)
     (eval nomis/add-to-list-local 'grep-find-ignored-files "main.js")
     (eval org-content 999)
     (nomis/cider-cljs-offer-to-open-app-in-browser\?)
     (nomis/cider-forbid-refresh\? . t)
     (nomis/cider-forbid-refresh-all\? . t)
     (nomis/cider-use-centralised-repl-history-location\? . nil)
     (nomis/cider/cljs-dev-host . "localhost")
     (nomis/cider/cljs-dev-port . 3333)
     (nomis/cider/cljs-dev-port . 9000)
     (nomis/cider/cljs-dev-port . 9001)
     (nomis/cider/cljs-dev-port . 9002)
     (nomis/cider/post-interactive-eval/clj-function-name . "dev/post-interactive-eval-hook")
     (nomis/cider/post-interactive-eval/cljs-function-name . "cljs.user/post-interactive-eval-hook")
     (nomis/clojure-lsp-and-cider/find-definition/use-lsp\? . t)
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
