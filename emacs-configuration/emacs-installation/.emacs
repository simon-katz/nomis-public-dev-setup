
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
     clojure-mode
     company
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
     inflections
     js2-mode
     json-mode
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
     saveplace
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
     (nomis/cider-forbid-refresh-all\? . t)
     (cider-ns-refresh-after-fn . "user/cider-refresh--start!")
     (cider-ns-refresh-before-fn . "user/cider-refresh--stop!")
     (nomis/grep/local-ignored-directories "resources/public/assets" "resources/dev-and-build/icons/bootstrap-icons-1.1.0")
     (nomis/grep/local-ignored-files "package-lock.json")
     (nomis/grep/local-ignored-directories "resources/public/assets")
     (cider-ns-refresh-after-fn . "integrant.repl/resume")
     (cider-ns-refresh-before-fn . "integrant.repl/suspend")
     (cider-font-lock-max-length . 1000)
     (cider-show-eval-spinner)
     (nomis/cider/post-interactive-eval/cljs-function-name . "cljs.user/post-interactive-eval-hook")
     (nomis/cider/post-interactive-eval/clj-function-name . "dev/post-interactive-eval-hook")
     (nomis/cider/cljs-dev-port . 3333)
     (nomis/cider/cljs-dev-port . 8777)
     (nomis/cider/cljs-dev-host . "localhost")
     (nomis/grep/local-ignored-directories "giggin/public/js" "increments")
     (nomis/cider-cljs-offer-to-open-app-in-browser\?)
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
     (checkdoc-package-keywords-flag))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
