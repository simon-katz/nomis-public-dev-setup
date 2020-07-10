
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
   (quote
    (blacken elpy flycheck-kotlin kotlin-mode terraform-mode flycheck dumb-jump json-mode gnu-elpa-keyring-update flycheck-clj-kondo yasnippet yaml-mode yafolding which-key undo-tree spinner smex sesman rainbow-delimiters queue projectile pos-tip paxedit parseedn org-bullets multiple-cursors markdown-mode magit key-chord js2-mode inflections ido-vertical-mode ido-completing-read+ hydra ht highlight-indentation helm fuzzy flycheck-joker fill-column-indicator exec-path-from-shell elisp-slime-nav edn discover dash-functional company clojure-mode cl-format cider-eval-sexp-fu auto-complete aggressive-indent)))
 '(safe-local-variable-values
   (quote
    ((nomis/local-grep-find-ignored-files "package-lock.json")
     (nomis/local-grep-find-ignored-directories "resources/public/assets")
     (cider-ns-refresh-after-fn . "integrant.repl/resume")
     (cider-ns-refresh-before-fn . "integrant.repl/suspend")
     (cider-font-lock-max-length . 1000)
     (cider-show-eval-spinner)
     (nomis/cider/post-interactive-eval/cljs-function-name . "cljs.user/post-interactive-eval-hook")
     (nomis/cider/post-interactive-eval/clj-function-name . "dev/post-interactive-eval-hook")
     (nomis/cider/cljs-dev-port . 3333)
     (nomis/cider/cljs-dev-port . 8777)
     (nomis/cider/cljs-dev-host . "localhost")
     (nomis/local-grep-find-ignored-directories "giggin/public/js" "increments")
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
     (checkdoc-package-keywords-flag)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
