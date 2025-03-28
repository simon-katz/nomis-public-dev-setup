;;; nomis-lsp.el --- LSP tailoring -*- lexical-binding: t; -*-

(require 'lsp-diagnostics)

;;;; ___________________________________________________________________________
;;;; Don't start lsp for large files

;; You have a bit of belt-and-braces at the moment:

;; (1) See `{:paths-ignore-regex [".*/test/resources/matches/.*edn"]}`
;;     in `neo-riche/.lsp/config.edn`.
;; (We're keeping this.)

;; (2) See `bug-report-clojure-lsp-ignored-edn-high-cpu` repo and any
;;     follow-up support stuff.
;; (We have a fix for clojure-lsp.)

;; (3) This hack here -- `nomis/no-lsp-for-large-files`..
;; (Commented out.)

;; We now have a fix for clojure-lsp.
;; See https://clojurians.slack.com/archives/CPABC1H61/p1685396565530999?thread_ts=1685351599.085689&cid=CPABC1H61

;; (advice-add 'lsp
;;             :around
;;             (lambda (orig-fun &rest args)
;;               (let* ((size (buffer-size)))
;;                 (if (> size (* 1024 1024 0.5))
;;                     (progn
;;                       (nomis/msg/grab-user-attention/high)
;;                       (message "Not starting lsp -- file is too large"))
;;                   (apply orig-fun args))))
;;             '((name . nomis/no-lsp-for-large-files)))

;;;; ___________________________________________________________________________
;;;; General setup.

(setq lsp-keymap-prefix "H-q H-s")

;; 2021-08-20 Suddenly this isn't needed. (Why?)
;; 2021-12-17 Hmmm, it /is/ needed. Maybe not initially, but during a session
;;            things can change so that CIDER signatures get blatted.
;;            Ah! It seems that doing `lsp-workspace-restart` causes the change.
;; 2023-09-03 It's changed. `lsp-hover` doesn't exist. Revamped to add
;;            to `lsp-after-initialize-hook`.
(defun nomis/lsp-eldoc (&rest args)
  ;; Don't blat signatures from CIDER.
  ;; I prefer CIDER signatures to LSP signatures for these reasons:
  ;; - CIDER gives better doc for `def` and `defn`. (Maybe LSP doesn't do
  ;;   macros properly.)
  ;; - When the cursor is placed after the symbol:
  ;;   - With CIDER, the signature appears.
  ;;   - With LSP, the echo area doesn't change.
  ;; - When the cursor is on an arg:
  ;;   - With CIDER, the signature for the function is shown with the formal
  ;;     arg bold.
  ;;   - With LSP, details of the arg are shown.
  ;; - When the cursor is moved away to empty space:
  ;;   - With CIDER, the echo area is cleared.
  ;;   - With LSP, the echo area doesn't change.
  ;;
  ;; Things to look at if you ever want/need to revisit this:
  ;;   lsp-eldoc-enable-hover
  ;;   cider-eldoc-display-for-symbol-at-point
  ;;   https://github.com/clojure-lsp/clojure-lsp/issues/569
  ;;   https://github.com/emacs-lsp/lsp-mode/pull/3106
  ;;

  ;; (unless (ignore-errors (cider-repls))
  ;;   (lsp-hover))
  )

(defun nomis/make-cider-first-in-eldoc-documentation-functions ()
  (dolist (b (buffer-list))
    (with-current-buffer b
      (when (member 'cider-eldoc eldoc-documentation-functions)
        (setq eldoc-documentation-functions
              (->> eldoc-documentation-functions
                   (-remove-item 'cider-eldoc)
                   (cons 'cider-eldoc)))))))

(add-hook 'lsp-after-initialize-hook
          'nomis/make-cider-first-in-eldoc-documentation-functions)

;; Stuff to maybe consider:
;; - 2021-10-20 Number of workspaces increases as time goes by.
;;   - Maybe, on startup, clear the list of workspaces. Otherwise you get
;;     loads of workspaces over time.
;; - Maybe:
;;     `(setq lsp-keep-workspace-alive t)`
;;   You need a better understanding of workspaces and sessions.
;;   See https://clojurians.slack.com/archives/CPABC1H61/p1634721926477500

(defun nomis/lsp-init ()
  ;; Non-LSP stuff. I guess these don't belong here.
  ;; See https://emacs-lsp.github.io/lsp-mode/page/performance/
  (setq gc-cons-threshold (* 100 1024 1024))
  (setq read-process-output-max (* 1024 1024))

  ;; Stuff I want:
  (setq lsp-lens-enable                   t)
  (setq lsp-enable-symbol-highlighting    t)
  (setq lsp-ui-doc-enable                 nil) ; Don't show big grey boxes.
  ;; (setq lsp-eldoc-hook                    '(nomis/lsp-eldoc))
  (setq lsp-enable-indentation            nil) ; Use CIDER indentation.
  (setq lsp-ui-sideline-show-code-actions nil) ; Don't show clutter! But see `nomis/lsp-toggle-lsp-ui-sideline-show-code-actions`.

  (setq lsp-ui-sideline-delay 0.2)

  ;; Playing with `lsp-ui-sideline-show-hover`:
  ;; - This doesn't work very well. If I have `(f1 (f2 (f3 ...)))` on a single
  ;;   line with the cursor on `f2` or `f3`, it shows info for `f1`.
  ;; (setq lsp-ui-sideline-show-hover        t)

  ;; Stuff to maybe look at:
  ;; (setq lsp-completion-enable            nil) ; Uncomment to use CIDER completion
  ;; (setq treemacs-space-between-root-nodes nil)
  ;; (setq lsp-signature-auto-activate      nil) ; TODO: What is this?
  ;; (setq lsp-enable-dap-auto-configure    nil) ; => t
  ;; (setq lsp-enable-file-watchers         nil) ; => t
  ;; (setq lsp-enable-folding               nil) ; => t
  ;; (setq lsp-enable-imenu                 nil) ; => t
  ;; (setq lsp-enable-links                 nil) ; => t
  ;; (setq lsp-enable-on-type-formatting    nil) ; => t
  ;; (setq lsp-enable-snippet               nil) ; => t
  ;; (setq lsp-enable-text-document-color   nil) ; => t
  ;; (setq lsp-enable-xref                  nil) ; => t
  )

(add-hook 'lsp-mode-hook 'nomis/lsp-init)

;;;; ___________________________________________________________________________
;;;; Tailoring of faces, needed especially for light themes.

(defun nomis/lsp-set-funky-faces ()
  ;; See https://github.com/emacs-lsp/lsp-mode/issues/2037
  (setf (alist-get 'unnecessary lsp-diagnostics-attributes)
        ;; This was simply `:foreground "gray"` I think.
        (if (nomis/dark-background-mode?)
            (list :foreground "Black"
                  :background "Grey70")
          (list :foreground "Grey45")))
  ;; Recreate dynamic faces. Relies in our hacked version of
  ;; `lsp-diagnostics--flycheck-level` to recalculate things such as:
  ;; - `lsp-flycheck-warning-unnecessary`
  ;; - `lsp-flycheck-info-unnecessary`.
  ;; We do this by causing `lsp-diagnostics-mode` initialisation to re-run.
  (let* ((a-lsp-diagnostics-mode-buffer (-find (lambda (b)
                                                 (with-current-buffer b
                                                   lsp-diagnostics-mode))
                                               (buffer-list))))
    (if a-lsp-diagnostics-mode-buffer
        (with-current-buffer a-lsp-diagnostics-mode-buffer
          (lsp-diagnostics-mode 0)
          (lsp-diagnostics-mode 1))
      (lsp-diagnostics-mode 1)
      (lsp-diagnostics-mode 0))))

(add-hook 'emacs-startup-hook
          'nomis/lsp-set-funky-faces)

(add-hook 'nomis/themes/theme-changed-hook
          'nomis/lsp-set-funky-faces)

;;;; ___________________________________________________________________________
;;;; Hack echo area messages.

;;;; :possible-open-source-contribution `nomis/show-flycheck-info`

;;;; Don't stop showing flycheck info in the echo area. Useful, and also
;;;; sometimes diagnostics are not shown in the lsp-ui-sideline.
;;;;
;;;; Don't blat signatures in the echo area with diagnostics -- append them
;;;; instead.

;;;; We depend on the fact that signatures are displayed before diagnostics.

;;;; Note: If you quit CIDER, lsp signatures are not shown unless you revert
;;;; buffers. That's nothing to do with these hacks.

(cond
 ((member (pkg-info-package-version 'lsp-ui)
          '((20210820 1331)
            (20211101 131)
            (20230811 552)
            (20241204 1808)))
  (advice-add
   'lsp-ui-sideline-mode
   :after
   (lambda (&rest _)
     (when lsp-ui-sideline-mode
       (kill-local-variable 'flycheck-display-errors-function)))
   '((name . nomis/show-flycheck-info))))

 (t
  (message-box
   "You need to fix `nomis/show-flycheck-info` for this version of `lsp-ui`.")))
;; (advice-remove 'lsp-ui-sideline-mode 'nomis/show-flycheck-info)

;;;; ___________________________________________________________________________
;;;; Additional functionality

(defun nomis/lsp-toggle-lsp-ui-sideline-show-code-actions ()
  (interactive)
  (cl-flet ((force-update-sideline-with-a-slegehammer
             ()
             (let* ((inhibit-message t))
               (lsp-ui-sideline-mode 0)
               (lsp-ui-sideline-mode 1))))
    (setq lsp-ui-sideline-show-code-actions
          (not lsp-ui-sideline-show-code-actions))
    (force-update-sideline-with-a-slegehammer))
  (message "Set `lsp-ui-sideline-show-code-actions` to `%s`"
           lsp-ui-sideline-show-code-actions))

;;;; ___________________________________________________________________________
;;;; Key bindings

(defun nomis/-lsp-key (s)
  (kbd (concat lsp-keymap-prefix "H-s " s)))

(defun nomis/lsp-add-key-bindings ()
  (define-key lsp-mode-map (nomis/-lsp-key "H-c")
    'nomis/lsp-toggle-lsp-ui-sideline-show-code-actions))

;; For some weird reason, after removing a space from `" H-s "` above, in
;; Clojure when loafing Kaocha we're getting: "Could not locate
;; kaocha/repl__init.class, kaocha/repl.clj or kaocha/repl.cljc on classpath".
;; So, for now, comment this out.
;; (add-hook 'lsp-mode-hook 'nomis/lsp-add-key-bindings)

;;;; ___________________________________________________________________________

(provide 'nomis-lsp)
