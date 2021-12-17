;;; nomis-lsp.el --- LSP tailoring -*- lexical-binding: t; -*-

;;;; ___________________________________________________________________________
;;;; General setup.

(setq lsp-keymap-prefix "H-q H-s")

;; 2021-08-20 Suddenly this isn't needed. (Why?)
;; 2021-12-17 Hmmm, it /is/ needed. Maybe not initially, but during a session
;;            things can change so that CIDER signatures get blatted.
;;            Ah! It seems that doing `lsp-restart-workspace` causes the change.
(defun nomis/lsp-eldoc ()
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
  (unless (ignore-errors (cider-repls))
    (lsp-hover)))

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
  (setq lsp-eldoc-hook                    '(nomis/lsp-eldoc))
  (setq lsp-enable-indentation            nil) ; Use CIDER indentation.
  (setq lsp-ui-sideline-show-code-actions nil) ; Don't show clutter! But see `nomis/lsp-toggle-lsp-ui-sideline-show-code-actions`.

  (setq lsp-ui-sideline-delay 2)

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

(defconst nomis/lsp-symbol-highlight-color "#ffff00")

(face-spec-set
 'lsp-face-highlight-textual
 ;; c.f. tailoring of `nomis/idle-highlight-muted`.
 `((((min-colors 88) (background dark))
    (:background ,nomis/lsp-symbol-highlight-color :foreground "black"))
   (((background dark)) (:background ,nomis/lsp-symbol-highlight-color :foreground "black"))
   (((min-colors 88)) (:background ,nomis/lsp-symbol-highlight-color))
   (t (:background ,nomis/lsp-symbol-highlight-color))))

(face-spec-set
 'lsp-ui-sideline-code-action
 '((t
    :foreground "gray95"
    :background "springgreen4")))

(face-spec-set
 'lsp-ui-sideline-global
 '((t
    :foreground "black"
    :background "LemonChiffon1")))

(with-eval-after-load 'lsp-diagnostics
  (setf (alist-get 'unnecessary lsp-diagnostics-attributes)
        '(:foreground "grey45") ; was "gray"
        ))

;;;; ___________________________________________________________________________
;;;; Hack echo area messages.

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
            (20211101 131)))
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

(with-eval-after-load 'lsp-mode ; nomis/show-lsp-errors-elsewhere
  (cond
   ((member (pkg-info-package-version 'lsp-mode)
            '((20210821 1359)
              (20211103 1331)))
    (advice-add
     'flycheck-display-error-messages
     :around
     (lambda (orig-fun errs)
       (let* ((message-1 (current-message)))
         (prog1 (funcall orig-fun errs)
           (let* ((message-2 (current-message)))
             (when message-1
               (if message-2
                   (message "%s\n%s" message-1 message-2)
                 (message "%s" message-1)))))))
     '((name . nomis/show-lsp-errors-elsewhere))))
   (t
    (message-box
     "You need to fix `nomis/show-lsp-errors-elsewhere` for this version of `lsp`."))))
;; (advice-remove 'flycheck-display-error-messages 'nomis/show-lsp-errors-elsewhere)

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
  (kbd (concat lsp-keymap-prefix " H-s " s)))

(defun nomis/lsp-add-key-bindings ()
  (define-key lsp-mode-map (nomis/-lsp-key "H-c")
    'nomis/lsp-toggle-lsp-ui-sideline-show-code-actions))

(add-hook 'lsp-mode-hook 'nomis/lsp-add-key-bindings)

;;;; ___________________________________________________________________________

(provide 'nomis-lsp)
