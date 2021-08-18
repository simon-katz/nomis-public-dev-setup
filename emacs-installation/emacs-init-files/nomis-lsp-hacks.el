;;; nomis-lsp-hacks.el --- LSP hacks -*- lexical-binding: t; -*-

;;;; ___________________________________________________________________________

(with-eval-after-load 'lsp-diagnostics ; hack `lsp-diagnostics--flycheck-level`

  (cond
   ((member (pkg-info-package-version 'lsp-mode)
            '((20210808 2036)))

    ;; The original `lsp-diagnostics--flycheck-level` is in the
    ;; `lsp-diagnostics` package (but note that `M-.` on that takes you to a
    ;; function in `lsp-mode` so be careful).
    (defun lsp-diagnostics--flycheck-level (flycheck-level tags)
      "Generate flycheck level from the original FLYCHECK-LEVEL (e.
g. `error', `warning') and list of LSP TAGS."
      (let ((name (format "lsp-flycheck-%s-%s"
                          flycheck-level
                          (mapconcat #'symbol-name tags "-"))))
        (or (intern-soft name)
            (let* ((face (--doto (intern (format "lsp-%s-face" name))
                           (copy-face (-> flycheck-level
                                          (get 'flycheck-overlay-category)
                                          (get 'face))
                                      it)
                           (mapc (lambda (tag)
                                   (apply #'set-face-attribute it nil
                                          (cl-rest (assoc tag lsp-diagnostics-attributes))))
                                 tags)))
                   (category (--doto (intern (format "lsp-%s-category" name))
                               (setf (get it 'face) face
                                     (get it 'priority) 100)))
                   (new-level (intern name))
                   (bitmap (or (get flycheck-level 'flycheck-fringe-bitmaps)
                               (get flycheck-level 'flycheck-fringe-bitmap-double-arrow))))
              (flycheck-define-error-level new-level
                :severity (get flycheck-level 'flycheck-error-severity)
                :compilation-level (get flycheck-level 'flycheck-compilation-level)
                :overlay-category category
                :fringe-bitmap bitmap
                :fringe-face (get flycheck-level 'flycheck-fringe-face)
                :error-list-face face)
              new-level)))))

   (t
    (message-box
     "You need to fix `lsp-diagnostics--flycheck-level` for this version of lsp-ui-sideline."))))

;;;; ___________________________________________________________________________

(provide 'nomis-lsp-hacks)
