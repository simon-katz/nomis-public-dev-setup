;;;; Init stuff -- Undo tree.

(require 'undo-tree)

(global-undo-tree-mode)

(setq undo-tree-auto-save-history t)

(progn
  ;; The default for M-z is zap-to-char.  I don't need that, so...

  ;; Hah! This is the thing that causes you to lose undo history!

  ;; (define-key global-map (kbd "M-z") 'undo-tree-undo)
  ;; (define-key global-map (kbd "M-Z") 'undo-tree-redo)
  )

;;;; ___________________________________________________________________________

(provide 'nomis-undo-tree)
