;;;; Init stuff -- Undo tree.

(require 'undo-tree)

(global-undo-tree-mode)

(progn
  ;; The default for M-z is zap-to-char.  I don't need that, so...
  (define-key global-map (kbd "M-z") 'undo-tree-undo)
  (define-key global-map (kbd "M-Z") 'undo-tree-redo))

(progn
  ;; Fix what seems to be a bug (and make it match the documentation)...
  (define-key undo-tree-visualizer-map (kbd "C-<up>") 'undo-tree-visualize-undo-to-x)
  (define-key undo-tree-visualizer-map (kbd "C-<down>") 'undo-tree-visualize-redo-to-x))

;;;; ___________________________________________________________________________

(provide 'nomis-undo-tree)
