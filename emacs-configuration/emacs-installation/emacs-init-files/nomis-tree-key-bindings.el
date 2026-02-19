;;; nomis-tree-key-bindings -*- lexical-binding: t -*-

;;; Key bindings

(define-key nomis/tree-mode-map (kbd "H-o H-o")
            ;; TODO: This is temporary -- just to make sure things are wired up.
            (lambda ()
              (interactive)
              (message "Ho ho!")))

;;; End

(provide 'nomis-tree-key-bindings)
