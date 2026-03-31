;;;; Init stuff -- which-key

(which-key-mode)

(with-eval-after-load 'which-key
  ;; (setopt which-key-allow-multiple-replacements t)
  (setopt which-key-sort-order 'which-key-description-order)
  (setopt which-key-replacement-alist
          (seq-map
           (lambda (rep)
             `((nil . ,(elt rep 0))
               . (nil . ,(elt rep 1))))
           '(("nomis/tree/nav\\+lineage/" "")
             ("nomis/tree/" "")
             ("nomis/scrolling/toggle-maintain-line-no-in-window"
              "toggle-maintain-line-no-in-window")
             ("-nomis/tree-key-bindings/projectile-keybinding-error"
              "ERROR (old Projectile)")))))

;;;; ___________________________________________________________________________

(provide 'nomis-which-key)
