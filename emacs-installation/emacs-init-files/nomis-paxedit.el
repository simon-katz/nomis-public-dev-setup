;;;; Init stuff -- Paredit.

;;;; ___________________________________________________________________________

(require 'paxedit)

(eval-after-load "paxedit"
  '(progn
     (define-key paxedit-mode-map (kbd "M-<right>") 'paxedit-transpose-forward)
     (define-key paxedit-mode-map (kbd "M-<left>") 'paxedit-transpose-backward)
     ;; (define-key paxedit-mode-map (kbd "M-<up>") 'paxedit-backward-up)
     ;; (define-key paxedit-mode-map (kbd "M-<down>") 'paxedit-backward-end)
     ;; (define-key paxedit-mode-map (kbd "M-b") 'paxedit-previous-symbol)
     ;; (define-key paxedit-mode-map (kbd "M-f") 'paxedit-next-symbol)
     ;; (define-key paxedit-mode-map (kbd "C-%") 'paxedit-copy)
     ;; (define-key paxedit-mode-map (kbd "C-&") 'paxedit-kill)
     (define-key paxedit-mode-map (kbd "C-*") 'paxedit-delete)
     ;; (define-key paxedit-mode-map (kbd "C-^") 'paxedit-sexp-raise)
     ;; ;; Symbol backward/forward kill
     ;; (define-key paxedit-mode-map (kbd "C-w") 'paxedit-backward-kill)
     ;; (define-key paxedit-mode-map (kbd "M-w") 'paxedit-forward-kill)
     ;; ;; Symbol manipulation
     ;; (define-key paxedit-mode-map (kbd "M-u") 'paxedit-symbol-change-case)
     ;; (define-key paxedit-mode-map (kbd "C-@") 'paxedit-symbol-copy)
     ;; (define-key paxedit-mode-map (kbd "C-#") 'paxedit-symbol-kill)
     ))

(provide 'nomis-paxedit)
