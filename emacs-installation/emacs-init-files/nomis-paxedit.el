;;;; Init stuff -- Paxedit.

;;;; ___________________________________________________________________________

(require 'paxedit)
(require 'nomis-undo)

;;;; ___________________________________________________________________________
;;;; ---- nomis/paxedit-transpose-backward ----

(defun nomis/paxedit-transpose-backward (&optional n)
  "A replacement for PAXEDIT-TRANSPOSE-BACKWARD that restores
point when undone. (It seems that PAXEDIT-TRANSPOSE-BACKWARD
doesn't restore point when undone in Clojure maps and binding
vectors that have two entries. I haven't looked into why.)"
  (interactive "p")
  (nomis/with-atomic-undo
    ;; Do something that makes the undo system remember where we are:
    (insert "x")
    (delete-char -1)
    ;; Do what we really want to do:
    (paxedit-transpose-backward n)))

;;;; ___________________________________________________________________________
;;;; ---- Key bindings ----

(eval-after-load "paxedit"
  '(progn
     (define-key paxedit-mode-map (kbd "M-<right>") 'paxedit-transpose-forward)
     (define-key paxedit-mode-map (kbd "M-<left>")  'nomis/paxedit-transpose-backward)
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
