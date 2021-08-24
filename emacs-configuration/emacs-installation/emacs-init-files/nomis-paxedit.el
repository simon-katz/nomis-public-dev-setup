;;;; nomis-paxedit.el --- Paxedit stuff  -*- lexical-binding: t -*-

;;;; ___________________________________________________________________________

(require 'paxedit)
(require 'nomis-undo)

;;;; ___________________________________________________________________________
;;;; ---- Make things work for ClojureScript ----

;;;; - See:
;;;;   - https://github.com/promethial/paxedit/pull/10
;;;;   - https://github.com/promethial/paxedit/pull/10#issuecomment-816749415

(cond
 ((equal (pkg-info-package-version 'paxedit)
         '(1 1 8))
  (let* ((paxedit-implicit-clojure '(paxedit-implicit-functions-clojure
                                     paxedit-implicit-structures-clojure)))
    (dolist (mode '(clojurescript-mode clojurec-mode))
      (add-to-list 'paxedit-assoc
                   `(,mode . ,paxedit-implicit-clojure)))))
 (t
  (message "You need to revisit your paxedit hacks for ClojureScript")))

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

;;;; ___________________________________________________________________________

(provide 'nomis-paxedit)
;;; nomis-paxedit.el ends here
