;;; nomis-tree -*- lexical-binding: t -*-

;;; nomis/tree-mode

(defvar nomis/tree-mode-map
  (make-sparse-keymap)
  "Keymap for `nomis/tree-mode'.")

(define-minor-mode nomis/tree-mode
  "Nomis-Tree Minor Mode"
  :group 'nomis/tree
  :keymap nomis/tree-mode-map
  ;; This minor mode exists only as a place to define key bindings, so we don't
  ;; need to do anything when turning on and off.
  )

;;; Turn on the mode

(add-hook 'org-mode-hook 'nomis/tree-mode)
(add-hook 'outline-minor-mode-hook 'nomis/tree-mode)

;; TODO: Do we want this? Or should we be testing whether `hs-minor-mode` is
;;       active when deciding what to do?
;; (add-hook 'hs-minor-mode-hook 'nomis/tree-mode)

;;; Utilities

(defun -nomis/tree/outline-mode? ()
  (or (eq major-mode 'outline-mode)
      outline-minor-mode))

(defun -nomis/tree/org-mode? ()
  (eq major-mode 'org-mode))

(defun -nomis/tree/mode ()
  (cond ((-nomis/tree/outline-mode?)
         :outline)
        ((-nomis/tree/org-mode?)
         :org)
        (t
         (error "Unexpected: None of outline-mode, outline-minor-mode or org-mode is active"))))

;;; Search heading text

(cl-defgeneric nomis/tree/search-heading-text--aux (k))
(cl-defgeneric nomis/tree/search-heading-text-again--aux (k))

(defun nomis/tree/search-heading-text ()
  (interactive)
  (nomis/tree/search-heading-text--aux (-nomis/tree/mode)))

(defun nomis/tree/search-heading-text-again ()
  (interactive)
  (nomis/tree/search-heading-text-again--aux (-nomis/tree/mode)))

;;; Visibility span

(cl-defgeneric nomis/tree/visibility-span/less--aux (k))
(cl-defgeneric nomis/tree/visibility-span/more--aux (k))
(cl-defgeneric nomis/tree/visibility-span/set-min--aux (k))
(cl-defgeneric nomis/tree/visibility-span/set-max--aux (k))

(defun nomis/tree/visibility-span/less ()
  (interactive)
  (nomis/tree/visibility-span/less--aux (-nomis/tree/mode)))

(defun nomis/tree/visibility-span/more ()
  (interactive)
  (nomis/tree/visibility-span/more--aux (-nomis/tree/mode)))

(defun nomis/tree/visibility-span/set-min ()
  (interactive)
  (nomis/tree/visibility-span/set-min--aux (-nomis/tree/mode)))

(defun nomis/tree/visibility-span/set-max ()
  (interactive)
  (nomis/tree/visibility-span/set-max--aux (-nomis/tree/mode)))

;;; nomis/tree/show-tree-only and nomis/tree/max-lineage

(cl-defgeneric nomis/tree/show-tree-only--aux (k))
(cl-defgeneric nomis/tree/max-lineage--aux (k))

(defun nomis/tree/show-tree-only ()
  (interactive)
  (nomis/tree/show-tree-only--aux (-nomis/tree/mode)))

(defun nomis/tree/max-lineage ()
  (interactive)
  (nomis/tree/max-lineage--aux (-nomis/tree/mode)))

;;; nomis/tree/set-step-n-levels-to-show

(cl-defgeneric nomis/tree/set-step-n-levels-to-show--aux (k n))

(defun nomis/tree/set-step-n-levels-to-show (n)
  (interactive "P")
  (nomis/tree/set-step-n-levels-to-show--aux (-nomis/tree/mode) n))

;;; End

(provide 'nomis-tree)
