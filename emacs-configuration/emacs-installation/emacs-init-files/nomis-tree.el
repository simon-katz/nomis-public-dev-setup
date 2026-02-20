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

(defun -not-supported ()
  (error "Not supported"))

;;; Visibility span

(defun nomis/tree/visibility-span/less ()
  (interactive)
  (cl-ecase (-nomis/tree/mode)
    (:outline (-not-supported))
    (:org     (nomis/org-visibility-span/less))))

(defun nomis/tree/visibility-span/more ()
  (interactive)
  (cl-ecase (-nomis/tree/mode)
    (:outline (-not-supported))
    (:org     (nomis/org-visibility-span/more))))

(defun nomis/tree/visibility-span/set-min ()
  (interactive)
  (cl-ecase (-nomis/tree/mode)
    (:outline (-not-supported))
    (:org     (nomis/org-visibility-span/set-min))))

(defun nomis/tree/visibility-span/set-max ()
  (interactive)
  (cl-ecase (-nomis/tree/mode)
    (:outline (nomis/outline/visibility-span/set-max))
    (:org     (nomis/org-visibility-span/set-max))))

;;; End

(provide 'nomis-tree)
