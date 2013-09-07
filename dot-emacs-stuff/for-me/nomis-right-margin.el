;;;; Right margin

;;;; ___________________________________________________________________________

(defvar nomis-right-margin-column 80)

(require 'nomis-whitespace)
(require 'nomis-fci-mode)

(define-minor-mode nomis-right-margin-mode
  "A minor mode that combines whitespace-mode and fci-mode."
  nil nil nil
  (if nomis-right-margin-mode
      (progn
        (whitespace-mode)
        ;; (fci-mode) ; See `nomis-fci-mode-issues`.
        )
    (progn
      (whitespace-mode-off)
      ;; (fci-mode-off) ; See `nomis-fci-mode-issues`.
      )))

(defun turn-on-nomis-right-margin-mode ()
  "Turn on nomis-right-margin-mode unconditionally."
  (interactive)
  (nomis-right-margin-mode 1))

(defun turn-off-nomis-right-margin-mode ()
  "Turn off nomis-right-margin-mode unconditionally."
  (interactive)
  (nomis-right-margin-mode 0))

;;;; ___________________________________________________________________________

(add-hook 'text-mode-hook 'turn-on-nomis-right-margin-mode)
(add-hook 'prog-mode-hook 'turn-on-nomis-right-margin-mode)

(add-hook 'org-mode-hook 'turn-off-nomis-right-margin-mode)

;;;; ___________________________________________________________________________

(provide 'nomis-right-margin)
