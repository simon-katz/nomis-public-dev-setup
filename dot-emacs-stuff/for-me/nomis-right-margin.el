;;;; Right margin

(defvar nomis-right-margin 80)

(require 'nomis-whitespace)
(require 'nomis-fci-mode)

(defun nomis-right-margin-mode ()
  (nomis-whitespace-mode-reinstating-blatted-faces)
  (fci-mode))

(defun nomis-right-margin-mode-off ()
  (whitespace-mode-off)
  (fci-mode-off))

(add-hook 'text-mode-hook 'nomis-right-margin-mode)
(add-hook 'prog-mode-hook 'nomis-right-margin-mode)

(add-hook 'org-mode-hook 'nomis-right-margin-mode-off)
(add-hook 'emacs-lisp-mode-hook 'fci-mode-off)

(provide 'nomis-right-margin)
