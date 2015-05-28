;;;; Init stuff -- Shell stuff.

;;;; ___________________________________________________________________________

(defun nomis-eshell-hook ()
  ;; Make eshell arrow keys better
  (cl-flet ((dk (key def)
                (define-key eshell-mode-map key def)))
    (dk [up] 'previous-line)
    (dk [down] 'next-line)
    (dk [(control p)] 'eshell-previous-matching-input-from-input)
    (dk [(control n)] 'eshell-next-matching-input-from-input)))

(add-hook 'eshell-mode-hook 'nomis-eshell-hook)

;;;; ___________________________________________________________________________

(provide 'nomis-eshell)
