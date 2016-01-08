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

(defvar nomis-eshell-prompt-foreground (case 1
                                         (1 "black")
                                         (2 "purple")))

(defvar nomis-eshell-prompt-background (case 6
                                         (1 "grey80")
                                         (2 "burlywood")
                                         (3 "SandyBrown")
                                         (4 "goldenrod")
                                         (5 "palegoldenrod")
                                         (6 "SkyBlue")))

(setq eshell-prompt-function
      (lambda ()
        ;; A single-line string with space-filling to give the appearance of
        ;; multiple lines in the relevant window.
        (let ((ww (window-width)))
          (cl-labels ((fill-to-window-width
                       (str)
                       (let* ((n-filler-spaces
                               (let* ((n-chars-so-far (length str)))
                                 (- ww (mod n-chars-so-far ww)))))
                         (concat str
                                 (make-string n-filler-spaces ?\s)))))
            (let* ((text (apply
                          #'concat
                          (mapcar #'fill-to-window-width
                                  (list "________________________________________"
                                        (format-time-string "%Y-%m-%d %H:%M:%S" (current-time))
                                        (eshell/pwd)))))
                   (text (concat
                          text
                          (if (= (user-uid) 0) "#" "$")
                          " ")))
              (propertize text
                          'face `(:foreground ,nomis-eshell-prompt-foreground
                                              :background ,nomis-eshell-prompt-background)))))))

;;;; ___________________________________________________________________________

(provide 'nomis-eshell)
