;;;; Init stuff -- Shell stuff.

;;;; ___________________________________________________________________________

(defun nomis/eshell-hook ()
  ;; Make eshell arrow keys better
  (cl-flet ((dk (key def)
                (define-key eshell-mode-map key def)))
    (dk [up] 'previous-line)
    (dk [down] 'next-line)
    (dk [(control p)] 'eshell-previous-matching-input-from-input)
    (dk [(control n)] 'eshell-next-matching-input-from-input)))

(add-hook 'eshell-mode-hook 'nomis/eshell-hook)

(defvar nomis/eshell-prompt-foreground (case 1
                                         (1 "black")
                                         (2 "purple")))

(defvar nomis/eshell-prompt-background (case 1
                                         (1 "grey80")
                                         (2 "burlywood")
                                         (3 "SandyBrown")
                                         (4 "goldenrod")
                                         (5 "palegoldenrod")
                                         (6 "SkyBlue")))

(defun nomis/fill-to-width (str width pad-char)
  (let* ((n-pad-chars (- width
                         (mod (length str)
                              width))))
    (concat str
            (make-string n-pad-chars pad-char))))

(defvar nomis/prompt-strings-fun
  (lambda ()
    (list "________________________________________"
          (format-time-string "%Y-%m-%d %H:%M:%S"
                              (current-time))
          (eshell/pwd))))

(setq eshell-prompt-function
      (lambda ()
        ;; A single-line string with space-filling to give the appearance of
        ;; multiple lines in the relevant window.
        (let* ((fill-fun #'(lambda (str)
                             (nomis/fill-to-width str (window-width) ?\s)))
               (strings-to-fill (funcall nomis/prompt-strings-fun))
               (filled-strings (apply #'concat
                                      (mapcar fill-fun strings-to-fill)))
               (prompt (concat filled-strings
                               (if (= (user-uid) 0) "#" "$")
                               " ")))
          (propertize prompt
                      'face (list :foreground nomis/eshell-prompt-foreground
                                  :background nomis/eshell-prompt-background)))))

;;;; ___________________________________________________________________________

(provide 'nomis-eshell)
