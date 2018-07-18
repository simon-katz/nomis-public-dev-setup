;;;; Init stuff -- Keyboard scrolling and movement.

;;;; ___________________________________________________________________________

;;;; See https://www.gnu.org/software/emacs/manual/html_node/emacs/Horizontal-Scrolling.html

(setq hscroll-margin 0)
(setq hscroll-step 1)

;;;; ___________________________________________________________________________

(defun nomis/scroll-down-in-place (&optional n)
  "Like scroll-down-line, except move point to keep it at the same position
in the display."
  (interactive "p")
  (scroll-down-line n)
  (previous-line n))

(defun nomis/scroll-up-in-place (&optional n)
  "Like scroll-up-line, except move point to keep it at the same position
in the display."
  (interactive "p")
  (next-line n)
  (scroll-up-line n))

(define-key global-map (kbd "C-M-S-<up>")   'scroll-down-line)
(define-key global-map (kbd "C-M-S-<down>") 'scroll-up-line)
(define-key global-map (kbd "C-S-<up>")     'nomis/scroll-down-in-place)
(define-key global-map (kbd "C-S-<down>")   'nomis/scroll-up-in-place)

;;;; ___________________________________________________________________________

(defconst go-left-right-amount 40)

(defun nomis/go-right-lots ()
  (interactive)
  (let ((current (point))
        (line-end (line-end-position)))
    (if (= current line-end)
        (beep)
      (let ((new (min line-end
                      (+ current go-left-right-amount))))
        (goto-char new)))))

(defun nomis/go-left-lots ()
  (interactive)
  (let ((current (point))
        (line-end (line-beginning-position)))
    (if (= current line-end)
        (beep)
      (let ((new (max line-end
                      (- current go-left-right-amount))))
        (goto-char new)))))

(define-key global-map (kbd "C-S-<right>") 'nomis/go-right-lots)
(define-key global-map (kbd "C-S-<left>") 'nomis/go-left-lots)

;;;; ___________________________________________________________________________

(provide 'nomis-keyboard-scrolling-and-movement)
