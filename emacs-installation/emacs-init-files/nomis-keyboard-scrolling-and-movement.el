;;;; Init stuff -- Keyboard scrolling and movement.

;;;; ___________________________________________________________________________

;;;; See https://www.gnu.org/software/emacs/manual/html_node/emacs/Horizontal-Scrolling.html

(setq hscroll-margin 0)
(setq hscroll-step 1)

;;;; ___________________________________________________________________________

(defun nomis-scroll-down-in-place (&optional n)
  "Like scroll-down-line, except move point to keep it at the same position
in the display."
  (interactive "p")
  (scroll-down-line n)
  (previous-line n))

(defun nomis-scroll-up-in-place (&optional n)
  "Like scroll-up-line, except move point to keep it at the same position
in the display."
  (interactive "p")
  (next-line n)
  (scroll-up-line n))

(define-key global-map (kbd "C-M-S-<up>")   'scroll-down-line)
(define-key global-map (kbd "C-M-S-<down>") 'scroll-up-line)
(define-key global-map (kbd "C-S-<up>")     'nomis-scroll-down-in-place)
(define-key global-map (kbd "C-S-<down>")   'nomis-scroll-up-in-place)

;;;; ___________________________________________________________________________

(provide 'nomis-keyboard-scrolling-and-movement)
