;;;; Right margin

;;;; ___________________________________________________________________________

(require 'nomis-right-margin-column)
(require 'nomis-whitespace)
(require 'column-marker)

(defface nomis/column-marker-1-face '((t (:background "magenta")))
  "Face used for a column marker.  Usually a background color."
  :group 'faces)

(setq column-marker-1-face 'nomis/column-marker-1-face)

;;;; ___________________________________________________________________________

(defun nomis/get-80-column-stuff ()
  (let* ((w? whitespace-mode)
         (c? column-marker-1))
    (cond ((and (not w?) (not c?)) 0)
          ((and (not w?)      c?)  1)
          ((and      w?  (not c?)) 2)
          (t                       3))))

(defun nomis/set-80-column-stuff (n)
  (interactive "p")
  ;; (message "nomis/set-80-column-stuff setting approach to %s" n)
  (cl-flet ((w-off () (whitespace-mode 0))
            (w-on  () (whitespace-mode 1))
            (c-off () (column-marker-1 '(4)))
            (c-on  () (column-marker-1 nomis/right-margin-column)))
    (case n
      (0 (w-off) (c-off))
      (1 (w-off) (c-on))
      (2 (w-on)  (c-off))
      (3 (w-on)  (c-on)))))

(defun nomis/set-80-column-stuff-0 ()
  (interactive)
  (nomis/set-80-column-stuff 0))

(defun nomis/set-80-column-stuff-1 ()
  (interactive)
  (nomis/set-80-column-stuff 1))

(defun nomis/set-80-column-stuff-2 ()
  (interactive)
  (nomis/set-80-column-stuff 2))

(defun nomis/set-80-column-stuff-3 ()
  (interactive)
  (nomis/set-80-column-stuff 3))

(defun nomis/cycle-80-column-stuff ()
  (interactive)
  (nomis/set-80-column-stuff (mod (1+ (nomis/get-80-column-stuff))
                                  4)))

;;;; ___________________________________________________________________________

(add-hook 'text-mode-hook 'nomis/set-80-column-stuff-3)
(add-hook 'prog-mode-hook 'nomis/set-80-column-stuff-3)
(add-hook 'org-mode-hook  'nomis/set-80-column-stuff-0)

;;;; ___________________________________________________________________________

(provide 'nomis-right-margin)
