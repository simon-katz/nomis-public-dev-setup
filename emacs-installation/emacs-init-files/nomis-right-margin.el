;;;; Right margin

;;;; ___________________________________________________________________________

(require 'nomis-right-margin-column)
(require 'nomis-fci-mode)
(require 'column-marker)

(defface nomis/column-marker-1-face `((t :box ,(list :line-width -1
                                                     :color fci-rule-color ; "red"
                                                     :style nil)))
  "Face used for a column marker.  Usually a background color."
  :group 'faces)

(setq column-marker-1-face 'nomis/column-marker-1-face)

;;;; ___________________________________________________________________________

(defun nomis/get-80-column-stuff ()
  (let* ((f? (bound-and-true-p fci-mode))
         (c? column-marker-1))
    (cond ((and (not f?) (not c?)) 0)
          ((and (not f?)      c?)  1)
          ((and      f?  (not c?)) 2)
          (t                       3))))

(defun nomis/set-80-column-stuff (n)
  (interactive "p")
  (message "nomis/set-80-column-stuff setting approach to %s" n)
  (cl-flet ((f-off () (fci-mode 0))
            (f-on  () (fci-mode 1))
            (c-off () (column-marker-1 '(4)))
            (c-on  () (column-marker-1 nomis/right-margin-column)))
    (case n
      (0 (f-off) (c-off))
      (1 (f-off) (c-on))
      (2 (f-on)  (c-off))
      (3 (f-on)  (c-on)))))

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

(provide 'nomis-right-margin)
