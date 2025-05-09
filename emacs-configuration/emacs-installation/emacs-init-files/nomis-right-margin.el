;;;; nomis-right-margin ---  -*- lexical-binding: t -*-

;;;; ___________________________________________________________________________

(defvar nomis/right-margin-column 80)
(defvar nomis/right-margin-column-color "#cc99cc")

(setq-default fill-column nomis/right-margin-column)

(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)

(set-face-foreground 'fill-column-indicator nomis/right-margin-column-color)

;;;; ___________________________________________________________________________

(require 'nomis-whitespace-with-overlays-mode)

(add-hook 'text-mode-hook 'nomis/wwo/mode)
(add-hook 'prog-mode-hook 'nomis/wwo/mode)
(add-hook 'org-mode-hook  'nomis/wwo/mode/only-trailing)

;;;; ___________________________________________________________________________

(require 'column-marker)

(defface nomis/column-marker-1-face `((t :box ,(list :line-width -1
                                                     :color nomis/right-margin-column-color
                                                     :style nil)))
  "Face used for a column marker.  Usually a background color."
  :group 'faces)

(setq column-marker-1-face 'nomis/column-marker-1-face)

(defun nomis/get-80-column-stuff ()
  (let* ((f? (bound-and-true-p display-fill-column-indicator-mode))
         (c? column-marker-1))
    (cond ((and (not f?) (not c?)) 0)
          ((and      f?  (not c?)) 1)
          ((and (not f?)      c?)  2)
          (t                       3))))

(defun nomis/set-80-column-stuff (n &optional print-message?)
  (when print-message?
    (message "nomis/set-80-column-stuff setting approach to %s" n))
  (cl-flet ((f-off () (display-fill-column-indicator-mode 0))
            (f-on  () (display-fill-column-indicator-mode 1))
            (c-off () (column-marker-1 '(4)))
            (c-on  () (column-marker-1 nomis/right-margin-column)))
    (cl-case n
      (0 (f-off) (c-off))
      (1 (f-on)  (c-off))
      (2 (f-off) (c-on))
      (3 (f-on)  (c-on)))))

(defun nomis/set-80-column-stuff-0 (&optional print-message?)
  (interactive "p")
  (nomis/set-80-column-stuff 0 print-message?))

(defun nomis/set-80-column-stuff-1 (&optional print-message?)
  (interactive "p")
  (nomis/set-80-column-stuff 1 print-message?))

(defun nomis/set-80-column-stuff-2 (&optional print-message?)
  (interactive "p")
  (nomis/set-80-column-stuff 2 print-message?))

(defun nomis/set-80-column-stuff-3 (&optional print-message?)
  (interactive "p")
  (nomis/set-80-column-stuff 3 print-message?))

(defun nomis/cycle-80-column-stuff (&optional print-message?)
  (interactive "p")
  (nomis/set-80-column-stuff (mod (1+ (nomis/get-80-column-stuff))
                                  4)
                             print-message?))

;;;; ___________________________________________________________________________

(provide 'nomis-right-margin)
