;;; nomis-tree-outline-2-waste.el --- Junk  -*- lexical-binding: t; -*-

;;; Code:

(error "nomis-tree-outline-2-waste is not for loading")

;;;; Requires

(require 'outline)

;;;; Utilities

(defun -nomis/tree/outline/making-visible (f) ; TODO: Unused.
  (save-excursion
    (funcall f)
    (outline-show-entry))
  (funcall f))

(defun -nomis/tree/outline/regexp-at-bol () ; TODO: Rename
  (concat "^" outline-regexp))

(defun -nomis/tree/outline/regexp-at-bol-2 () ; TODO: Rename
  ;; TODO: What about things that start with other things?
  (concat "^"
          "\\("
          (substring outline-regexp
                     0
                     (1+ (cl-search "*" outline-regexp)))
          " \\|("
          "\\)"))

;;;; Other stuff

(defun nomis/tree/outline/previous-heading-v001 ()
  (interactive)
  (if (bobp)
      (error "Beginning of buffer")
    (let* ((new-pos
            (save-excursion
              (let* ((pos (re-search-backward (-nomis/tree/outline/regexp-at-bol-2)
                                              nil
                                              t)))
                (when pos (outline-show-entry))
                pos))))
      (if new-pos
          (goto-char new-pos)
        (error "No previous heading")))))

(defun nomis/tree/outline/next-heading-v001 ()
  (interactive)
  (if (eobp)
      (error "End of buffer")
    (let* ((new-pos
            (save-excursion
              (when (looking-at-p (-nomis/tree/outline/regexp-at-bol))
                (forward-char))
              (let* ((pos (re-search-forward (-nomis/tree/outline/regexp-at-bol-2)
                                             nil
                                             t)))
                (when pos (outline-show-entry))
                pos))))
      (if new-pos
          (progn (goto-char new-pos)
                 (beginning-of-line))
        (error "No next heading")))))

(defun nomis/tree/outline/next-heading-v002 ()
  (interactive)
  (if (eobp)
      (error "End of buffer")
    (let* ((opoint (point))
           (npoint (save-excursion
                     (outline-next-heading)
                     (when (/= opoint (point)) (outline-show-entry))
                     (point))))
      (if (/= opoint npoint)
          (goto-char npoint)
        ;; (error "No next heading")
        ))))

;;; End

(provide 'nomis-tree-outline-2-waste)
