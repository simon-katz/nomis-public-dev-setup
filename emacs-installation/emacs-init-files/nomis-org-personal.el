;;;; nomis-org-personal ---  -*- lexical-binding: t -*-

;;;; ___________________________________________________________________________
;;;; ____ * Display -- blog faces

(defconst nomis/org-blog-faces
  '((org-level-1 . (:inherit outline-1 :weight bold :height 1.3
                             :box (:line-width 2
                                               :color "grey75"
                                               :style released-button)))
    (org-level-2 . (:inherit outline-2 :weight bold :height 1.2))
    (org-level-3 . (:inherit outline-3 :weight bold :height 1.1))
    (org-level-7 . nil)))

(defvar nomis/org-blog-stuff-on-p nil)

(defun nomis/toggle-org-blog-stuff ()
  (interactive)
  (make-local-variable 'nomis/org-blog-stuff-on-p)
  (make-local-variable 'face-remapping-alist)
  (setq face-remapping-alist
        (if nomis/org-blog-stuff-on-p
            (let* ((org-blog-faces-keys (-map 'car nomis/org-blog-faces)))
              (-remove (lambda (x) (memq (car x) org-blog-faces-keys))
                       face-remapping-alist))
          (append nomis/org-blog-faces
                  face-remapping-alist)))
  (setq nomis/org-blog-stuff-on-p
        (not nomis/org-blog-stuff-on-p)))


;;;; ___________________________________________________________________________
;;;; ____ * End

(provide 'nomis-org-personal)
