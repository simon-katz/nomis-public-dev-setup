;;; nomis-terraform.el --- Terraform support -*- lexical-binding: t -*-

;;;; ___________________________________________________________________________

;; Make resource names easier to read. The default foreground is "pink", which
;; is hard to read with my background colours

(face-spec-set
 'terraform--resource-name-face
 '((t
    :foreground "blue")))

;;;; ___________________________________________________________________________

(provide 'nomis-terraform)