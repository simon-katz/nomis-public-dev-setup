;;;; Init stuff --- Java ---  -*- lexical-binding: t -*-

;;;; ___________________________________________________________________________

(defun -nomis/java-init ()
  (setq c-basic-offset 2))

(add-hook 'java-mode-hook '-nomis/java-init)

;;;; ___________________________________________________________________________

(provide 'nomis-java)
