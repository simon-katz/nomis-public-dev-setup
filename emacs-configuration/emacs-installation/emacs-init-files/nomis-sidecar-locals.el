;;; nomis-sidecar-locals.el --- sidecar tailoring -*- lexical-binding: t -*-

;;;; ___________________________________________________________________________

(sidecar-locals-mode)

(setq sidecar-locals-paths-allow
      '("/Users/simonkatz/development-100/repositories/"))

(setq sidecar-locals-dir-name "temp-name-sidecar-locals") ; TODO: Hack: For `nomis-do-to-all-git-repos`, can't have names beginning with "."

;;;; ___________________________________________________________________________

(provide 'nomis-sidecar-locals)
