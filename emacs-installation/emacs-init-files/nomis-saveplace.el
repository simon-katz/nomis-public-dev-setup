;;;; Init stuff -- Remembering positions in files.

;;;; ___________________________________________________________________________
;;;; ---- saveplace ----

;;;; See https://www.emacswiki.org/emacs/SavePlace

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;;; For GNU Emacs 24.5 and older versions

;; (require 'saveplace)
;; (setq-default save-place t)

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;;; For GNU Emacs 25.1 and newer versions

(save-place-mode 1) 
(setq save-place-forget-unreadable-files nil)

;;;; ___________________________________________________________________________

(provide 'nomis-saveplace)
