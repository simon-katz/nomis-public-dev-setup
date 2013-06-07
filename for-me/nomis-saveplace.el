;;;; Init stuff -- Remembering positions in files.

;;;; ___________________________________________________________________________
;;;; ---- saveplace ----

;;;; I'm not sure, but this may be need to be done before the windows
;;;; stuff below in order for things under the control of windows to
;;;; work.

(require 'saveplace)
(setq-default save-place t)

;;;; ___________________________________________________________________________

(provide 'nomis-saveplace)
