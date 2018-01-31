;;;; Init stuff -- Nomis utils

(require 'cl)

;;;; ___________________________________________________________________________

(defun nomis/positions (pred list) ; does this exist anywhere else?
  (cl-loop for x in list
           as  cnt from 0
           when (funcall pred x)
           collect cnt))

;;;; ___________________________________________________________________________

(provide 'nomis-core-utils)
