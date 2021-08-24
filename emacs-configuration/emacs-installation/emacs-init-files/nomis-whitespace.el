;;;; Init stuff -- whitespace

(require 'nomis-right-margin-column)
(require 'nomis-whitespace-with-overlays-mode)

;;;; ___________________________________________________________________________

(add-hook 'text-mode-hook 'nomis/wwo/mode)
(add-hook 'prog-mode-hook 'nomis/wwo/mode)
(add-hook 'org-mode-hook  'nomis/wwo/mode/only-trailing)

;;;; ___________________________________________________________________________

(provide 'nomis-whitespace)
