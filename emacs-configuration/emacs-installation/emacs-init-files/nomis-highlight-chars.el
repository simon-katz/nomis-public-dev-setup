;;; nomis-highlight-chars.el --- highlight-chars setup -*- lexical-binding: t; -*-

;;;; TODO: Consider using this for other things -- eg highlighting of trailing
;;;;       whitespace.

(require 'highlight-chars)

;;;; ___________________________________________________________________________

(face-spec-set ; TODO: Change all `face-spec-set` to use themes.
 'hc-hard-space
 '((t
    :background "grey80")))

;; Not this: It breaks `list-colors-display` (and who knows what else).
;; (add-hook 'font-lock-mode-hook 'hc-highlight-hard-spaces)

(add-hook 'org-mode-hook 'hc-highlight-hard-spaces)

;;;; ___________________________________________________________________________

(provide 'nomis-highlight-chars)
