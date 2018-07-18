;;;; Init stuff -- Fix bugs in clojure-mode.

;;;; ___________________________________________________________________________

(require 'nomis-sexp-utils)

(when (equal clojure-mode-version "5.6.1")
  ;; Fix broken `clojure-cycle-privacy`
  (defun clojure-cycle-privacy ()
    "Make public the current private def, or vice-versa.
See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-cycle-privacy"
    (interactive)
    (save-excursion
      (nomis/beginning-of-top-level-form)
      (search-forward-regexp "(defn?\\(-\\| ^:private\\)?\\_>")
      (if (match-string 1)
          (replace-match "" nil nil nil 1)
        (goto-char (match-end 0))
        (insert (if (or clojure-use-metadata-for-privacy
                        (equal (match-string 0) "(def"))
                    " ^:private"
                  "-"))))))

;;;; ___________________________________________________________________________

(provide 'nomis-clojure-mode-fixes)
