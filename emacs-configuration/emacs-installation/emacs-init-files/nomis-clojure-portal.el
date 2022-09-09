;;;; Init stuff -- nomis-clojure-portal  -*- lexical-binding: t; -*-

;;;; Copied from https://cljdoc.org/d/djblue/portal/0.30.0/doc/editors/emacs

;;;; ___________________________________________________________________________
;;;; Leverage an existing cider nrepl connection to evaluate portal.api
;;;; functions and map them to convenient key bindings.

;; def portal to the dev namespace to allow dereferencing via @dev/portal
(defun portal.api/open ()
  (interactive)
  (cider-nrepl-sync-request:eval
    "(do (ns dev) (def portal ((requiring-resolve 'portal.api/open))) (add-tap (requiring-resolve 'portal.api/submit)))"))

(defun portal.api/clear ()
  (interactive)
  (cider-nrepl-sync-request:eval "(portal.api/clear)"))

(defun portal.api/close ()
  (interactive)
  (cider-nrepl-sync-request:eval "(portal.api/close)"))

;; Example key mappings for doom emacs
;; (map! :map clojure-mode-map
;;       ;; cmd  + o
;;       :n "s-o" #'portal.api/open
;;       ;; ctrl + l
;;       :n "C-l" #'portal.api/clear)

;;;; ___________________________________________________________________________

(provide 'nomis-clojure-portal)
