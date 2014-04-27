;;;; Init stuff -- smartparens

;; TODO: smartparens problems
;; - In HTML, doesn't understand comments <!-- xxxx -->

;;;; ___________________________________________________________________________

(require 'nomis-smartparens-authors-stuff)

;;;; ___________________________________________________________________________
;;;; Make things more like paredit.

(smartparens-global-strict-mode t)
(define-key sp-keymap (kbd "M-J") 'sp-join-sexp)
(define-key sp-keymap (kbd "M-S") 'sp-split-sexp)
(define-key sp-keymap (kbd "M-q") 'sp-indent-defun)

;;;; On Mac, I can't use C-M-d because it's highjacked at a low level
;;;; by the system for dictionary lookup.
;;;; Use M-d instead.  (The default is sp-kill-word, which
;;;; I can do without.)
(when (equal system-type 'darwin)
  (define-key sp-keymap (kbd "M-d")
    'sp-down-sexp))

;;;; ___________________________________________________________________________
;;;; TAB is broken. I get this error:
;;;;   ac-yasnippet-candidates:
;;;;     Symbol's function definition is void: yas/current-snippet-table
;;;; Fix from
;;;; http://www.kurup.org/blog/2012/10/15/emacs-autocomplete-stumbles-on-yasnippet/

(defalias 'yas/current-snippet-table 'yas--get-snippet-tables)

;;;; ___________________________________________________________________________

(provide 'nomis-smartparens)
