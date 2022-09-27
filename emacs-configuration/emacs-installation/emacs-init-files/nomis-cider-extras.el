;;;; Init stuff -- CIDER extras --  -*- lexical-binding: t -*-

;;;; ___________________________________________________________________________

(require 'nomis-clojure-test-files)

;;;; ___________________________________________________________________________

(cond
 ((member (nomis/cider-version)
          '("CIDER 0.24.0snapshot"))
  (advice-add
   'cider-repl-handler
   :around
   (lambda (orig-fun buffer)
     (let ((show-prompt t)
           (show-prefix t) ; THIS IS THE CHANGED BIT -- all `show-prefix` stuff
           )
       (nrepl-make-response-handler
        buffer
        (lambda (buffer value)
          (cider-repl-emit-result buffer value show-prefix)
          (setq show-prefix nil))
        (lambda (buffer out)
          (cider-repl-emit-stdout buffer out))
        (lambda (buffer err)
          (cider-repl-emit-stderr buffer err))
        (lambda (buffer)
          (when show-prompt
            (cider-repl-emit-prompt buffer)))
        nrepl-err-handler
        (lambda (buffer value content-type)
          (if-let* ((content-attrs (cadr content-type))
                    (content-type* (car content-type))
                    (handler (cdr (assoc content-type*
                                         cider-repl-content-type-handler-alist))))
              (setq show-prompt (funcall handler content-type buffer value nil t))
            (cider-repl-emit-result buffer value t t)))
        (lambda (buffer warning)
          (cider-repl-emit-stderr buffer warning)))))
   '((name . nomis/cider-avoid-multiple-result-prefixes))))
 ((let* ((v (pkg-info-version-info 'cider)))
    (or (ignore-errors ; because version string may be invalid
          (version<= "0.26.1" v))
        (member v
                '("1.2.0snapshot (package: 20210909.1011)"
                  "1.2.0snapshot (package: 20210929.1032)"))))
  ;; I think this is now fixed.
  )
 (t
  (cl-case 2
    (1 (message-box
        "You need to fix `nomis/cider-avoid-multiple-result-prefixes` for this version of CIDER."))
    (2 ; Assume it's fixed.
     ))))

;;;; ___________________________________________________________________________

(pushnew "deftest-msg" cider-test-defining-forms) ; a Wefarm Nabu thing

;;;; ___________________________________________________________________________
;;;; Add a prefix to CIDER eldoc info

(defconst nomis/-cider-eldoc-message-prefix "[cider] ")

(with-eval-after-load 'cider-eldoc
  (cond
   ((or (member (nomis/cider-version)
                '("CIDER 0.26.1 (Nesebar)"))
        (member (pkg-info-version-info 'cider)
                '("1.2.0snapshot (package: 20210909.1011)"
                  "1.2.0snapshot (package: 20210929.1032)"
                  "1.2.0snapshot (package: 20211105.708)"
                  "1.3.0 (package: 20220405.1216)"
                  "1.5.0 (package: 20220830.500)")))
    (advice-add
     'cider-eldoc-format-function
     :around
     (lambda (orig-fun &rest args)
       (concat nomis/-cider-eldoc-message-prefix
               (apply orig-fun args)))
     '((name . nomis/add-lsp-prefix))))

   (t
    (message-box
     "You need to fix `cider-eldoc-format-function` for this version of `CIDER`."))))

;;;; ___________________________________________________________________________
;;;; Fix annoying navigating back from single ns browser to all ns browser,
;;;; which always goes to top of buffer.

;;;; We want to hack `cider-browse-ns-all`, but only when called from the
;;;; single-ns browser. We do that by defining a new command
;;;; `nomis/cider-browse-ns-all-from-browse-single` and changing the
;;;; appropriate keybinding.

(cond
 ((member (pkg-info-version-info 'cider)
          '("1.5.0 (package: 20220830.500)"))

  (defun nomis/cider-browse-ns-all-from-browse-single ()
    (interactive)
    (let* ((ns-name-to-go-to cider-browse-ns-current-ns))
      (cider-browse-ns-all)
      (let* ((new-pos (or (search-forward  ns-name-to-go-to nil t)
                          (search-backward ns-name-to-go-to nil t))))
        (when new-pos
          (beginning-of-line)))))

  (define-key cider-browse-ns-mode-map "^" #'nomis/cider-browse-ns-all-from-browse-single))

 (t
  (message-box
   "You need to fix `nomis/cider-browse-ns-all-from-browse-single` for this version of `CIDER`.")))

;;;; ___________________________________________________________________________

(provide 'nomis-cider-extras)
