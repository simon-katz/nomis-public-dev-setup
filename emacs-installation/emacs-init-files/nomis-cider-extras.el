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
 ((version<= "0.26.1" (pkg-info-version-info 'cider))
  ;; I think this is now fixed.
  )
 (t
  (message-box
   "You need to fix `nomis/cider-avoid-multiple-result-prefixes` for this version of Cider.")))

;;;; ___________________________________________________________________________

(defun nomis/-get-cider-ns-refresh-log-buffer ()
  ;; Copied from `cider-ns-refresh`:
  (or (get-buffer cider-ns-refresh-log-buffer)
      (cider-make-popup-buffer cider-ns-refresh-log-buffer)))

;;;; ___________________________________________________________________________

(defvar nomis/cider-forbid-refresh-all? nil) ; Use dir-locals to set this when needed.

(advice-add
 'cider-ns-refresh
 :around
 (lambda (orig-fun mode &rest other-args)
   (let* ((log-buffer (nomis/-get-cider-ns-refresh-log-buffer)))
     (when (and nomis/cider-forbid-refresh-all?
                (member mode '(refresh-all 4 clear 16)))
       (let* ((msg "nomis/cider-forbid-refresh-all? is truthy, so I won't refresh-all"))
         (cider-emit-into-popup-buffer log-buffer
                                       (s-concat msg "\n")
                                       'font-lock-string-face
                                       t)
         (nomis/msg/grab-user-attention/high)
         (error msg))))
   (apply orig-fun mode args))
 '((name . nomis/maybe-forbid-refresh)))
;; (advice-remove 'cider-ns-refresh 'nomis/maybe-forbid-refresh)

;;;; ___________________________________________________________________________

(pushnew "deftest-msg" cider-test-defining-forms) ; a Wefarm Nabu thing

;;;; ___________________________________________________________________________

(provide 'nomis-cider-extras)
