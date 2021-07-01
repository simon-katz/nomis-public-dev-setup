;;;; Init stuff -- CIDER REPL History --  -*- lexical-binding: t -*-

;;;; ___________________________________________________________________________

(setq cider-repl-history-highlight-current-entry t)
(setq cider-repl-history-highlight-inserted-item t)

(progn
  (progn ; set `cider-repl-history-file`
    ;; Old notes:
    ;;   In projects, you can create a directory-local variable for
    ;;   `cider-repl-history-file` in `cider-repl-mode`, as follows:
    ;;   - create a file called ".dir-locals.el" with this content:
    ;;       ((cider-repl-mode
    ;;         (cider-repl-history-file . ".cider-repl-history")))
    ;; New thoughts on 2019-10-03:
    ;; - If you use ".cider-repl-history" rather than "~/.cider-repl-history"
    ;;   all seems OK, despite confusing (perhaps outdated) comments about using
    ;;   dir locals at the following places:
    ;;   http://blogish.nomistech.com/separate-cider-repl-histories/
    ;;   https://github.com/clojure-emacs/cider/issues/2353#issuecomment-401126801
    ;;   https://github.com/clojure-emacs/cider/issues/37#issuecomment-7312083
    ;; - So not this:
    ;;   (setq cider-repl-history-file "~/.cider-repl-history")
    ;; But this:
    (setq cider-repl-history-file ".cider-repl-history"))
  (setq cider-repl-history-size 5000) ; the default is 500
  )

(cond
 ;; CIDER has `(add-hook 'kill-emacs-hook #'cider-repl-history-just-save)`. But
 ;; that doesn't save history unless the current buffer is the REPL buffer, and
 ;; it also creates an empty ".cider-repl-history" file in the same directory as
 ;; the current buffer's file. This fixes things by changing the current buffer
 ;; to the REPL buffer.
 ((member (nomis/cider-version)
          '("CIDER 0.26.1 (Nesebar)"))
  (advice-add
   'cider-repl-history-just-save
   :around
   (lambda (orig-fun &rest args)
     (let* ((buffer-name (buffer-name (current-buffer))))
       (message "==== buffer-name = %s (%s)"
                buffer-name
                (s-starts-with? "*cider-repl" buffer-name))
       (with-current-buffer (or (cider-current-repl)
                                (current-buffer))
         (let* ((res (apply orig-fun args)))
           (with-temp-file (format "nomis-cider-repl-history-info-%s--%s"
                                   (nomis/timestamp :date-time)
                                   (if (s-starts-with? "*cider-repl" buffer-name)
                                       "repl-buffer"
                                     buffer-name))
             ;; empty file
             )
           res))))
   '((name . nomis/set-buffer-for-history))))
 (t
  (message-box
   "You need to fix `cider-repl-history-just-save for this version of CIDER.")))

;; (advice-remove 'cider-repl-history-just-save 'nomis/set-buffer-for-history)

;;;; ___________________________________________________________________________

(provide 'nomis-cider-repl-history)
