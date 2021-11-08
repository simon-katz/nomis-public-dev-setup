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
    ;;
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
    ;; (setq cider-repl-history-file ".cider-repl-history-clj")
    ;;
    ;; New notes on 2021-11-08:
    ;; - Now we are using `nomis/-cider-repl-history-filename-clj` and
    ;;   `nomis/-cider-repl-history-filename-cljs`.
    )
  (setq cider-repl-history-size 5000) ; the default is 500
  )

;; This shouldn't be needed now that we are dealing with CIDER history
;; ourselves in `nomis/-write-cider-repl-history-file-immediately`.
(cond
 ((or (member (nomis/cider-version)
              '("CIDER 0.26.1 (Nesebar)"))
      (member (pkg-info-version-info 'cider)
              '("1.2.0snapshot (package: 20210909.1011)"
                "1.2.0snapshot (package: 20210929.1032)"
                "1.2.0snapshot (package: 20211105.708)"

                ;; Don't add new versions here if we are right that this problem
                ;; has gone away now that we are using
                ;; `nomis/-write-cider-repl-history-file-immediately`.
                )))
  (advice-add
   'cider-repl-history-just-save
   :after
   (lambda ()
     (message-box "`cider-repl-history-just-save` was unexpectedly called. This shouldn't happen now that we are using `nomis/-write-cider-repl-history-file-immediately`.")
     (let* ((repl-type (cider-repl-type (current-buffer))))
       (when (null repl-type)
         ;; Current buffer is not a REPL buffer.
         (message-box "I think this is a situation where we have created a random CIDER history file. current-buffer = %s"
                      (current-buffer)))))
   '((name . nomis/-note-random-history-files))))
 (t
  (message-box
   "You need to fix `nomis/-note-random-history-files` for this version of CIDER.")))

;; (advice-remove 'cider-repl-history-just-save 'nomis/-note-random-history-files)

;;;; ___________________________________________________________________________

(provide 'nomis-cider-repl-history)
