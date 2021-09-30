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
 ((or (member (nomis/cider-version)
              '("CIDER 0.26.1 (Nesebar)"))
      (member (pkg-info-version-info 'cider)
              '("1.2.0snapshot (package: 20210909.1011)"
                "1.2.0snapshot (package: 20210929.1032)")))
  (advice-add
   'cider-repl-history-just-save
   :around
   (lambda (orig-fun &rest args)
     (let* ((repl (cider-current-repl)))
       (case 1
         (1 (when (null repl)
              (message-box "I think this might be a situation where we create a random CIDER history file. current-buffer = %s"
                           (current-buffer))
              (setq repl (current-buffer)))
            (with-current-buffer repl
              (apply orig-fun args)))
         (2 (if repl
                (with-current-buffer repl
                  (apply orig-fun args))
              (message-box "I think this might be a situation where we would have created a random CIDER history file."))))))
   '((name . nomis/set-buffer-for-history-and-prevent-random-history-files))))
 (t
  (message-box
   "You need to fix `cider-repl-history-just-save for this version of CIDER.")))

;; (advice-remove 'cider-repl-history-just-save 'nomis/set-buffer-for-history-and-prevent-random-history-files)

;;;; ___________________________________________________________________________

(provide 'nomis-cider-repl-history)
