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

;;;; ___________________________________________________________________________

(provide 'nomis-cider-repl-history)
