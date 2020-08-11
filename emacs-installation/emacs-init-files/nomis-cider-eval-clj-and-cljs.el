;;;; Init stuff -- nomis-cider-eval-clj-and-cljs ---  -*- lexical-binding: t -*-

;;;; ___________________________________________________________________________

;; On Slack clojurians #cider channel (2019-11-22/23)
;;
;; nomiskatz 7:45 PM
;;     I have a CLJ REPL and a CLJS REPL. My sesman-browser shows something like
;;     the following:
;;          CIDER Sessions:
;;           1: my-apps/my-app:localhost:63615
;;               linked-to: proj(~/dev/my-apps/my-app/)
;;                 objects: *cider-repl %s(clj)*
;;           2: my-apps/my-app:localhost:8778
;;               linked-to: proj(~/dev/my-apps/my-app/)
;;                 objects: *cider-repl %s(cljs:shadow-select)*
;;     Is it possible to pass the same form to both REPLs with C-M-x etc?
;;     (Perhaps I’ve set up my sessions wrongly.)
;;
;; nomiskatz 9:32 AM
;;     ^^ I mean a form from a .cljc file. (edited)
;;
;; bozhidar:cider: 12:14 PM
;;     @nomiskatz I think we’ve implemented this only for load-file. I’ll have
;;     to check. I have some vague memory that we wanted to make it possible to
;;     eval either in the last REPL or in both REPLs for both commands, but we
;;     never finished this.
;;     Might be a good idea to check the tickets for this, and potentially file
;;     one.
;;
;; bozhidar:cider: 12:24 PM
;;     I see we have this in the manual
;;     https://docs.cider.mx/cider/basics/clojurescript.html#_working_with_cljc_files
;;     but it’s a bit vague even for me. :slightly_smiling_face:
;;
;;
;; Created https://github.com/clojure-emacs/cider/issues/2756 -- details:
;;
;;     When both CLJ and CLJS REPLS exist, if I use an evaluation command such
;;     as C-M-x in a .cljc file the form is sent only to the
;;     most-recently-visited REPL.
;;
;;     `cider-load-file` works in a similar way.
;;
;;     There is some high-level code that handles multiple REPLs, but the
;;     low-level code (around Sesman sessions) doesn't handle multiple REPLs.
;;
;;     I suspect that at one time C-M-x etc may have sent forms to multiple
;;     REPLs, and that there have been changes that break this.
;;
;;     I haven't looked at `cider-load-file`, but for C-M-x and other commands
;;     that go through `cider-interactive-eval` the problem is in the following
;;     call chain:
;;
;;          `cider-interactive-eval`
;;       -> `cider-map-repls`
;;       -> `cider-repls`
;;       -> `sesman-ensure-session` or `sesman-current-session`
;;
;;     I have hacked a proof-of-concept fix for this (this file).
;;
;;     I've tested the hack on the current master of CIDER (commit dd74ddd --
;;     "Mention the "State of CIDER" survey in the REPL's help banner").
;;
;;     My code arranges for `cider-repls` to return multiple REPLs when called
;;     using the above call chain, by placing advice on `cider-interactive-eval`
;;     and `cider-map-repls` and using alternative versions of
;;     `sesman-ensure-session` and `sesman-current-session`.
;;
;; Further notes
;;
;;     Call chain for `cider-load-file`:
;;
;;          `cider-load-file`
;;       -> `cider-load-buffer`
;;       -> `cider-map-repls` which we have above for `cider-interactive-eval`.
;;
;;     So we can fix `cider-load-file` and `cider-load-buffer` in the same way.

;;;; ___________________________________________________________________________

;;;; Turn this off for now -- I'm not convinced I understood things properly. eg what is a sibling connection?

;; (defvar *nomis/cider/eval-clj-and-cljs/feature-on? nil)

;; (cond ; nomis/eval-clj-and-cljs
;;  ;; For example, send forms to both a CLJ REPL and a CLJS REPL from a
;;  ;; .cljc file.

;;  ;; Take care to send forms to each REPL process only once, so deal with
;;  ;; removing duplicates when there are siblings.

;;  ;; Do this by hacking `cider-repls` in some call chains to not use
;;  ;; `sesman-current-session` -- instead, start by calling
;;  ;; `sesman-current-sessions` and then process the return value.

;;  ;; TODO: Consider simply changing `cider-repls` (and not just in particular
;;  ;;       call chains).
;;  ;;       BUT your recent changes (one REPL of each type per session) mean you
;;  ;;       have moved on a long way, so maybe not. Maybe a new function to be
;;  ;;       called from a few places.

;;  ;; TODO: `cider-repl-switch-to-other` is broken. When there are sibling REPLs
;;  ;;       of the same type, it can go to one of them. The doc string says switch
;;  ;;       between CLJ and CLJS.

;;  ((member (cider-version)
;;           '("CIDER 0.23.0 (Lima)"
;;             "CIDER 0.24.0snapshot"))

;;   (defun -nomis/cider-repls-to-evaluate-in (&optional type ensure)
;;     "Return cider REPLs of type TYPE from all current sessions.
;; When there are sibling REPLs of the same type, include only one
;; of the REPLs. If TYPE is nil or multi, return all repls. If TYPE
;; is a list of types, return only REPLs of type contained in the
;; list. If ENSURE is non-nil, throw an error if no linked session
;; exists."
;;     (let* ((type (cond
;;                   ((listp type)
;;                    (mapcar #'cider-maybe-intern type))
;;                   ((cider-maybe-intern type))))
;;            (sesman-sessions (sesman-current-sessions 'CIDER))
;;            (one-repl-of-each-type-for-each-session
;;             (mapcar (lambda (sesman-session)
;;                       (let ((repls (cdr sesman-session)))
;;                         (seq-uniq repls
;;                                   (lambda (x y)
;;                                     (eq (cider-repl-type x)
;;                                         (cider-repl-type y))))))
;;                     sesman-sessions))
;;            (repls (apply #'append one-repl-of-each-type-for-each-session)))
;;       (or (seq-filter (lambda (b)
;;                         (cider--match-repl-type type b))
;;                       repls)
;;           (when ensure
;;             (cider--no-repls-user-error type)))))

;;   (defvar *-nomis/cider/eval-clj-and-cljs/hack-cider-repls? nil)

;;   (advice-add 'cider-interactive-eval
;;               :around
;;               (lambda (orig-fun &rest args)
;;                 (let* ((*-nomis/cider/eval-clj-and-cljs/hack-cider-repls?
;;                         *nomis/cider/eval-clj-and-cljs/feature-on?))
;;                   (apply orig-fun args)))
;;               '((name . nomis/eval-clj-and-cljs)
;;                 (depth . -100)))

;;   (advice-add 'cider-load-buffer
;;               :around
;;               (lambda (orig-fun &rest args)
;;                 (let* ((*-nomis/cider/eval-clj-and-cljs/hack-cider-repls?
;;                         *nomis/cider/eval-clj-and-cljs/feature-on?))
;;                   (apply orig-fun args)))
;;               '((name . nomis/eval-clj-and-cljs)
;;                 (depth . -100)))

;;   (advice-add 'cider-repls
;;               :around
;;               (lambda (orig-fun &rest args)
;;                 (if *-nomis/cider/eval-clj-and-cljs/hack-cider-repls?
;;                     (apply #'-nomis/cider-repls-to-evaluate-in args)
;;                   (apply orig-fun args)))
;;               '((name . nomis/eval-clj-and-cljs)))

;;   )
;;  (t
;;   (message-box
;;    "You need to fix `nomis/eval-clj-and-cljs` for this version of Cider.")))

;; ;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

;; (when nil ; Code to remove advice when in dev.
;;   (progn
;;     (advice-remove 'cider-interactive-eval 'nomis/eval-clj-and-cljs)
;;     (advice-remove 'cider-load-buffer      'nomis/eval-clj-and-cljs)
;;     (advice-remove 'cider-repls            'nomis/eval-clj-and-cljs))
;;   )

;;;; ___________________________________________________________________________

(provide 'nomis-cider-eval-clj-and-cljs)
