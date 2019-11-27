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

(cond ; nomis/eval-clj-and-cljs
 ;; For example, send forms to both a CLJ REPL and a CLJS REPL from a
 ;; .cljc file.
 ;; Do this by:
 ;; - providing private versions of the following to return multiple sessions:
 ;;   - `cider-repls`
 ;;   - `sesman-current-session`
 ;;   - `sesman-ensure-session`
 ;; - arranging for these to be used within a call of `cider-interactive-eval`.

 ((member (cider-version)
          '("CIDER 0.23.0 (Lima)"
            "CIDER 0.24.0snapshot"))

  (defun -nomis/sesman-current-session (system &optional cxt-types)
    "Get the most relevant current session for the SYSTEM.
CXT-TYPES is a list of context types to consider."
    (or (sesman--linked-sessions system 'sort cxt-types)
        (sesman--friendly-sessions system 'sort)))

  (defun -nomis/sesman-ensure-session (system &optional cxt-types)
    "Get the most relevant linked session for SYSTEM or throw if none exists.
CXT-TYPES is a list of context types to consider."
    (or (-nomis/sesman-current-session system cxt-types)
        (user-error "No linked %s sessions" system)))

  (defun -nomis/cider-repls (&optional type ensure)
    "Return cider REPLs of TYPE from the current session.
If TYPE is nil or multi, return all repls.  If TYPE is a list of types,
return only REPLs of type contained in the list.  If ENSURE is non-nil,
throw an error if no linked session exists."
    (let* ((type (cond
                  ((listp type)
                   (mapcar #'cider-maybe-intern type))
                  ((cider-maybe-intern type))))
           (repls (-map #'second
                        (if ensure
                            (-nomis/sesman-ensure-session 'CIDER)
                          (-nomis/sesman-current-session 'CIDER)))))
      (or (seq-filter (lambda (b)
                        (cider--match-repl-type type b))
                      repls)
          (when ensure
            (cider--no-repls-user-error type)))))

  (defvar *-nomis/cider/eval-clj-and-cljs/hack-cider-repls? nil)

  (advice-add 'cider-interactive-eval
              :around
              (lambda (orig-fun &rest args)
                (let* ((*-nomis/cider/eval-clj-and-cljs/hack-cider-repls? t))
                  (apply orig-fun args)))
              '((name . nomis/eval-clj-and-cljs)
                (depth . -100)))

  (advice-add 'cider-load-buffer
              :around
              (lambda (orig-fun &rest args)
                (let* ((*-nomis/cider/eval-clj-and-cljs/hack-cider-repls? t))
                  (apply orig-fun args)))
              '((name . nomis/eval-clj-and-cljs)
                (depth . -100)))

  (advice-add 'cider-repls
              :around
              (lambda (orig-fun type ensure)
                (if *-nomis/cider/eval-clj-and-cljs/hack-cider-repls?
                    (-nomis/cider-repls type ensure)
                  (funcall orig-fun type ensure)))
              '((name . nomis/eval-clj-and-cljs)))

  )
 (t
  (message-box
   "You need to fix `nomis/eval-clj-and-cljs` for this version of Cider.")))

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(when nil ; Code to remove advice when in dev.
  (progn
    (advice-remove 'cider-interactive-eval 'nomis/eval-clj-and-cljs)
    (advice-remove 'cider-load-buffer      'nomis/eval-clj-and-cljs)
    (advice-remove 'cider-repls            'nomis/eval-clj-and-cljs))
  )

;;;; ___________________________________________________________________________

(provide 'nomis-cider-eval-clj-and-cljs)
