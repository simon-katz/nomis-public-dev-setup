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

;; (pushnew "deftest-msg" cider-test-defining-forms) ; a Wefarm Nabu thing

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
                  "1.5.0 (package: 20220830.500)"
                  "1.7.0 (package: 20230518.550)")))
    (advice-add
     'cider-eldoc-format-function
     :around
     (lambda (orig-fun &rest args)
       (concat nomis/-cider-eldoc-message-prefix
               (apply orig-fun args)))
     '((name . nomis/add-cider-prefix))))

   (t
    ;; We are doing this generically in `mp-flycheck-eldoc` now.
    )))

;;;; ___________________________________________________________________________
;;;; Fix annoying navigating back from single ns browser to all ns browser,
;;;; which always goes to top of buffer.

;;;; :possible-open-source-contribution `nomis/cider-browse-ns-all-from-browse-single`

;;;; We want to hack `cider-browse-ns-all`, but only when called from the
;;;; single-ns browser. We do that by defining a new command
;;;; `nomis/cider-browse-ns-all-from-browse-single` and changing the
;;;; appropriate keybinding.

(cond
 ((member (pkg-info-version-info 'cider)
          '("1.5.0 (package: 20220830.500)"
            "1.7.0 (package: 20230518.550)"
            "20250217.1433"
            "20250430.722"
            "20250806.1944"))

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
;;;; Fix annoying prompt for namespace in `cider-browse-ns`.

;;;; :possible-open-source-contribution `nomis/cider-browse-ns`

(cond
 ((member (pkg-info-version-info 'cider)
          '("1.5.0 (package: 20220830.500)"
            "1.7.0 (package: 20230518.550)"
            "20250217.1433"
            "20250430.722"
            "20250806.1944"))

  (defun nomis/cider-browse-ns ()
    "Like `cider-browse-ns`, but uses the current namespace instead of prompting."
    (interactive)
    (let* ((repls (cider-repls)))
      (if (null repls)
          (user-error "This buffer has no CIDER sessions"))
      (let* ((ns-name (clojure-find-ns)))
        (if (null ns-name)
            (user-error "This buffer has no namespace")
          (let* ((clojure-command (format "(find-ns '%s)" ns-name))
                 (clojure-return-value-as-string
                  (nrepl-dict-get (cider-nrepl-sync-request:eval clojure-command)
                                  "value")))
            (if (equal "nil" clojure-return-value-as-string)
                (user-error "Namespace %s is not loaded" ns-name)
              (cider-browse-ns ns-name))))))))

 (t
  (message-box
   "You need to fix `nomis/cider-browse-ns` for this version of `CIDER`.")))

;;;; ___________________________________________________________________________
;;;; `nomis/add-cider-clojure-cli-alias`

(defun -nomis/new-cider-clojure-cli-aliases-value (old-value new-alias prepend?)
  (cl-assert (or (null old-value)
                 (stringp old-value)))
  (let* ((canonicalised-old-value (if (or (null old-value)
                                          (s-starts-with? ":" old-value))
                                      old-value
                                    (s-concat ":" old-value)))
         (old-aliases (if (null canonicalised-old-value)
                          '()
                        (s-split ":" (s-chop-left 1 canonicalised-old-value))))
         (old-aliases-sans-new-alias (remove new-alias old-aliases))
         (new-aliases (if prepend?
                          (cons new-alias old-aliases-sans-new-alias)
                        (-snoc old-aliases-sans-new-alias new-alias))))
    (s-concat ":" (s-join ":" new-aliases))))

(cl-defmacro nomis/add-cider-clojure-cli-alias (new-alias &key prepend?)
  "Add NEW-ALIAS to CIDER-CLOJURE-CLI-ALIASES.
By default add at the end, but if PREPEND? is non-nil add at the beginning.
If NEW-ALIAS is already in CIDER-CLOJURE-CLI-ALIASES, remove it first so that
NEW-ALIAS is always at the end (or at the beginning if PREPEND? is non-nil)."
  `(setq-local cider-clojure-cli-aliases
               (-nomis/new-cider-clojure-cli-aliases-value
                cider-clojure-cli-aliases
                ,new-alias
                ,prepend?)))

;; Testing -- all buffer-local so no problems with blatting stuff.

(cl-assert (equal ":a"
                  (progn
                    (setq-local cider-clojure-cli-aliases nil)
                    (nomis/add-cider-clojure-cli-alias "a"))))
(cl-assert (equal ":a:b:c"
                  (progn
                    (setq-local cider-clojure-cli-aliases nil)
                    (nomis/add-cider-clojure-cli-alias "a")
                    (nomis/add-cider-clojure-cli-alias "b")
                    (nomis/add-cider-clojure-cli-alias "c"))))
(cl-assert (equal ":a:b:c"
                  (progn
                    (setq-local cider-clojure-cli-aliases nil)
                    (nomis/add-cider-clojure-cli-alias "a")
                    (nomis/add-cider-clojure-cli-alias "b")
                    (nomis/add-cider-clojure-cli-alias "c")
                    (nomis/add-cider-clojure-cli-alias "a")
                    (nomis/add-cider-clojure-cli-alias "b")
                    (nomis/add-cider-clojure-cli-alias "c"))))
(cl-assert (equal ":b:c:a"
                  (progn
                    (setq-local cider-clojure-cli-aliases nil)
                    (nomis/add-cider-clojure-cli-alias "a")
                    (nomis/add-cider-clojure-cli-alias "b")
                    (nomis/add-cider-clojure-cli-alias "c")
                    (nomis/add-cider-clojure-cli-alias "a"))))

(cl-assert (equal ":a"
                  (progn
                    (setq-local cider-clojure-cli-aliases nil)
                    (nomis/add-cider-clojure-cli-alias "a" :prepend? t))))
(cl-assert (equal ":c:b:a"
                  (progn
                    (setq-local cider-clojure-cli-aliases nil)
                    (nomis/add-cider-clojure-cli-alias "a" :prepend? t)
                    (nomis/add-cider-clojure-cli-alias "b" :prepend? t)
                    (nomis/add-cider-clojure-cli-alias "c" :prepend? t))))
(cl-assert (equal ":c:b:a"
                  (progn
                    (setq-local cider-clojure-cli-aliases nil)
                    (nomis/add-cider-clojure-cli-alias "a")
                    (nomis/add-cider-clojure-cli-alias "b" :prepend? t)
                    (nomis/add-cider-clojure-cli-alias "c" :prepend? t)
                    (nomis/add-cider-clojure-cli-alias "a" :prepend? t)
                    (nomis/add-cider-clojure-cli-alias "b" :prepend? t)
                    (nomis/add-cider-clojure-cli-alias "c" :prepend? t))))
(cl-assert (equal ":a:c:b"
                  (progn
                    (setq-local cider-clojure-cli-aliases nil)
                    (nomis/add-cider-clojure-cli-alias "a")
                    (nomis/add-cider-clojure-cli-alias "b" :prepend? t)
                    (nomis/add-cider-clojure-cli-alias "c" :prepend? t)
                    (nomis/add-cider-clojure-cli-alias "a" :prepend? t))))

(cl-assert (equal ":a:b:c"
                  (progn
                    (setq-local cider-clojure-cli-aliases "a:b")
                    (nomis/add-cider-clojure-cli-alias "c"))))
(cl-assert (equal ":c:b:a"
                  (progn
                    (setq-local cider-clojure-cli-aliases "b:a")
                    (nomis/add-cider-clojure-cli-alias "c" :prepend? t))))

;;;; ___________________________________________________________________________
;;;; Electric `e/defn` indentation goes wrong when I connect a CLJS sibling
;;;; REPL. Here's a hack to fix that.
;;;; See https://github.com/clojure-emacs/cider/issues/3815.

(cond
 ((member (pkg-info-version-info 'cider)
          '("20250217.1433"
            "20250430.722"
            "20250806.1944"))

  (advice-add 'cider--get-symbol-indent
              :around
              (lambda (orig-fun symbol-name)
                (let* ((orig-res (funcall orig-fun symbol-name)))
                  (if (not (member symbol-name '("e/defn"
                                                 "e/fn")))
                      orig-res
                    ;; (let* (;; Copy code from `cider--get-symbol-indent` and print
                    ;;        ;; info:
                    ;;        (ns (let ((clojure-cache-ns t)) ; we force ns caching here for performance reasons
                    ;;              ;; silence bytecode warning of unused lexical var
                    ;;              (ignore clojure-cache-ns)
                    ;;              (cider-current-ns)))
                    ;;        (meta (cider-resolve-var ns symbol-name))
                    ;;        (indent (or (nrepl-dict-get meta "style/indent")
                    ;;                    (nrepl-dict-get meta "indent"))))
                    ;;   (let ((inhibit-message t))
                    ;;     (message "For %s : res = %s / indent of %s in %s"
                    ;;              symbol-name orig-res indent meta)))
                    nil)))
              '((name . nomis/fix-indentation-when-cljs-sibling))))

 (t
  (message-box
   "You need to fix `nomis/fix-indentation-when-cljs-sibling for this version of CIDER.")))

;; (advice-remove 'cider--get-symbol-indent 'nomis/fix-indentation-when-cljs-sibling)

;; These change when connecting sibling:
;;   (cider-resolve-var "hyperfiddle.electric3" "defn")
;;   (cider-resolve-var "hyperfiddle.electric3" "fn")
;; from
;;   (dict "macro" "true")
;;   (dict "macro" "true" "style/indent" ":defn")
;; to
;;   (dict "arglists" "([nm & fdecl])" "macro" "true" "style/indent" "1")
;;   (dict "arglists" "([& args])" "macro" "true" "style/indent" "0")

;;;; ___________________________________________________________________________

(defvar nomis/cider-newline-string "
")

(defun nomis/cider-transform-string-value (value)
  (s-replace "\\n"
             nomis/cider-newline-string
             value))

(defun nomis/cider-uptime ()
  (interactive)
  (let* ((repl (first (cider-repls "clj" t))))
    (with-current-buffer repl
      (nomis/run-clojure
       "(require 'nomis-clj-repl-tools)"
       (lambda (_)
         (nomis/run-clojure
          "(nomis-clj-repl-tools/uptime-string)"
          (lambda (v)
            (message "%s" (substring (nomis/cider-transform-string-value v)
                                     1
                                     -1)))))))))

;;;; ___________________________________________________________________________

(setq cider-enable-nrepl-jvmti-agent t)

;;;; ___________________________________________________________________________

(provide 'nomis-cider-extras)
