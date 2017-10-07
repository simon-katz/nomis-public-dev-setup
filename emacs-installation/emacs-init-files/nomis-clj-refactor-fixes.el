;;;; Init stuff -- Fix bugs in clj-refactor, and add things.

;;;; ___________________________________________________________________________

(defun nomis/cljr-define-keybindings-and-hydras ()
  (eval
   ;; `eval` used because we need the macroexpansion to happen at the time of
   ;; this call. Nasty.
   '(nomis/cljr--hydra/def-hydras nil))
  (nomis/cljr-add-keybindings))

(defun nomis/cljr-add-command (command-spec)
  "Add a cljr command. See the entries in `cljr--all-helpers` to see what
COMMAND-SPEC is.
This does the necessary for the cljr key binding prefix and for `cljr-helm`.
cljr's hydra menus aren't built from `cljr--all-helpers`, so the cljr hydra
menus are not changed."
  (setq cljr--all-helpers
        (-remove (lambda (helper)
                   (equal (cadr command-spec)
                          (cadr helper)))
                 cljr--all-helpers))
  (setq cljr--all-helpers
        (-snoc cljr--all-helpers
               command-spec))
  (nomis/cljr-define-keybindings-and-hydras))

;;;; ___________________________________________________________________________

(when (equal (cljr--version)
             "2.3.1")
  ;; `cider-eval-ns-form` is called with arguments, but it doesn't take any.
  (defadvice cider-eval-ns-form (around fix-broken-arg-list
                                        (&rest args-to-ignore))
    ad-do-it)
  (ad-activate 'cider-eval-ns-form))

;;;; ___________________________________________________________________________

(when (equal (cljr--version)
             "2.3.1")
  ;; `cljr-slash` doesn't respect `cljr-magic-requires` being `:prompt`.
  (defun cljr-slash ()
    "Inserts / as normal, but also checks for common namespace shorthands to require.
If `cljr-magic-require-namespaces' is non-nil, typing one of the
short aliases listed in `cljr-magic-requires' followed by this
command will add the corresponding require statement to the ns
form."
    (interactive)
    (insert "/")
    (unless (or (cljr--in-map-destructuring?)
                (cljr--in-ns-above-point-p))
      (when-let (aliases (and cljr-magic-requires
                              (not (cider-in-comment-p))
                              (not (cider-in-string-p))
                              (not (cljr--in-keyword-sans-alias-p))
                              (clojure-find-ns)
                              (cljr--magic-requires-lookup-alias)))
        (let ((short (cl-first aliases)))
          (when-let (long (cljr--prompt-user-for "Require " (cl-second aliases)))
            (when (and (not (cljr--in-namespace-declaration-p (concat ":as " short "\b")))
                       (or (not (eq :prompt cljr-magic-requires))
                           ;; jsk: This seems to me to break things, so get rid
                           ;;      of it.
                           ;; (not (> (length (cl-second aliases)) 1)) ; already prompted
                           (yes-or-no-p (format "Add %s :as %s to requires?" long short))))
              (save-excursion
                (cljr--insert-in-ns ":require")
                (let ((libspec (format "[%s :as %s]" long short)))
                  (insert libspec)
                  (ignore-errors (cljr--maybe-eval-ns-form))
                  (cljr--indent-defun)
                  (cljr--post-command-message "Required %s" libspec))))))))))

;;;; ___________________________________________________________________________

(require 'browse-url)

(when (equal (cljr--version)
             "2.3.1")
  
  (defun nomis/cljr-cheatsheet ()
    "Open a cljr cheatsheet."
    (interactive)
    (browse-url "https://www.cheatography.com/bilus/cheat-sheets/clj-refactor/pdf/"))

  (nomis/cljr-add-command
   '("zz" . (nomis/cljr-cheatsheet "cljr cheatsheet" ?h ("zz")))))

;;;; ___________________________________________________________________________

(when (equal (cljr--version)
             "2.3.1")
  
  (defun nomis/cljr-clean-ns-no-prune ()
    "Clean the ns form for the current buffer, without pruning."
    (interactive)
    (cljr--ensure-op-supported "clean-ns")
    (cider-eval-ns-form :sync)
    (cljr--clean-ns nil t))
  
  (nomis/cljr-add-command
   '("sn" . (nomis/cljr-clean-ns-no-prune "Clean ns no prune" ?c ("ns")))))

;;;; ___________________________________________________________________________

(when (equal (cljr--version)
             "2.3.1")  
  
  (defun nomis/cljr-cycle-collection-type ()
    "Display a message that describes the replacements for cycle collection."
    (interactive)
    (message (s-join " "
                     '("Use C-c C-r xxxx"
                      " where xxxx is one of x or C-x where x is one of"
                      " (  '  {  [  #")))
    (beep))
  
  (nomis/cljr-add-command
   '("cc" . (nomis/cljr-cycle-collection-type "Describe replacements for cycle collection." ?y ("code")))))

;;;; ___________________________________________________________________________

(provide 'nomis-clj-refactor-fixes)
