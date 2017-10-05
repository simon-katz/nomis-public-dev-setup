;;;; Init stuff -- clj-refactor -- deduce hydras

;;;; ___________________________________________________________________________

(require 's)
(require 'dash)
(require 'ht)

(defconst nomis/cljr--hydra/type->command-key/ht
  (ht ("ns"            "n")
      ("code"          "c")
      ("project"       "p")
      ("toplevel-form" "t")
      ("cljr"          "s")))

(defvar nomis/cljr--all-types
  (->> cljr--all-helpers
       (-map (lambda (fn-description) (-last-item (cdr fn-description))))
       (-flatten)
       (-distinct)
       (-remove-item "hydra")))

(defun nomis/cljr--hydra/type->name-string (type)
  (concat "nomis/hydra-cljr-" type "-menu"))

(defun nomis/cljr--hydra/type->name (type)
  (intern (nomis/cljr--hydra/type->name-string type)))

(defun nomis/cljr--hydra/type->name-incl-body (type)
  (intern (concat (nomis/cljr--hydra/type->name-string type)
                  "/body")))

(defun nomis/cljr--hydra/type->command-key (type)
  (or (ht-get nomis/cljr--hydra/type->command-key/ht
              type)
      (error "Bad type for `nomis/cljr--hydra/type->command-key`: %s"
             type)))

(defun nomis/cljr--type->helpers-RENAME (type)
  (->> cljr--all-helpers
       (-filter (lambda (fn-description)
                  (member type
                          (-last-item (cdr fn-description)))))))

(defvar nomis/cljr--doc-string-separator
  (make-string 60 ?-))

(defmacro nomis/cljr--def-hydras ()
  `(progn
     ,@(-map (lambda (type)
               (let* ((helpers (nomis/cljr--type->helpers-RENAME type))
                      (doc-string
                       (s-join "\n"
                               (list ""
                                     (concat type "-related refactorings")
                                     nomis/cljr--doc-string-separator
                                     (s-join "\n"
                                             (->> helpers
                                                  (-map (lambda (fn-description)
                                                          (format "_%s_: %s"
                                                                  (car fn-description)
                                                                  (caddr fn-description))))))
                                     "")))
                      (entries-RENAME (append (->> helpers
                                                   (-map (lambda (fn-description)
                                                           (list (car fn-description)
                                                                 (cadr fn-description)
                                                                 ;; #### FIXME
                                                                 :exit t))))
                                              '(("q" nil "quit")))))
                 `(defhydra ,(nomis/cljr--hydra/type->name type)
                    (:color pink :hint nil)
                    ,doc-string
                    ,@entries-RENAME)))
             nomis/cljr--all-types)
     (defhydra nomis/hydra-cljr-help-menu
       (:color pink :hint nil)
       "
#### MAKE DOC STRING
Available refactoring types
-----------------------------------------------------------------------------
_n_: Ns related refactorings      _c_: Code related refactorings
_p_: Project related refactorings _t_: Top level forms related refactorings
_s_: Refactor related functions"
       ,@(append (->> nomis/cljr--all-types
                      (-map (lambda (type)
                              (list (nomis/cljr--hydra/type->command-key type)
                                    (nomis/cljr--hydra/type->name-incl-body type)
                                    :exit t))))
                 '(("q" nil "quit" :color blue))))))

(nomis/cljr--def-hydras)


;;;; FIXME #### Define the top-level hydra.
;;;; FIXME #### Deal with updating `cljr--all-helpers`.


(define-key global-map (kbd "C-M-}") 'nomis/hydra-cljr-help-menu/body) ; FIXME #### Temp

;;;; ___________________________________________________________________________

(require 'nomis-clj-refactor-deduce-hydras-tests)

(provide 'nomis-clj-refactor-deduce-hydras)
