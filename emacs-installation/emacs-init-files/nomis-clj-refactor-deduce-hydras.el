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

(defconst nomis/cljr--hydra/doc-string-separator
  (make-string 60 ?-))

(defconst nomis/cljr--hydra/all-types
  (->> cljr--all-helpers
       (-map (lambda (cljr-helper) (-last-item (cdr cljr-helper))))
       (-flatten)
       (-distinct)
       (-remove-item "hydra")))

(defun nomis/cljr--hydra/type->name-string (type)
  (concat "nomis/cljr--hydra/" type "-menu"))

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

(defun nomis/cljr--hydra/type->cljr-helpers (type)
  (->> cljr--all-helpers
       (-filter (lambda (cljr-helper)
                  (member type
                          (-last-item (cdr cljr-helper)))))))

(defun nomis/cljr--hydra/cljr-helpers->doc-string (cljr-helpers)
  (s-join "\n"
          (list ""
                (concat type "-related refactorings")
                nomis/cljr--hydra/doc-string-separator
                (s-join "\n"
                        (->> cljr-helpers
                             (-map (lambda (cljr-helper)
                                     (format "_%s_: %s"
                                             (car cljr-helper)
                                             (caddr cljr-helper))))))
                "")))

(defun nomis/cljr--hydra/cljr-helpers->hydra-heads (cljr-helpers)
  (append (->> cljr-helpers
               (-map (lambda (cljr-helper)
                       (list (car cljr-helper)
                             (cadr cljr-helper)
                             ;; #### FIXME
                             :exit t))))
          '(("q" nil "quit"))))

(defun nomis/cljr--hydra/type->hydra-defining-form (type)
  (let* ((cljr-helpers (nomis/cljr--hydra/type->cljr-helpers type)))
    `(defhydra ,(nomis/cljr--hydra/type->name type)
       (:color pink :hint nil)
       ,(nomis/cljr--hydra/cljr-helpers->doc-string cljr-helpers)
       ,@(nomis/cljr--hydra/cljr-helpers->hydra-heads cljr-helpers))))

(defmacro nomis/cljr--hydra/def-hydras ()
  `(progn
     ,@(-map 'nomis/cljr--hydra/type->hydra-defining-form
             nomis/cljr--hydra/all-types)
     (defhydra nomis/cljr--hydra/help-menu
       (:color pink :hint nil)
       "
#### MAKE DOC STRING
Available refactoring types
-----------------------------------------------------------------------------
_n_: Ns related refactorings      _c_: Code related refactorings
_p_: Project related refactorings _t_: Top level forms related refactorings
_s_: Refactor related functions"
       ,@(append (->> nomis/cljr--hydra/all-types
                      (-map (lambda (type)
                              (list (nomis/cljr--hydra/type->command-key type)
                                    (nomis/cljr--hydra/type->name-incl-body type)
                                    :exit t))))
                 '(("q" nil "quit" :color blue))))))

(nomis/cljr--hydra/def-hydras)


;;;; FIXME #### Define the top-level hydra.
;;;; FIXME #### Deal with updating `cljr--all-helpers`.


(define-key global-map (kbd "C-M-}") 'nomis/cljr--hydra/help-menu/body) ; FIXME #### Temp

;;;; ___________________________________________________________________________

(require 'nomis-clj-refactor-deduce-hydras-tests)

(provide 'nomis-clj-refactor-deduce-hydras)
