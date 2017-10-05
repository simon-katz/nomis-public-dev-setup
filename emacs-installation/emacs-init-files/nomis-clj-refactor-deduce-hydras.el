;;;; Init stuff -- clj-refactor -- deduce hydras

;;;; ___________________________________________________________________________

(require 's)
(require 'dash)

(defvar nomis/cljr--all-types
  (->> cljr--all-helpers
       (-map (lambda (fn-description) (-last-item (cdr fn-description))))
       (-flatten)
       (-distinct)
       (-remove-item "hydra")))

(defun nomis/cljr--hydra-type->name (type)
  (intern (concat "nomis/hydra-cljr-" type "-menu")))

(defun nomis/cljr--type->hydra-entry-RENAME (type)
  (cond ((equal type "ns")
         '("n" nomis/hydra-cljr-ns-menu/body :exit t))
        ((equal type "code")
         '("c" nomis/hydra-cljr-code-menu/body :exit t))
        ((equal type "project")
         '("p" nomis/hydra-cljr-project-menu/body :exit t))
        ((equal type "toplevel-form")
         '("t" nomis/hydra-cljr-toplevel-form-menu/body :exit t))
        ((equal type "cljr")
         '("s" nomis/hydra-cljr-cljr-menu/body :exit t))))

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
                 `(defhydra ,(nomis/cljr--hydra-type->name type)
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
                              (nomis/cljr--type->hydra-entry-RENAME type))))
                 '(("q" nil "quit" :color blue))))))

(nomis/cljr--def-hydras)


;;;; #### Define the top-level hydra.
;;;; #### Deal with updating `cljr--all-helpers`.

;;;; ___________________________________________________________________________

(require 'nomis-clj-refactor-deduce-hydras-tests)

(provide 'nomis-clj-refactor-deduce-hydras)
