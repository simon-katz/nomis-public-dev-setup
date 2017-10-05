;;;; Init stuff -- clj-refactor -- deduce hydras

;;;; ___________________________________________________________________________

(require 's)
(require 'dash)
(require 'ht)

(defconst nomis/cljr--hydra/type->command-key/alist
  '(("ns"            . "n")
    ("code"          . "c")
    ("project"       . "p")
    ("toplevel-form" . "t")
    ("cljr"          . "s")))

(defconst nomis/cljr--hydra/types
  (-map 'car nomis/cljr--hydra/type->command-key/alist))

(defconst nomis/cljr--hydra/type->command-key/ht
  (ht<-alist nomis/cljr--hydra/type->command-key/alist))

(defconst nomis/cljr--hydra/doc-string-separator
  (make-string 60 ?-))

(defun nomis/cljr--hydra/all-types ()
  (->> cljr--all-helpers
       (-map (lambda (cljr-helper) (-last-item (cdr cljr-helper))))
       (-flatten)
       (-distinct)
       (-remove-item "hydra")
       (-sort (lambda (x y)
                (< (position x nomis/cljr--hydra/types :test 'equal)
                   (position y nomis/cljr--hydra/types :test 'equal))))))

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

;;;; ___________________________________________________________________________
;;;; Hydras for individual refactoring types

(defun nomis/cljr--hydra/cljr-helpers->hydra-doc-string (cljr-helpers)
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
                             ;; #### FIXME Maybe a bug you've not copied from clj-refactor
                             :exit t))))
          '(("q" nil "quit"))))

(defun nomis/cljr--hydra/type->hydra-defining-form (type)
  (let* ((cljr-helpers (nomis/cljr--hydra/type->cljr-helpers type)))
    `(defhydra ,(nomis/cljr--hydra/type->name type)
       (:color pink :hint nil)
       ,(nomis/cljr--hydra/cljr-helpers->hydra-doc-string cljr-helpers)
       ,@(nomis/cljr--hydra/cljr-helpers->hydra-heads cljr-helpers))))

;;;; ___________________________________________________________________________
;;;; Top-level Hydra

(defun nomis/cljr--hydra/cljr-helpers->top-level-hydra-doc-string (all-types)
  (s-join "\n"
          (list
           ""
           "#### MAKE DOC STRING"
           "Available refactoring types"
           "-----------------------------------------------------------------------------"
           "_n_: Ns related refactorings"
           "_c_: Code related refactorings"
           "_p_: Project related refactorings"
           "_t_: Top level forms related refactorings"
           "_s_: Refactor related functions")))

(defun nomis/cljr--hydra/cljr-helpers->top-level-hydra-heads (all-types)
  (append (->> all-types
               (-map (lambda (type)
                       (list (nomis/cljr--hydra/type->command-key type)
                             (nomis/cljr--hydra/type->name-incl-body type)
                             :exit t))))
          '(("q" nil "quit" :color blue))))

(defun nomis/cljr--hydra/top-level-hydra-defining-form (all-types)
  `(defhydra nomis/cljr--hydra/help-menu
     (:color pink :hint nil)
     ,(nomis/cljr--hydra/cljr-helpers->top-level-hydra-doc-string all-types)
     ,@(nomis/cljr--hydra/cljr-helpers->top-level-hydra-heads all-types)))

;;;; ___________________________________________________________________________

(defmacro nomis/cljr--hydra/def-hydras ()
  (let ((all-types (nomis/cljr--hydra/all-types)))
    `(progn
       ,@(-map 'nomis/cljr--hydra/type->hydra-defining-form
               all-types)
       ,(nomis/cljr--hydra/top-level-hydra-defining-form all-types))))

(nomis/cljr--hydra/def-hydras)

;;;; FIXME #### Deal with updating `cljr--all-helpers`.
;;;;            (That was how you got started on this!)


(define-key global-map (kbd "C-M-}") 'nomis/cljr--hydra/help-menu/body) ; FIXME #### Temp

;;;; ___________________________________________________________________________

(require 'nomis-clj-refactor-deduce-hydras-tests)

(provide 'nomis-clj-refactor-deduce-hydras)
