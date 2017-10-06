;;;; nomis-clj-refactor-deduce-hydras.el ---  -*- lexical-binding: t -*-

;;;; Init stuff -- clj-refactor -- deduce hydras

;;;; ___________________________________________________________________________

(require 's)
(require 'dash)
(require 'ht)

(defconst nomis/cljr--hydra/use-ruby-style-doc-strings? nil)

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

(defun nomis/cljr--hydra/str->hyphens (str)
  "Return a string of hyphens which is the same length as `str`."
  (make-string (length str)
               ?-))

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

(defun nomis/cljr--hydra/cljr-helpers->hydra-doc-string (type cljr-helpers)
  (let ((heading (concat type "-related refactorings")))
    (if nomis/cljr--hydra/use-ruby-style-doc-strings?
        (s-join "\n"
                (list ""
                      heading
                      (nomis/cljr--hydra/str->hyphens heading)
                      (s-join "\n"
                              (->> cljr-helpers
                                   (-map (lambda (cljr-helper)
                                           (format "_%s_: %s"
                                                   (car cljr-helper)
                                                   (caddr cljr-helper))))))
                      ""))
      heading)))

(defun nomis/cljr--hydra/cljr-helpers->hydra-heads (cljr-helpers)
  (append (->> cljr-helpers
               (-map (lambda (cljr-helper)
                       (append (list (car cljr-helper)
                                     (cadr cljr-helper))
                               (unless nomis/cljr--hydra/use-ruby-style-doc-strings?
                                 (list (caddr cljr-helper)))
                               (list 
                                
                                ;; #### FIXME Maybe a bug you've not copied from clj-refactor
                                :exit t)))))
          '(("q" nil "quit"))))

(defun nomis/cljr--hydra/type->hydra-defining-form (type)
  (let* ((cljr-helpers (nomis/cljr--hydra/type->cljr-helpers type)))
    `(defhydra ,(nomis/cljr--hydra/type->name type)
       (:color pink :hint nil)
       ,(nomis/cljr--hydra/cljr-helpers->hydra-doc-string type cljr-helpers)
       ,@(nomis/cljr--hydra/cljr-helpers->hydra-heads cljr-helpers))))

;;;; ___________________________________________________________________________
;;;; Top-level Hydra

(defun nomis/cljr--hydra/cljr-helpers->top-level-hydra-doc-string (all-types)
  (let ((heading "Available refactoring types"))
    (if nomis/cljr--hydra/use-ruby-style-doc-strings?
        (s-join "\n"
                (list ""
                      heading
                      (nomis/cljr--hydra/str->hyphens heading)
                      (s-join "\n"
                              (->> all-types
                                   (-map (lambda (type)
                                           (format "_%s_: %s-related refactorings"
                                                   (nomis/cljr--hydra/type->command-key
                                                    type)
                                                   type)))))
                      ""))
      heading)))

(defun nomis/cljr--hydra/cljr-helpers->top-level-hydra-heads (all-types)
  (append (->> all-types
               (-map (lambda (type)
                       (append (list (nomis/cljr--hydra/type->command-key type)
                                     (nomis/cljr--hydra/type->name-incl-body type))
                               (unless nomis/cljr--hydra/use-ruby-style-doc-strings?
                                 (list (concat type "-related refactorings")))
                               (list :exit t)))))
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

(defalias 'hydra-cljr-help-menu/body 'nomis/cljr--hydra/help-menu/body)

;;;; ___________________________________________________________________________

;;;; FIXME #### Deal with updating `cljr--all-helpers`.
;;;;            (That was how you got started on this!)

(require 'nomis-clj-refactor-deduce-hydras-tests)

(provide 'nomis-clj-refactor-deduce-hydras)
