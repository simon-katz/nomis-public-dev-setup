;;;; nomis-clj-refactor-deduce-hydras.el ---  -*- lexical-binding: t -*-

;;;; Init stuff -- clj-refactor -- deduce hydras


(defvar nomis/cljr--all-helpers-before-nomis-hacks cljr--all-helpers)

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

(defun nomis/cljr--hydra/type->heading (type)
  (concat type "-related refactorings"))

;;;; ___________________________________________________________________________
;;;; Hydras for individual refactoring types

(defun nomis/cljr--hydra/cljr-helpers->hydra-doc-string
    (type
     cljr-helpers
     use-ruby-style-doc-strings?)
  (let ((heading (nomis/cljr--hydra/type->heading type)))
    (if use-ruby-style-doc-strings?
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

(defun nomis/cljr--hydra/cljr-helpers->hydra-heads (cljr-helpers
                                                    use-ruby-style-doc-strings?)
  (append (->> cljr-helpers
               (-map (lambda (cljr-helper)
                       (append (list (car cljr-helper)
                                     (cadr cljr-helper))
                               (unless use-ruby-style-doc-strings?
                                 (list (caddr cljr-helper)))
                               (list 
                                ;; #### FIXME Maybe a bug you've not copied from clj-refactor
                                :exit t)))))
          '(("q" nil "quit"))))

(defun nomis/cljr--hydra/type->hydra-defining-form (type
                                                    use-ruby-style-doc-strings?)
  (let* ((cljr-helpers (nomis/cljr--hydra/type->cljr-helpers type)))
    `(defhydra ,(nomis/cljr--hydra/type->name type)
       (:color pink :hint nil)
       ,(nomis/cljr--hydra/cljr-helpers->hydra-doc-string
         type
         cljr-helpers
         use-ruby-style-doc-strings?)
       ,@(nomis/cljr--hydra/cljr-helpers->hydra-heads
          cljr-helpers
          use-ruby-style-doc-strings?))))

;;;; ___________________________________________________________________________
;;;; Top-level Hydra

(defun nomis/cljr--hydra/cljr-helpers->top-level-hydra-doc-string
    (all-types
     use-ruby-style-doc-strings?)
  (let ((heading "Available refactoring types"))
    (if use-ruby-style-doc-strings?
        (s-join "\n"
                (list ""
                      heading
                      (nomis/cljr--hydra/str->hyphens heading)
                      (s-join
                       "\n"
                       (-map (lambda (type)
                               (format "_%s_: %s"
                                       (nomis/cljr--hydra/type->command-key
                                        type)
                                       (nomis/cljr--hydra/type->heading type)))
                             all-types))
                      ""))
      heading)))

(defun nomis/cljr--hydra/cljr-helpers->top-level-hydra-heads
    (all-types
     use-ruby-style-doc-strings?)
  (append (->> all-types
               (-map (lambda (type)
                       (append
                        (list (nomis/cljr--hydra/type->command-key type)
                              (nomis/cljr--hydra/type->name-incl-body type))
                        (unless use-ruby-style-doc-strings?
                          (list (nomis/cljr--hydra/type->heading type)))
                        (list :exit t)))))
          '(("q" nil "quit" :color blue))))

(defun nomis/cljr--hydra/top-level-hydra-defining-form
    (all-types
     use-ruby-style-doc-strings?)
  `(defhydra nomis/cljr--hydra/help-menu
     (:color pink :hint nil)
     ,(nomis/cljr--hydra/cljr-helpers->top-level-hydra-doc-string
       all-types
       use-ruby-style-doc-strings?)
     ,@(nomis/cljr--hydra/cljr-helpers->top-level-hydra-heads
        all-types
        use-ruby-style-doc-strings?)))

;;;; ___________________________________________________________________________

(defmacro nomis/cljr--hydra/def-hydras (use-ruby-style-doc-strings?)
  (let* ((use-ruby-style-doc-strings? `,use-ruby-style-doc-strings?)
         (all-types (nomis/cljr--hydra/all-types)))
    `(progn
       ,@(-map (lambda (type)
                 (nomis/cljr--hydra/type->hydra-defining-form
                  type
                  use-ruby-style-doc-strings?))
               all-types)
       ,(nomis/cljr--hydra/top-level-hydra-defining-form
         all-types
         use-ruby-style-doc-strings?))))

(nomis/cljr--hydra/def-hydras nil)

(defalias 'hydra-cljr-help-menu/body 'nomis/cljr--hydra/help-menu/body)

;;;; ___________________________________________________________________________

;;;; FIXME #### Deal with updating `cljr--all-helpers`.
;;;;            (That was how you got started on this!)

(require 'nomis-clj-refactor-deduce-hydras-tests)

(provide 'nomis-clj-refactor-deduce-hydras)
