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
                                                                 :exit
                                                                 t))))
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

(ert-deftest nomis/cljr--def-hydras/works ()
  (should (equal (macroexpand-1 '(nomis/cljr--def-hydras))
                 '(progn
                    (defhydra nomis/hydra-cljr-ns-menu
                      (:color pink :hint nil)
                      "
ns-related refactorings
------------------------------------------------------------
_ai_: Add import to ns
_am_: Add missing libspec
_ap_: Add project dependency
_ar_: Add require to ns
_au_: Add use to ns
_cn_: Clean ns
_rm_: Add to or extend the require-macros form
_sr_: Stop referring
"
                      ("ai" cljr-add-import-to-ns)
                      ("am" cljr-add-missing-libspec)
                      ("ap" cljr-add-project-dependency)
                      ("ar" cljr-add-require-to-ns)
                      ("au" cljr-add-use-to-ns)
                      ("cn" cljr-clean-ns)
                      ("rm" cljr-require-macro)
                      ("sr" cljr-stop-referring)
                      ("q" nil "quit"))
                    (defhydra nomis/hydra-cljr-project-menu
                      (:color pink :hint nil)
                      "
project-related refactorings
------------------------------------------------------------
_ap_: Add project dependency
_cs_: Change function signature
_fu_: Find usages
_hd_: Hotload dependency
_is_: Inline symbol
_mf_: Move form
_pc_: Project clean
_rf_: Rename file-or-dir
_rs_: Rename symbol
_sp_: Sort project dependencies
_up_: Update project dependencies
"
                      ("ap" cljr-add-project-dependency)
                      ("cs" cljr-change-function-signature)
                      ("fu" cljr-find-usages)
                      ("hd" cljr-hotload-dependency)
                      ("is" cljr-inline-symbol)
                      ("mf" cljr-move-form)
                      ("pc" cljr-project-clean)
                      ("rf" cljr-rename-file-or-dir)
                      ("rs" cljr-rename-symbol)
                      ("sp" cljr-sort-project-dependencies)
                      ("up" cljr-update-project-dependencies)
                      ("q" nil "quit"))
                    (defhydra nomis/hydra-cljr-toplevel-form-menu
                      (:color pink :hint nil)
                      "
toplevel-form-related refactorings
------------------------------------------------------------
_as_: Add stubs for the interface/protocol at point
_cp_: Cycle privacy
_cs_: Change function signature
_ec_: Extract constant
_ed_: Extract form as def
_ef_: Extract function
_fe_: Create function from example
_is_: Inline symbol
_mf_: Move form
_pf_: Promote function
_rf_: Rename file-or-dir
_ad_: Add declaration
"
                      ("as" cljr-add-stubs)
                      ("cp" clojure-cycle-privacy)
                      ("cs" cljr-change-function-signature)
                      ("ec" cljr-extract-constant)
                      ("ed" cljr-extract-def)
                      ("ef" cljr-extract-function)
                      ("fe" cljr-create-fn-from-example)
                      ("is" cljr-inline-symbol)
                      ("mf" cljr-move-form)
                      ("pf" cljr-promote-function)
                      ("rf" cljr-rename-file-or-dir)
                      ("ad" cljr-add-declaration)
                      ("q" nil "quit"))
                    (defhydra nomis/hydra-cljr-code-menu
                      (:color pink :hint nil)
                      "
code-related refactorings
------------------------------------------------------------
_ci_: Cycle if
_ct_: Cycle thread
_dk_: Destructure keys
_el_: Expand let
_fu_: Find usages
_il_: Introduce let
_is_: Inline symbol
_ml_: Move to let
_pf_: Promote function
_rl_: Remove let
_rs_: Rename symbol
_tf_: Thread first all
_th_: Thread
_tl_: Thread last all
_ua_: Unwind all
_uw_: Unwind
"
                      ("ci" clojure-cycle-if)
                      ("ct" cljr-cycle-thread)
                      ("dk" cljr-destructure-keys)
                      ("el" cljr-expand-let)
                      ("fu" cljr-find-usages)
                      ("il" cljr-introduce-let)
                      ("is" cljr-inline-symbol)
                      ("ml" cljr-move-to-let)
                      ("pf" cljr-promote-function)
                      ("rl" cljr-remove-let)
                      ("rs" cljr-rename-symbol)
                      ("tf" clojure-thread-first-all)
                      ("th" clojure-thread)
                      ("tl" clojure-thread-last-all)
                      ("ua" clojure-unwind-all)
                      ("uw" clojure-unwind)
                      ("q" nil "quit"))
                    (defhydra nomis/hydra-cljr-cljr-menu
                      (:color pink :hint nil)
                      "
cljr-related refactorings
------------------------------------------------------------
_sc_: Show the project's changelog
_?_: Describe refactoring
"
                      ("sc" cljr-show-changelog)
                      ("?" cljr-describe-refactoring)
                      ("q" nil "quit"))
                    (defhydra nomis/hydra-cljr-help-menu
                      (:color pink :hint nil)
                      "
#### MAKE DOC STRING
Available refactoring types
-----------------------------------------------------------------------------
_n_: Ns related refactorings      _c_: Code related refactorings
_p_: Project related refactorings _t_: Top level forms related refactorings
_s_: Refactor related functions"
                      ("n" nomis/hydra-cljr-ns-menu/body :exit t)
                      ("p" nomis/hydra-cljr-project-menu/body :exit t)
                      ("t" nomis/hydra-cljr-toplevel-form-menu/body :exit t)
                      ("c" nomis/hydra-cljr-code-menu/body :exit t)
                      ("s" nomis/hydra-cljr-cljr-menu/body :exit t)
                      ("q" nil "quit" :color blue))))))

;;;; ___________________________________________________________________________

(provide 'nomis-clj-refactor-deduce-hydras)
