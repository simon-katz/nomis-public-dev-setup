;;;; Init stuff -- clj-refactor -- deduce hydras -- tests

;;;; ___________________________________________________________________________

(ert-deftest nomis/cljr--def-hydras/works-with-ruby-style-doc-strings ()
  (should (equal (let* ((nomis/cljr--hydra/use-ruby-style-doc-strings? t))
                   (macroexpand-1 '(nomis/cljr--hydra/def-hydras)))
                 '(progn
                    (defhydra nomis/cljr--hydra/ns-menu
                      (:color pink :hint nil)
                      "
ns-related refactorings
-----------------------
_ai_: Add import to ns
_am_: Add missing libspec
_ap_: Add project dependency
_ar_: Add require to ns
_au_: Add use to ns
_cn_: Clean ns
_rm_: Add to or extend the require-macros form
_sr_: Stop referring
"
                      ("ai" cljr-add-import-to-ns :exit t)
                      ("am" cljr-add-missing-libspec :exit t)
                      ("ap" cljr-add-project-dependency :exit t)
                      ("ar" cljr-add-require-to-ns :exit t)
                      ("au" cljr-add-use-to-ns :exit t)
                      ("cn" cljr-clean-ns :exit t)
                      ("rm" cljr-require-macro :exit t)
                      ("sr" cljr-stop-referring :exit t)
                      ("q" nil "quit"))
                    (defhydra nomis/cljr--hydra/code-menu
                      (:color pink :hint nil)
                      "
code-related refactorings
-------------------------
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
                      ("ci" clojure-cycle-if :exit t)
                      ("ct" cljr-cycle-thread :exit t)
                      ("dk" cljr-destructure-keys :exit t)
                      ("el" cljr-expand-let :exit t)
                      ("fu" cljr-find-usages :exit t)
                      ("il" cljr-introduce-let :exit t)
                      ("is" cljr-inline-symbol :exit t)
                      ("ml" cljr-move-to-let :exit t)
                      ("pf" cljr-promote-function :exit t)
                      ("rl" cljr-remove-let :exit t)
                      ("rs" cljr-rename-symbol :exit t)
                      ("tf" clojure-thread-first-all :exit t)
                      ("th" clojure-thread :exit t)
                      ("tl" clojure-thread-last-all :exit t)
                      ("ua" clojure-unwind-all :exit t)
                      ("uw" clojure-unwind :exit t)
                      ("q" nil "quit"))
                    (defhydra nomis/cljr--hydra/project-menu
                      (:color pink :hint nil)
                      "
project-related refactorings
----------------------------
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
                      ("ap" cljr-add-project-dependency :exit t)
                      ("cs" cljr-change-function-signature :exit t)
                      ("fu" cljr-find-usages :exit t)
                      ("hd" cljr-hotload-dependency :exit t)
                      ("is" cljr-inline-symbol :exit t)
                      ("mf" cljr-move-form :exit t)
                      ("pc" cljr-project-clean :exit t)
                      ("rf" cljr-rename-file-or-dir :exit t)
                      ("rs" cljr-rename-symbol :exit t)
                      ("sp" cljr-sort-project-dependencies :exit t)
                      ("up" cljr-update-project-dependencies :exit t)
                      ("q" nil "quit"))
                    (defhydra nomis/cljr--hydra/toplevel-form-menu
                      (:color pink :hint nil)
                      "
toplevel-form-related refactorings
----------------------------------
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
                      ("as" cljr-add-stubs :exit t)
                      ("cp" clojure-cycle-privacy :exit t)
                      ("cs" cljr-change-function-signature :exit t)
                      ("ec" cljr-extract-constant :exit t)
                      ("ed" cljr-extract-def :exit t)
                      ("ef" cljr-extract-function :exit t)
                      ("fe" cljr-create-fn-from-example :exit t)
                      ("is" cljr-inline-symbol :exit t)
                      ("mf" cljr-move-form :exit t)
                      ("pf" cljr-promote-function :exit t)
                      ("rf" cljr-rename-file-or-dir :exit t)
                      ("ad" cljr-add-declaration :exit t)
                      ("q" nil "quit"))
                    (defhydra nomis/cljr--hydra/cljr-menu
                      (:color pink :hint nil)
                      "
cljr-related refactorings
-------------------------
_sc_: Show the project's changelog
_?_: Describe refactoring
"
                      ("sc" cljr-show-changelog :exit t)
                      ("?" cljr-describe-refactoring :exit t)
                      ("q" nil "quit"))
                    (defhydra nomis/cljr--hydra/help-menu
                      (:color pink :hint nil)
                      "
Available refactoring types
---------------------------
_n_: ns-related refactorings
_c_: code-related refactorings
_p_: project-related refactorings
_t_: toplevel-form-related refactorings
_s_: cljr-related refactorings
"
                      ("n" nomis/cljr--hydra/ns-menu/body :exit t)
                      ("c" nomis/cljr--hydra/code-menu/body :exit t)
                      ("p" nomis/cljr--hydra/project-menu/body :exit t)
                      ("t" nomis/cljr--hydra/toplevel-form-menu/body :exit t)
                      ("s" nomis/cljr--hydra/cljr-menu/body :exit t)
                      ("q" nil "quit" :color blue))))))

(ert-deftest nomis/cljr--def-hydras/works-with-simple-doc-strings ()
  (should (equal (let* ((nomis/cljr--hydra/use-ruby-style-doc-strings? nil))
                   (macroexpand-1 '(nomis/cljr--hydra/def-hydras)))
                 '(progn
                    (defhydra nomis/cljr--hydra/ns-menu
                      (:color pink :hint nil)
                      "ns-related refactorings"
                      ("ai" cljr-add-import-to-ns "Add import to ns" :exit t)
                      ("am" cljr-add-missing-libspec "Add missing libspec" :exit t)
                      ("ap" cljr-add-project-dependency "Add project dependency" :exit t)
                      ("ar" cljr-add-require-to-ns "Add require to ns" :exit t)
                      ("au" cljr-add-use-to-ns "Add use to ns" :exit t)
                      ("cn" cljr-clean-ns "Clean ns" :exit t)
                      ("rm" cljr-require-macro "Add to or extend the require-macros form" :exit t)
                      ("sr" cljr-stop-referring "Stop referring" :exit t)
                      ("q" nil "quit"))
                    (defhydra nomis/cljr--hydra/code-menu
                      (:color pink :hint nil)
                      "code-related refactorings"
                      ("ci" clojure-cycle-if "Cycle if" :exit t)
                      ("ct" cljr-cycle-thread "Cycle thread" :exit t)
                      ("dk" cljr-destructure-keys "Destructure keys" :exit t)
                      ("el" cljr-expand-let "Expand let" :exit t)
                      ("fu" cljr-find-usages "Find usages" :exit t)
                      ("il" cljr-introduce-let "Introduce let" :exit t)
                      ("is" cljr-inline-symbol "Inline symbol" :exit t)
                      ("ml" cljr-move-to-let "Move to let" :exit t)
                      ("pf" cljr-promote-function "Promote function" :exit t)
                      ("rl" cljr-remove-let "Remove let" :exit t)
                      ("rs" cljr-rename-symbol "Rename symbol" :exit t)
                      ("tf" clojure-thread-first-all "Thread first all" :exit t)
                      ("th" clojure-thread "Thread" :exit t)
                      ("tl" clojure-thread-last-all "Thread last all" :exit t)
                      ("ua" clojure-unwind-all "Unwind all" :exit t)
                      ("uw" clojure-unwind "Unwind" :exit t)
                      ("q" nil "quit"))
                    (defhydra nomis/cljr--hydra/project-menu
                      (:color pink :hint nil)
                      "project-related refactorings"
                      ("ap" cljr-add-project-dependency "Add project dependency" :exit t)
                      ("cs" cljr-change-function-signature "Change function signature" :exit t)
                      ("fu" cljr-find-usages "Find usages" :exit t)
                      ("hd" cljr-hotload-dependency "Hotload dependency" :exit t)
                      ("is" cljr-inline-symbol "Inline symbol" :exit t)
                      ("mf" cljr-move-form "Move form" :exit t)
                      ("pc" cljr-project-clean "Project clean" :exit t)
                      ("rf" cljr-rename-file-or-dir "Rename file-or-dir" :exit t)
                      ("rs" cljr-rename-symbol "Rename symbol" :exit t)
                      ("sp" cljr-sort-project-dependencies "Sort project dependencies" :exit t)
                      ("up" cljr-update-project-dependencies "Update project dependencies" :exit t)
                      ("q" nil "quit"))
                    (defhydra nomis/cljr--hydra/toplevel-form-menu
                      (:color pink :hint nil)
                      "toplevel-form-related refactorings"
                      ("as" cljr-add-stubs "Add stubs for the interface/protocol at point" :exit t)
                      ("cp" clojure-cycle-privacy "Cycle privacy" :exit t)
                      ("cs" cljr-change-function-signature "Change function signature" :exit t)
                      ("ec" cljr-extract-constant "Extract constant" :exit t)
                      ("ed" cljr-extract-def "Extract form as def" :exit t)
                      ("ef" cljr-extract-function "Extract function" :exit t)
                      ("fe" cljr-create-fn-from-example "Create function from example" :exit t)
                      ("is" cljr-inline-symbol "Inline symbol" :exit t)
                      ("mf" cljr-move-form "Move form" :exit t)
                      ("pf" cljr-promote-function "Promote function" :exit t)
                      ("rf" cljr-rename-file-or-dir "Rename file-or-dir" :exit t)
                      ("ad" cljr-add-declaration "Add declaration" :exit t)
                      ("q" nil "quit"))
                    (defhydra nomis/cljr--hydra/cljr-menu
                      (:color pink :hint nil)
                      "cljr-related refactorings"
                      ("sc" cljr-show-changelog "Show the project's changelog" :exit t)
                      ("?" cljr-describe-refactoring "Describe refactoring" :exit t)
                      ("q" nil "quit"))
                    (defhydra nomis/cljr--hydra/help-menu
                      (:color pink :hint nil)
                      "Available refactoring types"
                      ("n" nomis/cljr--hydra/ns-menu/body "ns-related refactorings" :exit t)
                      ("c" nomis/cljr--hydra/code-menu/body "code-related refactorings" :exit t)
                      ("p" nomis/cljr--hydra/project-menu/body "project-related refactorings" :exit t)
                      ("t" nomis/cljr--hydra/toplevel-form-menu/body "toplevel-form-related refactorings" :exit t)
                      ("s" nomis/cljr--hydra/cljr-menu/body "cljr-related refactorings" :exit t)
                      ("q" nil "quit" :color blue))))))

;;;; ___________________________________________________________________________

(provide 'nomis-clj-refactor-deduce-hydras-tests)
