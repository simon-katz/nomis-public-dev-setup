;;;; Init stuff -- search filters.

;;;; TODO: Take a look at this.  What does it do?  Maybe from Windoze days?

;; ;;;; ___________________________________________________________________________
;; ;;;; ---- Stuff for grep-find / find-grep ----

;; (setq *nomis-grep-find-options*
;;       "\\\( \\\! -name \\\*.jar \\\) \\\( \\\! -name \\\*.class \\\) \\\( \\\! -name \\\#*# \\\)")

;; ;;;; ___________________________________________________________________________
;; ;;;; ---- Stuff for rgrep and lgrep ----

;; (progn
;;   ;; a hack -- really want to be able to change this within a session
;;   (defvar *extra-ignored-directories*
;;     '("labrepl*/public/javascripts/jquery.js"
;;       "emacs-configuration/nomis-addons/cygwin-mount.el"))
;;   (defvar *extra-ignored-files*
;;     '(".jar"
;;       ".exe"))
;;   (eval-after-load "grep"
;;     '(progn
;;        (mapc (lambda (x) (add-to-list 'grep-find-ignored-files x))
;;              *extra-ignored-files*)
;;        (mapc (lambda (x) (add-to-list 'grep-find-ignored-directories x))
;;              *extra-ignored-directories*))))

;;;; ___________________________________________________________________________

(provide 'nomis-searching-filters)
