;;;; Init stuff -- Zip files.

;;;; ___________________________________________________________________________
;;;; ---- Fix problem with unzipping ----

;; See http://web.archiveorange.com/archive/v/UcW9CERUAfsCRJzpBo7s
;; - I tried making this change by using around advice in my init file
;;   to replace the function definition, but couldn't make it work
;; - Made the change by hacking the file
;;   .../emacs-23.2/lisp/arc-mode.el

;; (defadvice archive-zip-extract (around
;;                                 replacement-bug-fix
;;                                 (archive name))
;;   (if (equal (emacs-version)
;;              "GNU Emacs 23.2.1 (i386-mingw-nt5.1.2600)
;;  of 2010-05-08 on G41R2F1")
;;       (if (member-ignore-case (car archive-zip-extract) '("pkunzip" 
;; "pkzip")) 
;;       (archive-*-extract archive name archive-zip-extract) 
;;     (archive-extract-by-stdout 
;;      archive 
;;      ;; unzip expands wildcards in NAME, so we need to quote it. 
;;      ;; FIXME: Does pkunzip need similar treatment? 
;;      (if (and (not w32-quote-process-args) ;; <---- 
;;               (equal (car archive-zip-extract) "unzip") 
;;               ) 
;;          (shell-quote-argument name) 
;;        name) 
;;      archive-zip-extract))
;;     ad-do-it))

;; (ad-activate 'archive-zip-extract)

;;;; ___________________________________________________________________________

(provide 'jsk-init-zip-files)
