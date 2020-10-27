;;;; Init stuff -- CIDER extras 2 --  -*- lexical-binding: t -*-

;;;; ___________________________________________________________________________

(require 'nomis-clojure-test-files)

;;;; ___________________________________________________________________________

(cond
 ((member (nomis/cider-version)
          '("CIDER 0.24.0snapshot"))
  (advice-add
   'cider-repl-handler
   :around
   (lambda (orig-fun buffer)
     (let ((show-prompt t)
           (show-prefix t) ; THIS IS THE CHANGED BIT -- all `show-prefix` stuff
           )
       (nrepl-make-response-handler
        buffer
        (lambda (buffer value)
          (cider-repl-emit-result buffer value show-prefix)
          (setq show-prefix nil))
        (lambda (buffer out)
          (cider-repl-emit-stdout buffer out))
        (lambda (buffer err)
          (cider-repl-emit-stderr buffer err))
        (lambda (buffer)
          (when show-prompt
            (cider-repl-emit-prompt buffer)))
        nrepl-err-handler
        (lambda (buffer value content-type)
          (if-let* ((content-attrs (cadr content-type))
                    (content-type* (car content-type))
                    (handler (cdr (assoc content-type*
                                         cider-repl-content-type-handler-alist))))
              (setq show-prompt (funcall handler content-type buffer value nil t))
            (cider-repl-emit-result buffer value t t)))
        (lambda (buffer warning)
          (cider-repl-emit-stderr buffer warning)))))
   '((name . nomis/cider-avoid-multiple-result-prefixes))))
 ((version<= "0.26.1" (pkg-info-version-info 'cider))
  ;; I think this is now fixed.
  )
 (t
  (message-box
   "You need to fix `nomis/cider-avoid-multiple-result-prefixes` for this version of Cider.")))

;;;; ___________________________________________________________________________

(pushnew "deftest-msg" cider-test-defining-forms) ; a Wefarm Nabu thing

;;## ;;;; ___________________________________________________________________________
;;## ;;;; ---- nomis/cider-rearrange-string-into-lines ----
;;##
;;## ;;;; ****
;;## ;;;; + Ensure `nomis/grab-text' has no free variables.
;;## ;;;;
;;## ;;;; + Put all your code-manipulation Clojure functions in single file in
;;## ;;;;   a new project.
;;## ;;;;   And have proper tests of the code-manipulation code.
;;## ;;;;

;;## (defun get-string-from-file (filePath)
;;##   "Return FILEPATH's file content."
;;##   ;; http://xahlee.blogspot.co.uk/2010/09/elisp-read-file-content-in-one-shot.html
;;##   ;; which says:
;;##   ;;   thanks to “Pascal J Bourguignon”
;;##   ;;   and "TheFlyingDutchman <zzbba...@aol.com>". 2010-09-02
;;##   ;;
;;##   ;; I changed insert-file-contents to insert-file-contents-literally
;;##   (with-temp-buffer
;;##     (insert-file-contents-literally filePath)
;;##     (buffer-string)))

(defun nomis/cider-rearrange-string-into-lines (prefix)
  "Rearrange string into lines.
   Without a prefix argument, indent second and subsequent lines so
   that they line up sensibly with the first line.
   With a prefix argument, indent second and subsequent lines one
   character less as is the convention for Clojure doc strings
   (which is stupid)."
  (interactive "*P")
  (if nil ; (y-or-n-p "Use `fill-paragraph` instead?")
      (fill-paragraph)
    (let* ((string (nomis/grab-text
                    :top-level-p nil
                    :delete-p t))
           (clojure-form-as-string
            (format "(do (require '[com.nomistech.emacs-hacks-in-clojure :as ehic])
                         (ehic/rearrange-string-into-lines '%s %s %s))"
                    string
                    (+ (current-column)
                       (if prefix 0 1))
                    72)))
      (nomis/run-clojure-and-insert-result clojure-form-as-string))))

(define-key cider-mode-map (kbd "C-c C-g")
  'nomis/cider-rearrange-string-into-lines)

;;;; ___________________________________________________________________________

(provide 'nomis-cider-extras-2)
