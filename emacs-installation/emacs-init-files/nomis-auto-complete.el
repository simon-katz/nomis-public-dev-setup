;;;; Init stuff -- auto-complete

;;;; ___________________________________________________________________________

;;;; See https://cider.readthedocs.io/en/latest/code_completion/

(global-company-mode)

(global-set-key (kbd "TAB") #'company-indent-or-complete-common)

(case 1
  ;; Only one of these makes sense. Which do you prefer?
  (1 (setq company-idle-delay nil))
  (2 (setq company-minimum-prefix-length 2)))

(cond 
 ((member (company-version)
          '("0.9.6"))
  (advice-add 'company-calculate-candidates
              :around
              (lambda (orig-fun &rest args)  
                (if (equal args '(""))
                    (progn
                      (nomis/beep)  
                      (error "Not doing completion when there's nothing to complete")) 
                  (apply orig-fun args)))
              `((name . if-no-prefix-do-nothing))))
 (t
  (message-box
   "You need to fix `if-no-prefix-do-nothing` advice on `company-calculate-candidates` for this version of Company.")))

;;;; ___________________________________________________________________________
;;;; company-quickhelp

(company-quickhelp-mode)

(setq company-quickhelp-delay nil)

(define-key company-active-map (kbd "<right>") #'company-quickhelp-manual-begin)

;;;; ___________________________________________________________________________
;;;; ---- Helm ----
;;;; Maybe Helm is good, but I don't like it much at first glance.
;;;; It does have some useful stuff though (eg live grepping (`helm-find-files`
;;;; and then C-s, or C-u C-s for recursive), so keep it around.
;;;; Also `helm-resume` to return to the grep results.

;;;; For more, see http://tuhdo.github.io/helm-intro.html

;; (require 'helm)
;; (require 'helm-config)

(provide 'nomis-auto-complete)
