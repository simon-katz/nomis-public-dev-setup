;; -*- lexical-binding: t -*-
;;;; Init stuff -- auto-complete

;;;; ___________________________________________________________________________

;;;; See https://cider.readthedocs.io/en/latest/code_completion/

(global-company-mode)

(global-set-key (kbd "TAB") #'company-indent-or-complete-common)

(case 1
  ;; Only one of these makes sense. Which do you prefer?
  (1 (setq company-idle-delay nil))
  (2 (setq company-minimum-prefix-length 2)))

;;;; ___________________________________________________________________________
;;;; Avoid annoyance of completing when nothing to complete.

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
;;;; Make right arrow cycle through things in `company-active-map`.

(defconst nomis/company-show-info/functions
  (list #'company-show-doc-buffer
        #'company-show-location))

(defvar nomis/company-show-info/current-index 0)

(defun nomis/company-show-info/next-function ()
  (interactive)
  (let* ((i nomis/company-show-info/current-index))
    (setq nomis/company-show-info/current-index
          (mod (1+ i)
               (length nomis/company-show-info/functions)))
    (nth i nomis/company-show-info/functions)))

(defun nomis/company-show-info/call-next-function ()
  (interactive)
  (funcall (nomis/company-show-info/next-function)))

(define-key company-active-map
  (kbd "<right>") #'nomis/company-show-info/call-next-function)

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;;; Reset `nomis/company-show-info/current-index` for commands other than
;;;; info commands.

(defconst nomis/company-show-info/reset-functions
  (list #'company-complete-common
        #'company-select-next
        #'company-select-previous
        #'company-select-next-or-abort
        #'company-select-previous-or-abort
        #'company-next-page
        #'company-previous-page 
        #'company-filter-candidates))

(dolist (f nomis/company-show-info/reset-functions)
  (advice-add f
              :around
              (lambda (orig-fun &rest args)
                (apply orig-fun args)
                (setq nomis/company-show-info/current-index
                      0))
              `((name . reset-nomis/company-show-info/current-index))))

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
