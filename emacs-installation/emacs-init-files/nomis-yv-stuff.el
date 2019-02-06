;;;; Init stuff -- diff-mode.

;;;; ___________________________________________________________________________

(defvar nomis/doing-yv-dev-env-var-name "DOING_YV_DEV")

(defun nomis/toggle-doing-yv-dev ()
  (interactive)
  (let ((v (setenv nomis/doing-yv-dev-env-var-name
                   (if (getenv nomis/doing-yv-dev-env-var-name)
                       nil
                     "YES"))))
    (message "YV stuff turned %s"  (if v "on" "off"))))

(progn
  ;; This is hacky, but good enough for now.
  ;; May need further work when and if you do AWS stuff separate from YV.
  ;; Should tie in with `nomis/toggle-doing-yv-dev`.
  ;; But how would you turn off?
  (setenv "YV_ENVIRONMENT" "san01")
  (setenv "AWS_DEFAULT_REGION" "eu-west-1")
  (setenv "AWS_REGION" "eu-west-1") ; needed by co.wrisk.jcredstash/jcredstash
  (setenv "AWS_ACCESS_KEY_ID"
          (-> (shell-command-to-string "aws configure get aws_access_key_id")
              s-trim-right))
  (setenv "AWS_SECRET_ACCESS_KEY"
          (-> (shell-command-to-string "aws configure get aws_secret_access_key")
              s-trim-right)))

;;;; ___________________________________________________________________________

(provide 'nomis-yv-stuff)
