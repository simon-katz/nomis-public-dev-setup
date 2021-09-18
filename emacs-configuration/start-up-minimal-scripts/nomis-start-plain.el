;;; nomis-start-plain.el --- Plain Emacs quick starter      -*- lexical-binding: t; -*-

;; This file is a helper to start a plain Emacs environment.
;; To use this, start your Emacs with "emacs -q" and load this file.

;; It forces Emacs to load `.el' files rather than `.elc' files for more
;; readable backtrace.

(when (and (equal emacs-version "27.2")
           (eql system-type 'darwin))
  ;; See https://emacs.stackexchange.com/questions/68288/error-retrieving-https-elpa-gnu-org-packages-archive-contents and https://emacs.stackexchange.com/questions/60560/error-retrieving-https-elpa-gnu-org-packages-archive-contents-error-http-400#comment105217_62321
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

(require 'package)

(setq debug-on-error t
      no-byte-compile t
      package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/"))
      package-user-dir (expand-file-name (make-temp-name "nomis-tmp-elpa")
                                         user-emacs-directory)
      custom-file (expand-file-name "custom.el" package-user-dir))

(let* ((pkg-list '(;; Add package names here if you want more than a plain Emacs.
                   )))

  (when pkg-list
    (package-initialize)
    (package-refresh-contents))

  (mapc (lambda (pkg)
          (unless (package-installed-p pkg)
            (package-install pkg))
          (require pkg))
        pkg-list)

  (add-hook 'kill-emacs-hook `(lambda ()
                                (delete-directory ,package-user-dir t))))

(provide 'nomis-start-plain)
;;; nomis-start-plain.el ends here
