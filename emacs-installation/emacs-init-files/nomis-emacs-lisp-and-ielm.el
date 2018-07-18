;;;; Init stuff -- emacs-lisp and ielm.

;;;; ___________________________________________________________________________

(add-hook 'emacs-lisp-mode-hook 'whitespace-mode)

(defvar nomis/lisp-and-ielm-mode-hook-functions
  `(rainbow-delimiters-mode
    paredit-mode
    paxedit-mode ; some commands (at least) don't work in ielm mode
    ,(lambda () (set (make-local-variable 'comment-column) 0))
    turn-on-elisp-slime-nav-mode
    turn-on-eldoc-mode
    ;; aggressive-indent-mode
    ))

(dolist (hook '(emacs-lisp-mode-hook
                ielm-mode-hook))
  (dolist (hook-fun nomis/lisp-and-ielm-mode-hook-functions)
    (add-hook hook hook-fun)))

(define-key emacs-lisp-mode-map (kbd "RET") 'newline-and-indent)

;;;; ___________________________________________________________________________
;;;; Pop-up help in Emacs Lisp.

(require 'popup)

(defun nomis/describe-thing-in-popup ()
  ;; Thanks to Bruce Durling, Kris Jenkins and Steve Purcell.
  ;; Based on code taken from
  ;; https://github.com/otfrom/otfrom-org-emacs/blob/master/org/config.org.
  ;; See also http://blog.jenkster.com/2013/12/popup-help-in-emacs-lisp.html.
  (interactive)
  (let* ((thing (symbol-at-point))
         (help-xref-following t)
         (description (with-temp-buffer
                        (help-mode)
                        (help-xref-interned thing)
                        (buffer-string))))
    ;; Provide a useful message, and overwrite the now-wrong message about
    ;; typing "q" in the help window.
    (message "Type \"C-g\" to dismiss popup.")
    (popup-tip description
               :point (point)
               :around t
               :height 30
               :scroll-bar t
               :margin t)))

(progn
  ;; Key binding.
  ;;
  ;; elisp-slime-nav-mode binds the following keys
  ;;   C-c C-d d
  ;;   C-c C-d C-d
  ;; to `elisp-slime-nav-describe-elisp-thing-at-point`, which displays
  ;; documentation in the *Help* buffer.
  ;; Here rebind only C-c C-d C-d, because it's useful to have both.
  (require 'elisp-slime-nav)
  (define-key elisp-slime-nav-mode-map (kbd "C-c C-d C-d") 'nomis/describe-thing-in-popup))

;;; ___________________________________________________________________________

(provide 'nomis-emacs-lisp-and-ielm)
