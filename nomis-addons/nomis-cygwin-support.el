;;; nomis-cygwin-support.el

;;; Copied from http://www.emacswiki.org/emacs/NTEmacsWithCygwin
;;; which says...
;;; - The following code will produce a reasonable approximation of
;;;   Unix Emacs on NT:
;;; - This code first verifies it is running under windows with cygwin
;;;   installed, so this snippet can be safely placed in a .emacs file
;;;   that is also used on emacs in unices.
;;;   You will also need cygwin-mount.el if you want Emacs to
;;;   understand Cygwin paths.
;;; - Note that this is substantially the same as the bit of code given
;;;   in the Cygwin FAQ:
;;;   http://www.cygwin.com/faq/faq.using.html#faq.using.ntemacs


;;;;
;;;; cygwin support
;;;;
;; Sets your shell to use cygwin's bash, if Emacs finds it's running
;; under Windows and c:\cygwin exists. Assumes that C:\cygwin\bin is
;; not already in your Windows Path (it generally should not be).
;;
(let* ((cygwin-root "c:/cygwin")
       (cygwin-bin (concat cygwin-root "/bin")))
  (when (and (eq 'windows-nt system-type)
	     (file-readable-p cygwin-root))
  
    (setq exec-path (cons cygwin-bin exec-path))
    (setenv "PATH" (concat cygwin-bin ";" (getenv "PATH")))
  
    ;; By default use the Windows HOME.
    ;; Otherwise, uncomment below to set a HOME
    ;;      (setenv "HOME" (concat cygwin-root "/home/eric"))
  
    ;; NT-emacs assumes a Windows shell. Change to baash.
    (setq shell-file-name "bash")
    (setenv "SHELL" shell-file-name) 
    (setq explicit-shell-file-name shell-file-name) 
  
    ;; This removes unsightly ^M characters that would otherwise
    ;; appear in the output of java applications.
    (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)))

;;;; Stuff added by SK:

(require 'cygwin-mount)

(provide 'nomis-cygwin-support.el)
