;;;; Ibit stuff -- OS X environment

;;;; From https://github.com/technomancy/swank-clojure:

;;; On Mac OS X, Emacs sessions launched from the GUI don't always
;;; respect your configured $PATH. If Emacs can't find lein, you may
;;; need to give it some help. The quickest way is probably to add
;;; this elisp to your config:

;;; FIXME: I can't make this work -- the path has not had my .bashrc
;; stuff added.
;; (setenv "PATH" (shell-command-to-string "echo $PATH"))
;; (shell-command-to-string "echo $PS1")
;; (progn shell-file-name)
;;; So hack like this instead for now:
(setenv "PATH" (concat "~/bin:"
                       "/usr/local/bin:"
                       (shell-command-to-string "echo $PATH")))

(provide 'nomis-environment-os-x)
