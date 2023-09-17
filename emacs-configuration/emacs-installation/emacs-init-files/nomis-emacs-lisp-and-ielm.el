;;;; Init stuff -- emacs-lisp and ielm  -*- lexical-binding: t; -*-

;;;; ___________________________________________________________________________

(defvar nomis/lisp-and-ielm-mode-hook-functions
  `(rainbow-delimiters-mode
    paredit-mode
    paxedit-mode ; some commands (at least) don't work in ielm mode
    ;; See https://github.com/clojure-emacs/clojure-mode/issues/516#issuecomment-569336063
    ,(lambda () (set (make-local-variable 'comment-column) 0))
    turn-on-elisp-slime-nav-mode
    turn-on-eldoc-mode
    ;; aggressive-indent-mode
    ))

(dolist (hook '(emacs-lisp-mode-hook
                ielm-mode-hook
                lisp-data-mode-hook))
  (dolist (hook-fun nomis/lisp-and-ielm-mode-hook-functions)
    (add-hook hook hook-fun)))

(with-eval-after-load 'paredit
  ;; As of me upgrading
  ;;   from Emacs 28.1.3 to 28.2
  ;;   from Paredit 20191121.2328 to 20221127.1452
  ;; there's a conflict between Paredit and ielm: Pressing RETURN in an ielm
  ;; buffer invokes `paredit-RET`, which inserts a newline and does not
  ;; evaluate. There's no way to invoke `ielm-return` from the keyboard.
  ;;
  ;; Maybe take a look at `cider-repl-setup-paredit` for other ideas, but this
  ;; seems to fix things:
  (define-key paredit-mode-map (kbd "RET") nil)
  (define-key paredit-mode-map (kbd "C-j") 'paredit-newline)
  (with-eval-after-load 'ielm
    (define-key ielm-map (kbd "RET") 'ielm-return)))

;;;; ___________________________________________________________________________

(provide 'nomis-emacs-lisp-and-ielm)
