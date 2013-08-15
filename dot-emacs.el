;;;; ---- Emacs init file ----

(let ((expected-version "24.3.1")
      (version emacs-version))
  (unless (equal version expected-version)
    (unless (y-or-n-p (format (concat
                               "Things might not work. This Emacs init is"
                               " expecting Emacs %s, but this is Emacs %s."
                               " Type 'y' to continue or 'n' to exit.")
                              expected-version
                              version))
      (kill-emacs))))

;;;; ___________________________________________________________________________
;;;; ---- Package setup ----

(progn
  (require 'package)

  ;; Add Marmalade as a package archive source in ~/.emacs.d/init.e
  (add-to-list 'package-archives
               '("marmalade" . "http://marmalade-repo.org/packages/") t)

  (package-initialize)

  (when (not package-archive-contents)
    (package-refresh-contents))

  (defvar my-packages '(elisp-slime-nav
                        paredit
                        rainbow-delimiters
                        auto-complete
                        nrepl
                        ac-nrepl
                        clojure-mode
                        clojure-test-mode
                        saveplace
                        workgroups
                        fuzzy
                        htmlize
                        pos-tip
                        magit
                        ido-ubiquitous
                        smex
                        idle-highlight-mode)
    "A list of packages to ensure are installed at launch.")

  (dolist (p my-packages)
    (when (not (package-installed-p p))
      (package-install p))))

;;;; ___________________________________________________________________________

;;; TODO: Move to new file: "nomis-environment-os-x".

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

;;;; ___________________________________________________________________________
;;;; ---- load-path ----

(defun nomis-load-file-name ()
  (file-truename (or load-file-name (buffer-file-name))))

(let ((default-directory (file-name-directory (nomis-load-file-name))))
  (normal-top-level-add-subdirs-to-load-path))

;;;; ___________________________________________________________________________
;;;; ---- Load various files ----

(require 'homeless)

(require 'nomis-very-general-stuff)

(require 'nomis-dired)
(require 'dirtree) ; see https://github.com/zk/emacs-dirtree

(require 'nomis-ido)
(require 'nomis-smex)

(require 'nomis-ispell)
(require 'nomis-org-mode)

;; (require 'nomis-cygwin-support)

(require 'nomis-mac-keyboard-hacking)
(require 'nomis-normal-window-commands)
(require 'nomis-avoid-window-stealing)
(require 'nomis-mouse-scrolling)
(require 'nomis-highlighting)
(require 'nomis-line-numbering)
(require 'nomis-watch-words)

(require 'nomis-frames)
(require 'nomis-windows)
(require 'nomis-uniquify)

(require 'nomis-saveplace)
;; (require 'nomis-remember-desktop-using-windows)
(require 'nomis-remember-desktop-using-workgroups)
(require 'nomis-auto-complete)
(require 'nomis-keyboard-macros)
;; (require 'nomis-searching)
;; (require 'nomis-searching-filters)
(require 'nomis-paredit)
;; (require 'nomis-ibuffer)
(require 'nomis-indent-sexp)
(require 'nomis-define-lispy-modes)
(require 'nomis-emacs-lisp-mode)
(require 'nomis-clojure-mode)
(require 'nomis-slime-eval)
(require 'nomis-nrepl-tailoring)
(require 'nomis-nrepl-extras)
;; (require 'nomis-slime-fancy)
;; (require 'nomis-zip-files)
(require 'nomis-keyboard-scrolling-and-movement)

(require 'nomis-clojure-indentation)

(require 'nomis-shell-stuff)

;;;; ___________________________________________________________________________
;;;;; ---- temp for playing ----
