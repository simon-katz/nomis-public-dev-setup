;;;; ---- Emacs init file ----


;;;; ___________________________________________________________________________
;;;; ---- Emacs Starter Kit ----

(progn
  ;; From https://github.com/technomancy/emacs-starter-kit

  ;; Add Marmalade as a package archive source in ~/.emacs.d/init.e

  (require 'package)
  (add-to-list 'package-archives
               '("marmalade" . "http://marmalade-repo.org/packages/") t)
  (package-initialize)

  ;; It's recommended to create a list of packages in init.el which will be
  ;; installed if they are found to not be present:

  (when (not package-archive-contents)
    (package-refresh-contents))

  ;; Add in your own as you wish:

  (defvar my-packages '(starter-kit
                        starter-kit-lisp
                        starter-kit-bindings
                        clojure-mode
                        clojure-test-mode
                        saveplace
                        workgroups
                        auto-complete
                        ac-nrepl
                        rainbow-delimiters
                        fuzzy
                        htmlize
                        pos-tip)
    "A list of packages to ensure are installed at launch.")

  (dolist (p my-packages)
    (when (not (package-installed-p p))
      (package-install p)))

  ;; There are a few conventions for naming files which will get loaded automatically. ~/.emacs.d/$USER.el as well as any files in the ~/.emacs.d/$USER/ directory. Finally, the Starter Kit will look for a file named after the current hostname ending in ".el" which will allow host-specific configuration.

  ;; The Starter Kit used to be a git repository that you checked out and used as your own personal .emacs.d directory, but it's been restructured so that it can be treated like any other package, freeing you up to structure your .emacs.d directory as you wish. See "Upgrading" below.
  )

;;;; ___________________________________________________________________________

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
