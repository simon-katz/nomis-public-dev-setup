;;;; ---- Emacs init file ----

(let ((expected-version "24.2.1")
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

  (add-to-list 'package-archives
               '("marmalade" . "http://marmalade-repo.org/packages/") t)

  (package-initialize)

  (when (null package-archive-contents)
    (package-refresh-contents))

  (defvar my-packages '(elisp-slime-nav
                        cl-lib
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
;;;; ---- load-path ----

(defun nomis-load-file-name ()
  (file-truename (or load-file-name (buffer-file-name))))

(defun nomis-load-file-directory ()
  (file-name-directory (nomis-load-file-name)))

(let ((default-directory (nomis-load-file-directory)))
  (normal-top-level-add-subdirs-to-load-path))

;;;; ___________________________________________________________________________
;;;; ---- Compilation ----

;;;; It would be nice to compile things when necessary, but Emacs
;;;; relies on an "is source file newer than compiled file" check to
;;;; decide whether to recompile. This won't work with the way you
;;;; often replace source files with copies of old copies. But it's
;;;; nice to check for compilation errors every now and then.
;;;; 
;;;; For some reason, having these combined in a single function
;;;; means that deletion doesn't happen. So have two functions.

(defun nomis-compile-this-dir ()
  "Compile and delete the results."
  (byte-recompile-directory (nomis-load-file-directory) 0 t))

(defun nomis-delete-elc-files ()
  (shell-command (format "find \"%s\" -name *.elc -delete"
                         (nomis-load-file-directory))))

;;;; (nomis-compile-this-dir)
;;;; (nomis-delete-elc-files)

;;;; ___________________________________________________________________________
;;;; ---- Load various files ----

(require 'nomis-very-general-stuff)
(require 'nomis-mouse-scrolling)
(require 'nomis-avoid-window-stealing)
(require 'nomis-whitespace)
(require 'nomis-highlighting)
(require 'nomis-line-numbering)
(require 'nomis-saveplace)
(require 'nomis-ido)
(require 'nomis-smex)
(require 'nomis-frames)
(require 'nomis-windows)
(require 'nomis-uniquify)
(require 'nomis-remember-desktop-using-workgroups)
;;;; #### I'm happy up to here
(require 'nomis-mac-keyboard-hacking)
(require 'nomis-keyboard-scrolling-and-movement)

(require 'nomis-ispell)
(require 'nomis-watch-words)

(require 'nomis-dired)
(require 'dirtree) ; see https://github.com/zk/emacs-dirtree

(require 'nomis-org)

(require 'nomis-paredit)
(require 'nomis-emacs-lisp-and-ielm)
(require 'nomis-clojure)
(require 'nomis-clojure-indentation)
(require 'nomis-nrepl-tailoring)
(require 'nomis-nrepl-extras)

(require 'nomis-shell-stuff)

(require 'homeless)

(require 'nomis-auto-complete) ; TODO: Check this; maybe move nrepl stuff.

;; TODO: Consider these; maybe delete.
(require 'nomis-slime-eval)
(require 'nomis-indent-sexp)
(require 'nomis-keyboard-macros)
;; (require 'nomis-searching)
;; (require 'nomis-searching-filters)
;; (require 'nomis-ibuffer)
;; (require 'nomis-slime-fancy)
;; (require 'nomis-zip-files)

;;;; ___________________________________________________________________________
;;;;; ---- temp for playing ----
