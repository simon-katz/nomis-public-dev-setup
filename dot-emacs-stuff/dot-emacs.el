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
               '("marmalade" . "http://marmalade-repo.org/packages/")
               t)

  (add-to-list 'package-archives
               '("melpa" . "http://melpa.milkbox.net/packages/")
               t)

  (package-initialize)

  (when (null package-archive-contents)
    (package-refresh-contents))

  (defvar my-packages '(elisp-slime-nav
                        cl-lib
                        paredit
                        rainbow-delimiters
                        auto-complete
                        saveplace
                        workgroups
                        fuzzy
                        htmlize
                        pos-tip
                        magit
                        ido-ubiquitous
                        smex
                        idle-highlight-mode
                        scala-mode2
                        undo-tree
                        cider
                        ac-nrepl
                        clojure-mode
                        clojure-test-mode)
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

;; ;;;; It would be nice to compile things when necessary, but Emacs
;; ;;;; relies on an "is source file newer than compiled file" check to
;; ;;;; decide whether to recompile. This won't work with the way you
;; ;;;; often replace source files with copies of old copies. But it's
;; ;;;; nice to check for compilation errors every now and then.
;; ;;;; 
;; ;;;; For some reason, having these combined in a single function
;; ;;;; means that deletion doesn't happen. So have two functions.

;; (defun nomis-delete-elc-files (dir)
;;   (shell-command (format "find \"%s\" -name *.elc -delete"
;;                          dir)))

;; (defun nomis-compile-dir (dir)
;;   (byte-recompile-directory dir 0 t))

;; (defvar *this-dir*
;;   (nomis-load-file-directory))

;; (defvar *emacs-config-dir*
;;   (file-truename (concat *this-dir* "../")))

;; (defun nomis-compile-emacs-config ()
;;   (interactive)
;;   (nomis-compile-dir *emacs-config-dir*))

;; ;;;; (nomis-delete-elc-files *emacs-config-dir*)
;; ;;;; (nomis-compile-dir *emacs-config-dir*)

;;;; ___________________________________________________________________________
;;;; ---- Load various files ----

;;;; ---- Lisp and Clojure stuff probably good for anyone ----

(require 'nomis-paredit)
(require 'nomis-emacs-lisp-and-ielm)
(require 'nomis-clojure)

(require 'nomis-auto-complete)

;;;; ---- Lisp and Clojure stuff probably only for me ----

(require 'nomis-indent-sexp)

(require 'nomis-slime-eval)

;;;; ---- Other stuff ----

(require 'nomis-very-general-stuff)
(require 'nomis-mouse-scrolling)
(require 'nomis-avoid-window-stealing)
(require 'nomis-right-margin)
(require 'nomis-highlighting)
(require 'nomis-line-numbering)
(require 'nomis-saveplace)
(require 'nomis-ido)
(require 'nomis-smex)
(require 'nomis-frames)
(require 'nomis-windows)
(require 'nomis-uniquify)
(require 'nomis-remember-desktop-using-workgroups)

(require 'nomis-mac-keyboard-hacking)
(require 'nomis-undo-tree)
(require 'nomis-diff-mode)

(require 'nomis-keyboard-scrolling-and-movement)

(require 'nomis-ispell)
(require 'nomis-watch-words)

(require 'nomis-dired)
(require 'nomis-dirtree)

(require 'nomis-org)

(require 'nomis-shell-stuff)

(require 'nomis-keyboard-macros)
(require 'nomis-searching)
(require 'nomis-searching-filters)
(require 'nomis-ibuffer)
(require 'nomis-ace-jump-mode)

(require 'nomis-multi-web-mode)

(require 'homeless)

(progn
  ;; Putting this where it belongs (in "nomis-very-general-stuff") doesn't work;
  ;; I guess something blats it.
  ;; Ah! This was probably because of issues with source files vs compiled
  ;; files, so try again.
  (define-key global-map [(insert)] nil))

;;;; ___________________________________________________________________________
;;;;; ---- temp for playing ----
