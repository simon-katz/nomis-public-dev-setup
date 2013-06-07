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
                        saveplace
                        )
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
                       (shell-command-to-string "echo $PATH")))

;;;; ___________________________________________________________________________
;;;; ---- load-path ----

(defvar nomis-init-dir
  "~/Documents/jsk/development-100/__for-sync/code/nomis/emacs-configuration")

(defun add-dir-and-subdirs-to-load-path (base-dir)
  (add-to-list 'load-path base-dir)
  (dolist (f (directory-files base-dir))
    (let ((name (concat base-dir "/" f)))
      (when (and (file-directory-p name) 
                 (not (equal f ".."))
                 (not (equal f "."))
                 (not (equal f ".git")))
        (add-to-list 'load-path name)))))

(add-dir-and-subdirs-to-load-path nomis-init-dir)

;;;; ___________________________________________________________________________
;;;; ---- Load various files ----

(require 'nomis-init-very-general-stuff)
(require 'nomis-init-dired)

(require 'nomis-init-ispell)
(require 'nomis-init-org-mode)

;; (require 'nomis-cygwin-support)

(require 'nomis-init-mac-keyboard-hacking)
(require 'nomis-init-normal-window-commands)
(require 'nomis-init-avoid-window-stealing)
(require 'nomis-init-mouse-scrolling)
(require 'nomis-init-highlighting)
(require 'nomis-init-line-numbering)
(require 'nomis-init-watch-words)
(require 'nomis-init-frames)
(require 'nomis-init-saveplace)
;; (require 'nomis-init-remember-desktop-using-windows)
(require 'nomis-init-remember-desktop-using-workgroups)
(require 'nomis-init-auto-complete)
(require 'nomis-init-keyboard-macros)
;; (require 'nomis-init-searching)
;; (require 'nomis-init-searching-filters)
(require 'nomis-init-paredit)
;; (require 'nomis-init-ibuffer)
(require 'nomis-init-indent-sexp)
(require 'nomis-init-define-lispy-modes)
(require 'nomis-init-emacs-lisp-mode)
(require 'nomis-init-clojure-mode)
(require 'nomis-init-slime-eval)
(require 'nomis-init-nrepl-tailoring)
(require 'nomis-init-nrepl-extras)
;; (require 'nomis-init-slime-fancy)
;; (require 'nomis-init-zip-files)
(require 'nomis-init-keyboard-scrolling-and-movement)

(require 'nomis-init-clojure-indentation)

(require 'nomis-init-shell-stuff)

;;;; ___________________________________________________________________________
;;;;; ---- temp for playing ----
