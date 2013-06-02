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

(defvar nomis-addons-dir
  (concat nomis-init-dir
          "/nomis-addons"))

(setq load-path (append (list nomis-init-dir
                              nomis-addons-dir)
                        load-path))

;;;; ___________________________________________________________________________
;;;; ---- Load various files ----

(load "nomis-init-very-general-stuff.el")
(load "nomis-init-dired.el")

(load "nomis-init-ispell")
(load "nomis-init-org-mode")

;; (require 'nomis-cygwin-support.el)

(load "nomis-init-mac-keyboard-hacking.el")
(load "nomis-init-normal-window-commands.el")
(load "nomis-init-avoid-window-stealing.el")
(load "nomis-init-mouse-scrolling.el")
(load "nomis-init-highlighting.el")
(load "nomis-init-line-numbering.el")
(load "nomis-init-watch-words.el")
(load "nomis-init-frames.el")
(load "nomis-init-saveplace.el")
;; (load "nomis-init-remember-desktop-using-windows.el")
(load "nomis-init-remember-desktop-using-workgroups.el")
(load "nomis-init-auto-complete.el")
(load "nomis-init-keyboard-macros.el")
;; (load "nomis-init-searching.el")
;; (load "nomis-init-searching-filters.el")
(load "nomis-init-paredit.el")
;; (load "nomis-init-ibuffer.el")
(load "nomis-init-indent-sexp.el")
(load "nomis-init-define-lispy-modes.el")
(load "nomis-init-emacs-lisp-mode.el")
(load "nomis-init-clojure-mode.el")
(load "nomis-init-slime-eval.el")
(load "nomis-init-nrepl-tailoring.el")
(load "nomis-init-nrepl-extras.el")
;; (load "nomis-init-slime-fancy.el")
;; (load "nomis-init-zip-files.el")
(load "nomis-init-keyboard-scrolling-and-movement.el")

(load "nomis-init-clojure-indentation.el")

(load "nomis-shell-stuff.el")

;;;; ___________________________________________________________________________
;;;;; ---- temp for playing ----
