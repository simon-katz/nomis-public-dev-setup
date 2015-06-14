;;;; ___________________________________________________________________________
;;;; ---- Duh ----

;; Now using exec-path-from-shell

;; (add-to-list 'exec-path "/usr/local/bin")
;; (add-to-list 'exec-path "~/bin")

;;;; ___________________________________________________________________________
;;;; ---- Emacs setup -- Tailor the installation ----

(load-file-relative-to-this-file "ensure-expected-emacs-version")
(load-file-relative-to-this-file "set-up-package-stuff")

(defun nomis-load-file-name ()
  (file-truename (or load-file-name (buffer-file-name))))

(defun nomis-load-file-directory ()
  (file-name-directory (nomis-load-file-name)))

;;;; ___________________________________________________________________________
;;;; ---- exec-path-from-shell ----

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;;;; ___________________________________________________________________________
;;;; ---- i-am-nomis-p ----

(defvar nomis-personal-emacs-init-file
  (concat (nomis-load-file-directory)
          "../../emacs-configuration-personal/nomis-personal-emacs-init.el"))

(defvar i-am-nomis-p
  (file-exists-p nomis-personal-emacs-init-file))

;;;; ___________________________________________________________________________

(defun add-dir-and-normal-subdirs-to-load-path (dir)
  (add-to-list 'load-path dir)
  (let* ((default-directory dir))
    (normal-top-level-add-subdirs-to-load-path)))

(add-dir-and-normal-subdirs-to-load-path
 (concat (nomis-load-file-directory)
         "emacs-init-files"))

(when i-am-nomis-p ; #### What about compiling?
  (dolist (d (directory-files (concat (nomis-load-file-directory)
                                      "../../emacs-package-repos/")
                              t
                              "[^\\.].*"))
    (add-to-list 'load-path d)))

;;;; ___________________________________________________________________________
;;;; ---- Load various files ----

(require 'nomis-org) ; When this was later in the file my setting of
                     ; `org-replace-disputed-keys` wasn't working.
                     ; Something must be loading org-mode.
                     ; (That didn't used to happen. It's a change in the
                     ; last few days.)

;;;; ---- Lisp and Clojure stuff probably good for anyone ----

(require 'nomis-paredit)
(require 'nomis-emacs-lisp-and-ielm)
(require 'nomis-clojure)

(require 'nomis-auto-complete)

;;;; ---- Lisp and Clojure stuff probably only for me ----

;; (require 'nomis-indent-sexp)

;; (require 'nomis-slime-eval)

;;;; ---- Other stuff ----

(require 'nomis-string-utilities)
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

(require 'nomis-shell-stuff)
(require 'nomis-eshell)

(require 'nomis-keyboard-macros)
(require 'nomis-searching)
(require 'nomis-searching-filters)
(require 'nomis-ibuffer)
(require 'nomis-ace-jump-mode)

(require 'nomis-multi-web-mode)

(require 'nomis-javascript)

(require 'nomis-rcirc)

(require 'nomis-magit)

(require 'nomis-projectile)

(require 'nomis-markdown)

(require 'nomis-text-size)

(require 'homeless)

(progn
  ;; Putting this where it belongs (in "nomis-very-general-stuff") doesn't work;
  ;; I guess something blats it.
  ;; Ah! This was probably because of issues with source files vs compiled
  ;; files, so try again.
  (define-key global-map [(insert)] nil))

;;;; ___________________________________________________________________________
;;;;; ---- personal ----

(when i-am-nomis-p
  (load nomis-personal-emacs-init-file))

;;;; ___________________________________________________________________________
;;;;; ---- temp for playing ----
