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
  (add-to-list 'load-path (concat (nomis-load-file-directory)
                                  "../../emacs-package-repos/align-cljlet"))
  (add-to-list 'load-path (concat (nomis-load-file-directory)
                                  "../../emacs-package-repos/clj-refactor"))
  (add-to-list 'load-path (concat (nomis-load-file-directory)
                                  "../../emacs-package-repos/cljr-helm"))
  (add-to-list 'load-path (concat (nomis-load-file-directory)
                                  "../../emacs-package-repos/multi-web-mode")))


;;;; ___________________________________________________________________________

(defvar nomis/system-name
  (cond ((string-equal (system-name) "CHIVERS")
         :chivers)
        ((string-equal (system-name) "GILZEAN2")
         :gilzean2)
        ((string-equal (system-name) "JENNINGS")
         :jennings)
        ((string-equal (system-name) "Simon-Katzs-MacBook-Pro.local")
         :simon-katzs-macbook-pro.local)
        ((member (system-name) ; this keeps changing --why? Ah! At the time of writing it's "188.28.48.230.threembb.co.uk", which mentions "three" and I'm on my data connection with 3connect
                 (list "unknown-70-56-81-a2-7a-0f.home"
                       "Perryman.local"
                       "perryman.home"
                       "lonmaclt002.home"))
         :perryman)
        ((member (system-name) ; this keeps changing --why? Ah! At the time of writing it's "188.28.48.230.threembb.co.uk", which mentions "three" and I'm on my data connection with 3connect
                 (list "unknown-3c-15-c2-e6-e3-64.home" ; sheringham sometimes
                       "sheringham.local"
                       "sheringham.home"))
         :sheringham)
        ((equal system-type 'windows-nt)
         :probably-windows-at-work-place)
        ((and (equal system-type 'gnu/linux)
              (string-match-p (regexp-quote "termux")
                              (getenv "HOME")))
         :termux-on-android)
        (t
         nil)))

;;;; ___________________________________________________________________________
;;;; ---- Load various files ----

(require 'nomis-very-general-stuff-new)

(require 'nomis-org) ; When this was later in the file my setting of
                     ; `org-replace-disputed-keys` wasn't working.
                     ; Something must be loading org-mode.
                     ; (That didn't used to happen. It's a change in the
                     ; last few days.)
                     ; -- jsk 2014-08-13

;;;; ---- General stuff ----

(require 's)
(require 'dash)
(require 'cl)
(require 'mmt)

(require 'nomis-ido-and-smex)
(require 'nomis-auto-complete)

(require 'nomis-key-chord)
(require 'nomis-hydra)

;;;; ---- Lisp and Clojure stuff probably good for anyone ----

(require 'nomis-paredit)
(require 'nomis-emacs-lisp-and-ielm)
(require 'nomis-clojure)

;;;; ---- Lisp and Clojure stuff probably only for me ----

(require 'nomis-indent-sexp)

;; (require 'nomis-slime-eval)

;;;; ---- Other stuff ----

(require 'nomis-string-utilities)
(require 'nomis-very-general-stuff) ; TODO: Move this to beginning, and check all is ok
(require 'nomis-timers)
(require 'nomis-beep)
(require 'nomis-mouse-scrolling)
(require 'nomis-avoid-window-stealing)
(require 'nomis-right-margin)
(require 'nomis-highlighting)
(require 'nomis-line-numbering)
(require 'nomis-saveplace)
(when (display-graphic-p)
  (require 'nomis-screens)
  (require 'nomis-frames))
(require 'nomis-buffers)
(require 'nomis-frame-style)
(require 'nomis-windows)
(require 'nomis-uniquify)

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

(unless (equal system-type 'windows-nt)
  (require 'nomis-multi-web-mode))

(require 'nomis-javascript)

(require 'nomis-magit)

(require 'nomis-projectile)

(require 'nomis-markdown)

(require 'nomis-text-size)

(require 'nomis-hide-show)

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

;;;; ___________________________________________________________________________
;;;;; ---- temp for Windows -- probably move things to a better home ----

(when (equal system-type 'windows-nt)

  (setq find-program "unixfind.exe")

  (setq-default buffer-file-coding-system 'utf-8-unix)

  (setq projectile-indexing-method 'alien))
