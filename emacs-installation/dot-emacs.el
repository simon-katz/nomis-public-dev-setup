;;;; ___________________________________________________________________________
;;;; ---- Duh ----

;; Now using exec-path-from-shell

;; (add-to-list 'exec-path "/usr/local/bin")
;; (add-to-list 'exec-path "~/bin")

;;;; ___________________________________________________________________________
;;;; ---- Emacs setup -- Tailor the installation ----

(load-file-relative-to-this-file "ensure-expected-emacs-version")
(load-file-relative-to-this-file "set-up-package-stuff")

(defun nomis/load-file-name ()
  (file-truename (or load-file-name (buffer-file-name))))

(defun nomis/load-file-directory ()
  (file-name-directory (nomis/load-file-name)))

;;;; ___________________________________________________________________________
;;;; ---- exec-path-from-shell ----

(when (memq window-system '(mac ns))
  (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-initialize))

;;;; ___________________________________________________________________________
;;;; ---- i-am-nomis/p ----

(defvar nomis/personal-emacs-init-file
  (concat (nomis/load-file-directory)
          "../../emacs-configuration-personal/nomis-personal-emacs-init.el"))

(defvar i-am-nomis/p
  (file-exists-p nomis/personal-emacs-init-file))

;;;; ___________________________________________________________________________

(defun add-dir-and-normal-subdirs-to-load-path (dir)
  (add-to-list 'load-path dir)
  (let* ((default-directory dir))
    (normal-top-level-add-subdirs-to-load-path)))

(add-dir-and-normal-subdirs-to-load-path
 (concat (nomis/load-file-directory)
         "emacs-init-files"))

(defun add-and-recompile-directory (relative-dir &optional compile?)
  (let* ((dir (concat (nomis/load-file-directory) relative-dir)))
    (add-to-list 'load-path dir)
    (when compile?
     (byte-recompile-directory dir))))

(when i-am-nomis/p
  (add-and-recompile-directory "../../emacs-package-repos/cider" t)
  (add-and-recompile-directory "../../emacs-package-repos/clj-refactor" t)
  (add-and-recompile-directory "../../emacs-package-repos/align-cljlet")
  (add-and-recompile-directory "../../emacs-package-repos/cljr-helm")
  (add-and-recompile-directory "../../emacs-package-repos/multi-web-mode"))

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

(when (version< emacs-version "25.3")
  ;; See https://lists.gnu.org/archive/html/info-gnu/2017-09/msg00006.html
  ;; - Version 25.3 is a emergency release to fix a security vulnerability.
  ;; - This code works around the vulnerability.
  (eval-after-load "enriched"
    '(defun enriched-decode-display-prop (start end &optional param)
       (list start end))))

;;;; ---- Very general stuff ----

(require 's)
(require 'dash)
(require 'cl)
(require 'mmt)

(require 'nomis-core-utils)
(require 'nomis-very-general-stuff-new)
(require 'nomis-message-window)

;;;; ---- This is here because of a problem when it was later ----

(require 'nomis-org) ; When this was later in the file my setting of
                     ; `org-replace-disputed-keys` wasn't working.
                     ; Something must be loading org-mode.
                     ; (That didn't used to happen. It's a change in the
                     ; last few days.)
                     ; -- jsk 2014-08-13

;;;; ---- General stuff ----

(require 'nomis-ido-and-smex)
(require 'nomis-auto-complete)

(require 'nomis-which-key)

(require 'nomis-key-chord)
(require 'nomis-hydra)

(require 'nomis-line-move-visual)

(require 'nomis-special-characters)

(require 'nomis-dictionaries-etc)

;;;; ---- Lisp and Clojure stuff probably good for anyone ----

(require 'nomis-paredit)
(require 'nomis-paxedit)
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
(require 'nomis-scrolling)
(require 'nomis-mouse-scrolling)
(require 'nomis-avoid-window-stealing)
(require 'nomis-whitespace)
(require 'nomis-right-margin)
(require 'nomis-fci-mode)
(require 'nomis-highlighting)
(require 'nomis-line-numbering)
(require 'nomis-saveplace)

(require 'nomis-buffers-windows-frames)

(require 'nomis-window-config)
(require 'nomis-window-config-settings)

(when (display-graphic-p)
  (require 'nomis-screens)
  (require 'nomis-frames))
(require 'nomis-frame-style)
(require 'nomis-windows)

(require 'nomis-files)

(require 'nomis-faces)

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

(require 'nomis-yafolding)
(require 'nomis-yaml)

(require 'homeless)

(require 'string-inflection)

(require 'nomis-popup)

(progn
  ;; Putting this where it belongs (in "nomis/very-general-stuff") doesn't work;
  ;; I guess something blats it.
  ;; Ah! This was probably because of issues with source files vs compiled
  ;; files, so try again.
  (define-key global-map [(insert)] nil))

;;;; ___________________________________________________________________________
;;;;; ---- personal ----

(when i-am-nomis/p
  (load nomis/personal-emacs-init-file))

;;;; ___________________________________________________________________________
;;;;; ---- temp for playing ----

;;;; ___________________________________________________________________________
;;;;; ---- temp for Windows -- probably move things to a better home ----

(when (equal system-type 'windows-nt)

  (setq find-program "unixfind.exe")

  (setq-default buffer-file-coding-system 'utf-8-unix)

  (setq projectile-indexing-method 'alien))

;;;; ___________________________________________________________________________
;;;; ---- YV-specific ----

(require 'nomis-yv-stuff)

;;;; ___________________________________________________________________________
;;;; ---- Local ----

(let* ((local-extras-file "~/development-100/repositories/local/sk-local-settings/dot-emacs-extras.el"))
  (when (file-exists-p local-extras-file)
    (load local-extras-file)))
