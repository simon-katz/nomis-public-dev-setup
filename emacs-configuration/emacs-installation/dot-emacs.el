;;;; ___________________________________________________________________________
;;;; ---- Raise frame ----

;;;; After upgrading to macOS Sonoma, when starting Emacs the frame is
;;;; not always on top and does not always have input focus.

(when window-system
  (select-frame-set-input-focus (selected-frame)))

;;;; ___________________________________________________________________________
;;;; ---- Duh ----

;; Now using exec-path-from-shell

;; (add-to-list 'exec-path "/usr/local/bin")
;; (add-to-list 'exec-path "~/bin")

;;;; ___________________________________________________________________________
;;;; ---- Emacs setup -- Tailor the installation ----

(setq nomis/emacs-d-non-repo-dir "~/.emacs.d-non-repo")
(make-directory nomis/emacs-d-non-repo-dir t)

(load-file-relative-to-this-file "ensure-expected-emacs-version")
(load-file-relative-to-this-file "set-up-package-stuff")

(defun nomis/load-file-name ()
  (file-truename (or load-file-name (buffer-file-name))))

(defun nomis/load-file-directory ()
  (file-name-directory (nomis/load-file-name)))

;;;; ___________________________________________________________________________
;;;; ---- exec-path-from-shell ----

(setq shell-file-name "/opt/homebrew/bin/bash")

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;;;; ___________________________________________________________________________
;;;; ---- i-am-nomis/p ----

(defvar nomis/personal-emacs-init-file
  (concat (nomis/load-file-directory)
          "../../../emacs-configuration-personal/nomis-personal-emacs-init.el"))

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
  (add-and-recompile-directory "../../../emacs-package-repos/align-cljlet")
  (add-and-recompile-directory "../../../emacs-package-repos/cljr-helm")
  (add-and-recompile-directory "../../../emacs-package-repos/multi-web-mode"))

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
        ((member (system-name)
                 (list "defoe.local"
                       "defoe.home"))
         :defoe)
        ((member (system-name)
                 (list "keane.local"
                       "keane.home"
                       "HUD040197"))
         :keane)
        ((member (system-name)
                 ;; My Wefarm Mac
                 '("Simons-MBP.home"
                   "Simons-MacBook-Pro.local"))
         :simon-katzs-wefarm-macbook-pro)
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
(global-dash-fontify-mode)
(with-eval-after-load 'info-look
  (dash-register-info-lookup))

(require 'cl)
(require 'mmt)

(require 'nomis-fix-post-command-hook-slow-with-m-x-commands)
(require 'nomis-fix-28-2-svg-error)

(require 'nomis-sidecar-locals)

(require 'nomis-core-utils)
(require 'nomis-very-general-stuff-new)
(require 'nomis-message-window)

(require 'nomis-themes)

;;;; ---- This is here because of a problem when it was later ----

(require 'nomis-org) ; When this was later in the file my setting of
                     ; `org-replace-disputed-keys` wasn't working.
                     ; Something must be loading org-mode.
                     ; (That didn't used to happen. It's a change in the
                     ; last few days.)
                     ; -- jsk 2014-08-13

;;;; ---- General stuff ----

(require 'nomis-disable-c-z)
(require 'nomis-hack-custom-save-variables)

(require 'nomis-datetime)

(require 'nomis-y-or-n-p)

(require 'nomis-ido-and-smex)
(require 'nomis-auto-complete)

(require 'nomis-which-key)

(require 'nomis-key-chord)
(require 'nomis-hydra)

(require 'nomis-line-move-visual)

(require 'nomis-special-characters)

(require 'nomis-dictionaries-etc)

(require 'nomis-lsp-hacks)

(require 'nomis-vterm-hacks)
(require 'nomis-vterm)

(require 'nomis-tooltips)

;;;; ---- Lisp and Clojure stuff probably good for anyone ----

(require 'nomis-sexp-navigation)

(require 'nomis-paredit)
(require 'nomis-paxedit)
(require 'nomis-emacs-lisp-and-ielm)
(require 'nomis-ielm-persistent-history)
(require 'nomis-clojure)

;;;; ---- Lisp and Clojure stuff probably only for me ----

(require 'nomis-indent-sexp)

;; (require 'nomis-slime-eval)

;;;; ---- Other stuff ----

(require 'nomis-string-utilities)
(require 'nomis-very-general-stuff) ; TODO: Move this to beginning, and check all is ok
(require 'nomis-timers)
(require 'nomis-msg)
(require 'nomis-scrolling)
(require 'nomis-mouse-scrolling)
(require 'nomis-avoid-window-stealing)
(require 'nomis-highlight-chars)
(require 'nomis-right-margin)
(require 'nomis-highlighting)
(require 'nomis-line-numbering)
(require 'nomis-saveplace)
(require 'nomis-savehist)

(require 'nomis-string-edit)

(require 'nomis-buffers-windows-frames)

(require 'nomis-window-config)
(require 'nomis-window-config-settings)

(when (display-graphic-p)
  (require 'nomis-screens)
  (require 'nomis-frames))
(require 'nomis-window-backgrounds)
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
(require 'nomis-loccur)
(require 'nomis-ibuffer)
(require 'nomis-ace-jump-mode)

(unless (equal system-type 'windows-nt)
  (require 'nomis-multi-web-mode))

(require 'nomis-java)
(require 'nomis-javascript)
(require 'nomis-json)
(require 'nomis-magit)
(require 'nomis-revert)
(require 'nomis-diff-hl)

(require 'nomis-projectile)

(require 'nomis-markdown)

(require 'nomis-text-size)

(require 'nomis-hide-show)

(require 'nomis-yafolding)
(require 'nomis-yaml)
(require 'nomis-applescript)

(require 'homeless-non-lexical)

(require 'string-inflection)

(require 'nomis-popup)

(require 'nomis-dumb-jump)

(require 'nomis-flycheck)

(require 'nomis-eldoc)

(require 'nomis-logview)

(require 'nomis-auto-revert)

(require 'show-point-mode)

(require 'nomis-buffer-menu)

(require 'nomis-avro)

(require 'nomis-terraform)

(require 'nomis-text-processing)

(require 'nomis-irc)

(require 'nomis-auto-save-hacks)

(require 'nomis-nav-flash)

(require 'nomis-beacon)

(require 'nomis-very-large-files)

(require 'nomis-sql)

(require 'nomis-gpg)

(require 'nomis-overlay-priorities)

(require 'nomis-replace-text)

(require 'nomis-eca)

;; (eval-after-load "isearch" '(require 'isearch+))
;; Is this the cause of problems with Magit when trying to commit?...
;; -- *ERROR*: ‘recenter’ing a window that does not display current-buffer.

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

(let* ((local-extras-file "~/development-100/repositories/nomis/dev-setup/jsk-settings-private/_no-commit_/dot-emacs-extras.el"))
  (when (file-exists-p local-extras-file)
    (load local-extras-file)))

(let* ((local-extras-file "~/development-100/repositories/nomis/dev-setup/jsk-settings-private/_no-commit_/_nomis-no-sync_/dot-emacs-extras.el"))
  (when (file-exists-p local-extras-file)
    (load local-extras-file)))
