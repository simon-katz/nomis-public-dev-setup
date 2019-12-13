;;;; Init stuff -- Searching.

;;;; ___________________________________________________________________________
;;;; ---- Stuff for rgrep and lgrep ----

(defvar logs-dir-name "logs")

(defvar nomis/rgrep-local-ignored-directories '()) ; set this in .dir-locals.el

(advice-add 'rgrep-find-ignored-directories
            :around
            (lambda (orig-fun &rest args)
              (append nomis/rgrep-local-ignored-directories
                      (apply orig-fun args)))
            '((name . nomis/add-local-ignored-directories)))

(progn
  (defvar *extra-ignored-directories*
    (list logs-dir-name
          ;; ".emacs.d"
          ".emacs-backups"
          ".worksheet"
          "out"
          "target"
          ".repl"
          "bundle"
          ".idea"
          ;; "labrepl*/public/javascripts/jquery.js"
          ;; "emacs-configuration/nomis-addons/cygwin-mount.el"
          "node_modules"
          ".shadow-cljs"
          ".emacs.d"
          "emacs-configuration-pre-2018-06-upgrade-packages"
          "clojure-for-the-brave-and-true/emacs-for-clojure-book1"
          "cljs-runtime"
          ".clj-kondo"
          ;; Instead of adding stuff here, consider defining
          ;; `nomis/rgrep-local-ignored-directories` in a .dir-locals file.
          ))
  (defvar *extra-ignored-files*
    '(".ido.last"
      ".smex-items"
      ;; ".jar"
      ;; ".exe"
      ".cider-repl-history"
      ".lein-repl-history"
      "*.iml"
      "*.zip"
      "figwheel_server.log"
      "archive-contents"))
  (eval-after-load "grep"
    '(progn
       (mapc (lambda (x) (add-to-list 'grep-find-ignored-files x))
             *extra-ignored-files*)
       (mapc (lambda (x) (add-to-list 'grep-find-ignored-directories x))
             *extra-ignored-directories*))))

(defun nomis/toggle-include-emacs.d-in-searches ()
  (interactive)
  (let ((dir-name ".emacs.d"))
    (setq grep-find-ignored-directories
          (if (member dir-name grep-find-ignored-directories)
              (remove dir-name grep-find-ignored-directories)
            (cons dir-name grep-find-ignored-directories)))
    (message "%s %s -- NOTE: THIS WILL APPLY ONLY TO NEW GREP BUFFERS"
             (if (member dir-name grep-find-ignored-directories)
                 "Excluding"
               "Including")
             dir-name)))

;;;; ___________________________________________________________________________

(progn
  ;; M-> and M-< are by default bound to end-of-buffer and
  ;; beginning-of-buffer, which are also bound to C-end and C-home.
  ;; Rebind M-> and M-< without much lossage.
  (define-key global-map (kbd "M->") 'next-error)
  (define-key global-map (kbd "M-<") 'previous-error))

;;;; ___________________________________________________________________________
;;;; Better grepping.

(eval-after-load "grep"
  '(when (equal system-type 'darwin)
     (grep-apply-setting
      'grep-find-template
      (case 2
        (1
         ;; The default computed on Sheringham
         "find <D> <X> -type f <F> -exec grep <C> -nH --null -e <R> \\{\\} +")
        (2
         ;; What I'm trying -- sort the files
         "find <D> <X> -type f <F> -print0 | sort -z | xargs -0 grep <C> -nH --null -e <R>")))))

;;;; Based on code found at
;;;; https://github.com/magnars/.emacs.d/blob/master/setup-rgrep.el.
;;;; Note:
;;;;   I had tried something that advised `grep-read-files`, but that didn't
;;;;   work. Looks like `grep-read-files` does some weird side-effecty stuff.
;;;;   In any case, I don't understand and I gave up on that approach.

(defun -nomis/rgrep-interactive-stuff (all-p)
  (progn
    (grep-compute-defaults)
    (cond
     ((and grep-find-command (equal current-prefix-arg '(16)))
      (list (read-from-minibuffer "Run: " grep-find-command
                                  nil nil 'grep-find-history)))
     ((not grep-find-template)
      (error "grep.el: No `grep-find-template' available"))
     (t (let* ((regexp (grep-read-regexp))
               (files (if all-p
                          "*"
                        (grep-read-files regexp)))
               (dir (ido-read-directory-name "Base directory: "
                                             nil default-directory t))
               (confirm (equal current-prefix-arg '(4))))
          (list regexp files dir confirm))))))

(defun nomis/rgrep (regexp &optional files dir confirm)
  "A variation of `rgrep` that:
- uses `ido-read-directory-name` for nicer directory navigation."
  (interactive (-nomis/rgrep-interactive-stuff nil))
  (rgrep regexp files dir confirm))

(defun nomis/rgrep-all-unignored-files (regexp &optional files dir confirm)
  "A variation of `rgrep` that:
- uses `ido-read-directory-name` for nicer directory navigation
- searches all (unignored) files."
  (interactive (-nomis/rgrep-interactive-stuff t))
  (rgrep regexp files dir confirm))

(defun nomis/grep-logs-dirs-include ()
  (interactive)
  (setq grep-find-ignored-directories
        (remove logs-dir-name
                grep-find-ignored-directories)))

(defun nomis/grep-logs-dirs-exclude ()
  (interactive)
  (setq grep-find-ignored-directories
        (cons logs-dir-name
              grep-find-ignored-directories)))

;; (define-key global-map (kbd "H-q g a") 'nomis/rgrep)
;; (define-key global-map (kbd "H-q g g") 'nomis/rgrep-all-unignored-files)

;;;; ___________________________________________________________________________
;;;; ---- Stuff for rgrep and lgrep ----

;; (defadvice grep-tag-default (around change-space-to-todo ()) ; TODO: Huh? What's this for?
;;   (flet ((do-it () ad-do-it))
;;     (when (string-equal (do-it) "")
;;       (setq ad-return-value
;;             ;; use \ below so this doesn't show up in searches
;;             "TOD\O"))))
;; (ad-activate 'grep-tag-default)

;; (define-key global-map (kbd "C-c C-v C-s") 'rgrep)

;;;; ___________________________________________________________________________

(provide 'nomis-searching)
