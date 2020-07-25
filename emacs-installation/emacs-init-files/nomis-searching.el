;;;; Init stuff -- Searching.

;;;; ___________________________________________________________________________

(require 'cl)
(require 'nomis-searching-file-stuff)
(require 'nomis-searching-directory-stuff)

;;;; ___________________________________________________________________________
;;;; ---- Stuff for grep ----

(defun with-augmented-grep-find-ignored-things* (f)
  (let* ((grep-find-ignored-directories (nomis/grep/ignored-directories))
         (grep-find-ignored-files       (nomis/grep/ignored-files)))
    (funcall f)))

(defmacro with-augmented-grep-find-ignored-things (options &rest body)
  (declare (indent 1))
  `(with-augmented-grep-find-ignored-things* (lambda () ,@body)))

(advice-add 'rgrep-default-command
            :around
            (lambda (orig-fun &rest args)
              (with-augmented-grep-find-ignored-things ()
                (apply orig-fun args)))
            '((name . nomis/augment-grep-find-ignored-things)))

(advice-add 'projectile-rgrep-default-command
            :around
            (lambda (orig-fun &rest args)
              (with-augmented-grep-find-ignored-things ()
                (apply orig-fun args)))
            '((name . nomis/augment-grep-find-ignored-things)))

;;;; ___________________________________________________________________________

(defun -nomis/grep/toggle-ignored-things (dirs? names) ; TODO This is over-complicated for what you now need.
  (let ((dirs-or-files (if dirs? "dirs" "files")))
    (cl-flet* ((currently-ignored?
                ()
                (member (first names)
                        (if dirs?
                            (nomis/grep/ignored-directories)
                          (nomis/grep/ignored-files))))
               (change-state
                (ignore?)
                (cl-loop
                 for v1 in (if dirs?
                               -nomis/grep/ignore-overridden/all/directories-vars
                             -nomis/grep/ignore-overridden/all/files-vars)
                 as  v2 in (if dirs?
                               -nomis/grep/ignored/all/directories-vars
                             -nomis/grep/ignored/all/files-vars)
                 do (cl-loop for name in names
                             when (member name (symbol-value v2))
                             do   (set v1
                                       (let ((v (symbol-value v1)))
                                         (if ignore?
                                             (remove name v)
                                           (cons name v))))))
                (message "%s %S -- NOTE: THIS WILL APPLY ONLY TO NEW GREP BUFFERS"
                         (if ignore?
                             (format "Ignoring the following %s:"
                                     dirs-or-files)
                           (format "Not ignoring the following %s:"
                                   dirs-or-files))
                         names)))
      (if (currently-ignored?)
          (change-state nil)
        (progn
          (cl-loop
           for name in names
           unless (member name
                          (if dirs?
                              (-nomis/grep/ignored/all/directories)
                            (-nomis/grep/ignored/all/files)))
           do (error "%s '%s' is not ignored, so cannot override the ignore"
                     (if dirs? "Directory" "File")
                     name))
          (change-state t))))))

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
