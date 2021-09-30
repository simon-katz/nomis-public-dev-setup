;;;; Init stuff -- projectile -*- lexical-binding: t -*-

(projectile-mode)

;; If you ever forget any of Projectile's keybindings just do a: C-c p C-h

;; See https://github.com/bbatsov/projectile

;; If you're going to use the default ido completion it's extremely highly
;; recommended that you install the optional flx-ido package, which provides a
;; much more powerful alternative to ido's built-in flex matching.

;; projectile-enable-caching
;; Running C-u C-c p f will invalidate the cache prior to prompting you for a file.
;; The project cache is persistent and will be preserved during Emacs restarts.
;; projectile-remember-window-configs

;; projectile-switch-project
;; projectile-remember-window-configs
;; projectile-switch-project-action

(define-key projectile-mode-map (kbd "H-o d") 'projectile-find-dir)
;; (define-key projectile-mode-map (kbd "H-o p") 'projectile-switch-project)
(define-key projectile-mode-map (kbd "H-o f") 'projectile-find-file)
(define-key projectile-mode-map (kbd "H-o g") 'projectile-grep)

;;;; ___________________________________________________________________________

(setq projectile-create-missing-test-files t)

;;;; ___________________________________________________________________________

;;;; If a Leiningen project has a ".midje.clj" file, Projectile thinks test
;;;; files have a "t_" prefix rather than a "_test" suffix.
;;;; I think Projectile is confused on two counts:
;;;; - It treats Midje projects differently depending on whether they have a
;;;;   ".midje.clj" file.
;;;;   - Ah, it needs a way to distinguish Midje projects from clojure.test
;;;;     projects, because the `:test` entries in the project types are
;;;;     different.
;;;; - I've never seen the "t_" prefix used in a Midje project (or indeed any
;;;;   Clojure project).
;;;;
;;;; This hack fixes my problem, but might well break things for other people.
;;;;
;;;; TODO Think more about the proper fix for this. Can you just hack the
;;;;      test prefix and suffix?
;;;; TODO-open-source-contribution: Projectile and ".midje.clj" files.
;;;;     - First try to just hack prefix and suffix.
;;;;     - First ask why it's as it is and get agreement on the fix.

(advice-add 'projectile-project-type
            :around
            (lambda (orig-fun &rest args)
              (let ((res (apply orig-fun args)))
                (if (equal res 'lein-midje)
                    'lein-test
                  res)))
            '((name . nomis/hack-projectile-midje-projects)))

;;;; ___________________________________________________________________________
;;;; ---- Fix broken `projectile-grep` ----

;;;; See https://github.com/bbatsov/projectile/issues/1687

(with-eval-after-load 'projectile
  (cond
   ((member (pkg-info-version-info 'projectile)
            '("20210811.435"))

    (defvar *nomis/in-projectile-grep?* nil)

    (advice-add
     'projectile-grep
     :around
     (lambda (orig-fun &rest args)
       (let* ((*nomis/in-projectile-grep?* t))
         (apply orig-fun args)))
     '((name . nomis/hack-projectile-grep)))

    (advice-add
     'rename-buffer
     :around
     (lambda (orig-fun newname &optional unique)
       (when *nomis/in-projectile-grep?*
         (pop-to-buffer "*grep*"))
       (funcall orig-fun
                newname
                (if *nomis/in-projectile-grep?* t unique)))
     '((name . nomis/hack-projectile-grep))))

   (t
    (message-box
     "You need to fix `nomis/hack-projectile-grep` for this version of projectile."))))

;;;; ___________________________________________________________________________

(provide 'nomis-projectile)
