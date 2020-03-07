;;;; Init stuff -- projectile -*- lexical-binding: t -*-

(projectile-global-mode)

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

;; (setq projectile-create-missing-test-files t)

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

(defvar nomis/toggle-between-impl-and-test/approach :normal)

(defvar nomis/toggle-between-impl-and-test/project-name-as-dir nil)

(advice-add
 'projectile-toggle-between-implementation-and-test
 :around
 (lambda (orig-fun &rest args)
   (ecase nomis/toggle-between-impl-and-test/approach
     (:normal
      (apply orig-fun args))
     (:wefarm-001
      (let* ((project-name-as-dir
              nomis/toggle-between-impl-and-test/project-name-as-dir)
             (src-path-section (s-concat "/" project-name-as-dir "/"))
             (test-path-section (s-concat "/" project-name-as-dir "/test/"))
             (file-name (buffer-file-name))
             (src-file? (cl-search "/src/" file-name))
             (other-file (if src-file?
                             (destructuring-bind (prefix suffix)
                                 (s-split-up-to "/src/"
                                                file-name
                                                1)
                               (s-concat prefix
                                         "/test/"
                                         (->> suffix
                                              (s-replace src-path-section
                                                         test-path-section))))
                           (destructuring-bind (prefix suffix)
                               (s-split-up-to "/test/"
                                              file-name
                                              1)
                             (s-concat prefix
                                       "/src/"
                                       (->> suffix
                                            (s-replace test-path-section
                                                       src-path-section)))))))
        (message
         "nomis projectile-toggle-between-implementation-and-test other-file = %s"
         other-file)
        (if (or projectile-create-missing-test-files
                (file-exists-p other-file))
            (find-file other-file)
          (progn
            (message "No such file: %s" other-file)
            (nomis/msg/beep)))))))
 '((name . nomis/hack-for-non-standard-test-file-naming)))

;;;; ___________________________________________________________________________

(provide 'nomis-projectile)
