;;;; Init stuff -- projectile

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

(define-key projectile-mode-map (kbd "H-d") 'projectile-find-dir)
;; (define-key projectile-mode-map (kbd "H-p") 'projectile-switch-project)
(define-key projectile-mode-map (kbd "H-f") 'projectile-find-file)
(define-key projectile-mode-map (kbd "H-g") 'projectile-grep)


;;;; ___________________________________________________________________________

(provide 'nomis-projectile)
