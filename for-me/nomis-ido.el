;; Init stuff -- ido.

;;;; ___________________________________________________________________________

;; TODO: Look at this ido stuff copied from the Starter Kit.

(ido-mode t)
(ido-ubiquitous t) ; e.g. for `describe-function' (C-h f)

(setq ido-default-file-method 'selected-window)
(setq ido-default-buffer-method 'selected-window)

;; (setq ido-enable-prefix nil)
(setq ido-enable-flex-matching t)
;; (setq ido-auto-merge-work-directories-length nil)
;; (setq ido-create-new-buffer 'always)
(setq ido-use-filename-at-point 'guess)
;; (setq ido-use-virtual-buffers t)
;; (setq ido-handle-duplicate-virtual-buffers 2)
;; (setq ido-max-prospects 10)

;;;; ___________________________________________________________________________

(provide 'nomis-ido)
