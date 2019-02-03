;;;; Init stuff -- Undo tree.

(require 'undo-tree)

(global-undo-tree-mode)

(setq undo-tree-enable-undo-in-region nil) ; apparently there are bugs

;; (setq undo-tree-auto-save-history t) ; apparently there are bugs

(setq undo-tree-visualizer-timestamps t)
(setq undo-tree-visualizer-relative-timestamps nil)
(setq undo-tree-visualizer-diff t)

(defadvice undo-tree-make-history-save-file-name
    (after undo-tree activate)
  (setq ad-return-value (concat ad-return-value ".gz")))

(push "~undo-tree~" ido-ignore-files)

(progn
  ;; The default for M-z is zap-to-char.  I don't need that, so...

  ;; Hah! This is the thing that causes you to lose undo history!

  ;; (define-key global-map (kbd "M-z") 'undo-tree-undo)
  ;; (define-key global-map (kbd "M-Z") 'undo-tree-redo)
  )

;;;; ___________________________________________________________________________
;;;; Set up `undo-tree-visualizer-selection-mode` by default

(add-hook 'undo-tree-visualizer-mode-hook 'undo-tree-visualizer-selection-mode)

;;;; ___________________________________________________________________________
;;;; diff direction

(defvar *nomis/undo-tree/invert-diff?* nil)

(defvar *nomis/undo-tree/in-undo-tree-diff?* nil)

(defun nomis/undo-tree/toggle-invert-diff-in-selection-mode ()
  (interactive)
  (setf *nomis/undo-tree/invert-diff?*
        (not *nomis/undo-tree/invert-diff?*))
  (nomis/undo-tree/set-face)
  (when undo-tree-visualizer-diff (undo-tree-visualizer-update-diff
                                   undo-tree-visualizer-selected-node))
  (message "Invert diff turned %s"
           (if *nomis/undo-tree/invert-diff?* "on" "off")))

(define-key undo-tree-visualizer-mode-map
  "D"
  'nomis/undo-tree/toggle-invert-diff-in-selection-mode)

(let* ((advice-name '-nomis/undo-tree/swap-diff-direction))
  (advice-add 'undo-tree-diff
              :around
              (lambda (orig-fun &rest args)
                (let* ((*nomis/undo-tree/in-undo-tree-diff?* t))
                  (apply orig-fun args)))
              `((name . ,advice-name)))
  (advice-add 'diff-no-select
              :around
              (lambda (orig-fun old new &rest other-args)
                (if (and *nomis/undo-tree/in-undo-tree-diff?*
                         *nomis/undo-tree/invert-diff?*)
                    (apply orig-fun new old other-args)
                  (apply orig-fun old new other-args)))
              `((name . ,advice-name))))

;;;; ___________________________________________________________________________
;;;; Buffer face

;;;; TODO This was a copy and paste of `nomis/dirtree/set-face`. Extract the
;;;;      common pattern.

(defun nomis/undo-tree/make-face-kvs ()
  (list :background
        (if *nomis/undo-tree/invert-diff?*
            "mint cream"
          'unspecified)))

(defvar nomis/undo-tree/face-cookie nil)
(defvar nomis/undo-tree/previous-face-kvs nil)

(defun nomis/undo-tree/set-face (&optional force)
  (when (get-buffer undo-tree-visualizer-buffer-name)
    (let* ((face-kvs (nomis/undo-tree/make-face-kvs)))
      (when (or force
                (not (equal face-kvs nomis/undo-tree/previous-face-kvs)))
        (setq nomis/undo-tree/previous-face-kvs face-kvs)
        (with-current-buffer undo-tree-visualizer-buffer-name
          (face-remap-reset-base 'default)
          (when nomis/undo-tree/face-cookie
            (face-remap-remove-relative nomis/undo-tree/face-cookie)
            (setq nomis/undo-tree/face-cookie nil))
          (setq nomis/undo-tree/face-cookie
                (face-remap-add-relative 'default
                                         (list face-kvs))))))))

(let* ((advice-name '-nomis/undo-tree/set-face))
  (advice-add 'undo-tree-visualize
              :after
              (lambda () (nomis/undo-tree/set-face t))
              `((name . ,advice-name))))

;;;; ___________________________________________________________________________

(provide 'nomis-undo-tree)
