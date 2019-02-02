;;;; Init stuff -- Undo tree.

(require 'undo-tree)

(global-undo-tree-mode)

(setq undo-tree-auto-save-history t)

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
;;;; diff direction

(defvar *nomis/undo-tree/invert-diff-in-selection-mode?* t)

(defvar *nomis/undo-tree/in-undo-tree-diff?* nil)

(defun nomis/undo-tree/toggle-invert-diff-in-selection-mode ()
  (interactive)
  (setf *nomis/undo-tree/invert-diff-in-selection-mode?*
        (not *nomis/undo-tree/invert-diff-in-selection-mode?*))
  (when undo-tree-visualizer-diff (undo-tree-visualizer-update-diff
                                   undo-tree-visualizer-selected-node))
  (message "*nomis/undo-tree/invert-diff-in-selection-mode?* turned %s"
           (if *nomis/undo-tree/invert-diff-in-selection-mode?* "on" "off")))

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
                (let* ((selection-mode? (with-current-buffer
                                            undo-tree-visualizer-buffer-name
                                          undo-tree-visualizer-selection-mode)))
                  (if (and selection-mode?
                           *nomis/undo-tree/in-undo-tree-diff?*
                           *nomis/undo-tree/invert-diff-in-selection-mode?*)
                      (apply orig-fun new old other-args)
                    (apply orig-fun old new other-args))))
              `((name . ,advice-name))))

;;;; ___________________________________________________________________________

(provide 'nomis-undo-tree)
