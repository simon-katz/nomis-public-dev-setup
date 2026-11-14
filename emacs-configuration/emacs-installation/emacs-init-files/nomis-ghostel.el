;;; nomis-ghostel.el --- ghostel tailoring -*- lexical-binding: t; -*-

;;; Code

;;;; Requires

(require 'cl-lib)
(require 'dash)

;;;; Avoid flycheck warnings

(defvar ghostel-shell)
(defvar ghostel-mode-map)
(defvar ghostel-keymap-exceptions)
(defvar ghostel--input-mode)
(defvar ghostel--readonly-fast-exit)
(declare-function ghostel-clear "ghostel")
(declare-function ghostel-clear-scrollback "ghostel")

;;;; ghostel config

(defun nomis/ghostel--append-fast-exit-indicator ()
  "Append fast-exit indicator to `mode-line-process' after ghostel refreshes it."
  (when (memq ghostel--input-mode '(copy emacs))
    (let ((indicator (if ghostel--readonly-fast-exit " [FE-on]" " [FE-off]")))
      (setq mode-line-process
            (if (listp mode-line-process)
                (append mode-line-process (list indicator))
              (list mode-line-process indicator))))))

(with-eval-after-load 'ghostel

  (setopt ghostel-shell "/opt/homebrew/bin/bash")

  (define-key ghostel-mode-map (kbd "H-k") #'ghostel-clear)
  (define-key ghostel-mode-map (kbd "H-K") #'ghostel-clear-scrollback)

  (setopt ghostel-keymap-exceptions
          (-> ghostel-keymap-exceptions
              (append '("M-`"
                        "M-~"
                        "M-Z"))
              cl-remove-duplicates))

  (setopt ghostel-readonly-default-mode 'emacs)

  (advice-add 'ghostel--mode-line-refresh :after
              #'nomis/ghostel--append-fast-exit-indicator))

;;; End

(provide 'nomis-ghostel)
