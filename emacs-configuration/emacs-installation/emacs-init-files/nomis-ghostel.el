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
(defvar ghostel-identity)
(declare-function ghostel-clear "ghostel")
(declare-function ghostel-clear-scrollback "ghostel")

;;;; ghostel config

(setopt ghostel-module-directory "~/development-100/repositories/software-installations/emacs-downloads/ghostel")

(defun nomis/ghostel--append-fast-exit-indicator ()
  "Append fast-exit indicator to `mode-line-process' after ghostel refreshes it."
  (when (memq ghostel--input-mode '(copy emacs))
    (let ((indicator (if ghostel--readonly-fast-exit " [FE-on]" " [FE-off]")))
      (setq mode-line-process
            (if (listp mode-line-process)
                (append mode-line-process (list indicator))
              (list mode-line-process indicator))))))

(defun nomis/ghostel-buffer-name (_title)
  "Name ghostel buffers as \"*ghostel<N>*\" using ghostel's instance number."
  (format "*ghostel<%s>"
          (alist-get 'instance ghostel-identity)))

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
              #'nomis/ghostel--append-fast-exit-indicator)

  (setopt ghostel-buffer-name-function #'nomis/ghostel-buffer-name))

;;; End

(provide 'nomis-ghostel)
