;;; Very general stuff (new + lexical) -- -*- lexical-binding: t -*-

;;; nomis/temporarily-disable-keys

(defconst -nomis/ignore-all-keys-keymap
  (let* ((km (make-sparse-keymap)))
    (define-key km [t] (lambda ()
                         (interactive)
                         (nomis/msg/pulse-buffer-error)))
    km))

(defun nomis/temporarily-disable-keys (&optional pulse-on-restore?)
  ;; For a list of keymaps see
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Active-Keymaps.html
  ;;
  ;; TODO: Disable the keymap in the `keymap` text or overlay property at point.
  (let* ((old-gkm (current-global-map))
         (old-lkm (current-local-map))
         (old-mmma minor-mode-map-alist))
    (use-global-map -nomis/ignore-all-keys-keymap)
    (use-local-map nil)
    (setq minor-mode-map-alist nil)
    (run-at-time 1
                 nil
                 (lambda ()
                   (use-global-map old-gkm)
                   (use-local-map old-lkm)
                   (setq minor-mode-map-alist old-mmma)
                   (when pulse-on-restore?
                     (nomis/msg/pulse-buffer))))))

;;; End

(provide 'nomis-very-general-stuff-new-lexical)
