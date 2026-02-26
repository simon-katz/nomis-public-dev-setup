;;; nomis-very-general-stuff-new-lexical.el --- Very general stuff (new + lexical)  -*- lexical-binding: t; -*-

;;; Code:

;;;; `nomis/define-key-with-filter`

(defmacro nomis/define-key-with-filter (keymap key command condition)
  "In KEYMAP, bind KEY to COMMAND only if CONDITION is met.
Otherwise, the keybinding is ignored, letting Emacs search lower-priority maps."
  ;; The `menu-item` trick with `:filter` is an Emacs idiom for conditional
  ;; keybindings. When the filter returns `nil`, Emacs treats the binding as if
  ;; it doesn't exist and falls through to lower-priority keymaps.
  `(define-key ,keymap ,key
               `(menu-item ,(symbol-name ,command) ,,command
                           :filter (lambda (cmd) (when ,',condition cmd)))))

;;;; `nomis/temporarily-disable-keys`

(defconst -nomis/ignore-all-keys-keymap
  (let* ((km (make-sparse-keymap)))
    (define-key km [t] (lambda ()
                         (interactive)
                         (nomis/msg/pulse-buffer-error))) ; noflycheck -- TODO: Delete this comment and move this functionality to a place loader in the load sequence
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
                     (nomis/msg/pulse-buffer)))))) ; noflycheck -- see earlier `noflycheck` comment

;;; End

(provide 'nomis-very-general-stuff-new-lexical)
