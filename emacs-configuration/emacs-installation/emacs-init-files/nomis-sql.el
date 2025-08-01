;;;; Init stuff -- Nomis SQL tailoring --  -*- lexical-binding: t -*-

;;;; ___________________________________________________________________________

(add-hook 'sql-mode-hook 'lsp)

;; (add-hook 'sql-mode-hook 'sqlup-mode)
;; (add-hook 'sql-interactive-mode-hook 'sqlup-mode)

;; (global-set-key (kbd "C-c u") 'sqlup-capitalize-keywords-in-region)

(add-hook 'sql-mode-hook 'sqlind-minor-mode)

(defun nomis/initialize-sql-interactive-mode ()
  (define-key sql-interactive-mode-map (kbd "H-k") 'comint-clear-buffer)
  (toggle-truncate-lines t))

(add-hook 'sql-interactive-mode-hook
          'nomis/initialize-sql-interactive-mode)

;;;; ___________________________________________________________________________

(with-eval-after-load 'sql

  (defun sql-send-buffer ()
    "Send the buffer contents to the SQL process."
    (interactive)
    (message-box "sql-send-buffer is disabled — too easy to do it accidentally."))

  (require 'eval-sexp-fu)
  (define-eval-sexp-fu-flash-command sql-send-paragraph
    (eval-sexp-fu-flash (cons (save-excursion
                                (backward-paragraph)
                                (point))
                              (save-excursion
                                (forward-paragraph)
                                (point)))))

  ;; I'm used to this key binding in TablePlus:
  (define-key sql-mode-map (kbd "M-RET") 'sql-send-paragraph)


  ;; HACKY WIP:

  (defface nomis/sql-separator-face
    '((((background dark)) (:foreground "Green2" :italic t))
      (t (:foreground "Green4")))
    "Face for reloading messages.")

  (defun nomis/sql/goto-eob-in-all-buffer-windows ()
    (goto-char (point-max))
    (dolist (w (get-buffer-window-list (current-buffer)))
      (set-window-point w (point-max))))

  (defun sql-send-string (str)
    "Send the string STR to the SQL process."
    (interactive "sSQL Text: ")

    (let ((comint-input-sender-no-newline nil)
          (s (replace-regexp-in-string "[[:space:]\n\r]+\\'" "" str)))
      (if (sql-buffer-live-p sql-buffer)
          (progn
            ;; Ignore the hoping around...
            (save-excursion
              ;; Set product context
              (with-current-buffer sql-buffer
                ;; :nomis-hack Move to end of buffer.
                (nomis/sql/goto-eob-in-all-buffer-windows)
                (when sql-debug-send
                  (message ">>SQL> %S" s))
                ;; :nomis-hack Insert query.
                (insert "\n\n\n________________________________________\n")
                (insert "Query:")
                (insert s)
                (insert "\n\nResult:\n")
                (comint-set-process-mark)

                ;; Send the string (trim the trailing whitespace)
                (sql-input-sender (get-buffer-process (current-buffer)) s)

                ;; Send a command terminator if we must
                (sql-send-magic-terminator sql-buffer s sql-send-terminator)

                (when sql-pop-to-buffer-after-send-region
                  (message "Sent string to buffer %s" sql-buffer))
                (nomis/sql/goto-eob-in-all-buffer-windows)))

            ;; Display the sql buffer
            (sql-display-buffer sql-buffer))

        ;; We don't have no stinkin' sql
        (user-error "No SQL process started")))))

;;;; ___________________________________________________________________________

(provide 'nomis-sql)
