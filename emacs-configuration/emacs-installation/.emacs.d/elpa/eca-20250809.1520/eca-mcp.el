;;; eca-mcp.el --- ECA (Editor Code Assistant) mcp -*- lexical-binding: t; -*-
;; Copyright (C) 2025 Eric Dallo
;;
;; SPDX-License-Identifier: Apache-2.0
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  The ECA (Editor Code Assistant) mcp.
;;
;;; Code:

(require 'compat)

(require 'eca-util)
(require 'eca-process)

(defcustom eca-mcp-details-position-params `((display-buffer-in-side-window)
                                             (side . right)
                                             (window-width . 0.35))
  "Position params for mcp details display."
  :type 'alist
  :group 'eca)

(defface eca-mcp-details-tool-face
  '((t (:inherit hl-line :slant italic)))
  "Face for tools showed in mcp details buffer."
  :group 'eca)

(defface eca-mcp-details-tool-disabled-face
  '((t (:inherit hl-line :slant italic :strike-through t)))
  "Face for tools showed in mcp details buffer."
  :group 'eca)

(defface eca-mcp-details-button-face
  '((t (:inherit button)))
  "Face for buttons in mcp details buffer."
  :group 'eca)

;; Internal

(defun eca-mcp-details-buffer-name (session)
  "Return the chat buffer name for SESSION."
  (format  "<eca-mcp-details:%s>" (eca--session-id session)))

(defun eca-mcp--get-details-buffer (session)
  "Get the eca mcp buffer for SESSION."
  (get-buffer (eca-mcp-details-buffer-name session)))

(defun eca-mcp--create-details-buffer (session)
  "Create the eca mcp details buffer for SESSION."
  (get-buffer-create (generate-new-buffer-name (eca-mcp-details-buffer-name session))))

(defun eca-mcp--refresh-server-details (session)
  "Refresh the MCP server details for SESSION."
  (when (buffer-live-p (get-buffer (eca-mcp-details-buffer-name session)))
    (with-current-buffer (eca-mcp--get-details-buffer session)
      (erase-buffer)
      (insert (propertize "MCP servers" 'font-lock-face 'helpful-heading))
      (insert "\n\n")
      (seq-doseq (server (-sort  (lambda (a b)
                                   (string-lessp (plist-get a :name)
                                                 (plist-get b :name)))
                                 (eca-vals (eca--session-tool-servers session))))
        (-let (((&plist :name name :command command :args args
                        :status status :tools tools) server))
          (insert (propertize name 'font-lock-face 'bold))
          (insert " - ")
          (insert (propertize status
                              'font-lock-face (pcase status
                                                ("running" 'success)
                                                ("starting" 'warning)
                                                ("failed" 'error)
                                                ("stopped" 'default)
                                                ("stopping" 'default)
                                                ("disabled" 'shadow))))
          (insert " "
                  (if (or (string= "running" status)
                          (string= "starting" status))
                      (eca-buttonize
                       (propertize "stop" 'font-lock-face 'eca-mcp-details-button-face)
                       (lambda () (eca-api-notify session
                                                  :method "mcp/stopServer"
                                                  :params (list :name name))))
                    (eca-buttonize
                     (propertize "start" 'font-lock-face 'eca-mcp-details-button-face)
                     (lambda () (eca-api-notify session
                                                :method "mcp/startServer"
                                                :params (list :name name))))))
          (insert "\n")
          (if (seq-empty-p tools)
              (insert (propertize "No tools available" 'font-lock-face font-lock-doc-face))
            (progn
              (insert (propertize "Tools: " 'font-lock-face font-lock-doc-face))
              (seq-doseq (tool tools)
                (insert (propertize (plist-get tool :name)
                                    'font-lock-face (if (plist-get tool :disabled)
                                                        'eca-mcp-details-tool-disabled-face
                                                      'eca-mcp-details-tool-face)) " "))))
          (when command
            (insert "\n")
            (insert (propertize "Command: " 'font-lock-face font-lock-doc-face))
            (insert command " " (string-join args " ")))
          (when (string= "failed" status)
            (insert "\n")
            (insert (propertize (format "Failed to start, check %s for details"
                                        (buttonize
                                         "eca stderr buffer"
                                         (lambda(_) (eca-process-show-stderr session))))
                                'font-lock-face 'error))))
        (insert "\n\n")))))

;; Public

(define-derived-mode eca-mcp-details-mode fundamental-mode "eca-mcp-details"
  "Major mode for ECA mcp details."
  :group 'eca
  (visual-line-mode)
  (eca-mcp--refresh-server-details (eca-session)))

(defun eca-mcp-servers (session)
  "Return all servers that are not from eca server SESSION, the MCP servers."
  (eca-vals (eca-dissoc (eca--session-tool-servers session) "ECA")))

(defun eca-mcp--handle-mcp-server-updated (session _server)
  "Handle mcp SERVER updated for SESSION."
  (eca-mcp--refresh-server-details session))

(defun eca-mcp-details-exit (session)
  "Exit the ECA mcp details for SESSION."
  (when (buffer-live-p (get-buffer (eca-mcp-details-buffer-name session)))
    (with-current-buffer (eca-mcp--get-details-buffer session)
      (goto-char (point-max))
      (setq-local mode-line-format '("*Closed session*"))
      (rename-buffer (concat (buffer-name) ":closed") t)
      (when-let* ((window (get-buffer-window (eca-mcp--get-details-buffer session))))
        (quit-window nil window)))))

;;;###autoload
(defun eca-mcp-details ()
  "List MCP servers with their status and options."
  (interactive)
  (let ((session (eca-session)))
    (eca-assert-session-running session)
    (unless (buffer-live-p (eca-mcp--get-details-buffer session))
      (eca-mcp--create-details-buffer session))
    (with-current-buffer (eca-mcp--get-details-buffer session)
      (unless (derived-mode-p 'eca-mcp-details-mode)
        (eca-mcp-details-mode))
      (if (window-live-p (get-buffer-window (buffer-name)))
          (select-window (get-buffer-window (buffer-name)))
        (display-buffer (current-buffer) eca-mcp-details-position-params)
        (select-window (get-buffer-window (current-buffer)))
        (set-window-buffer (get-buffer-window (current-buffer)) (current-buffer))))))

(provide 'eca-mcp)
;;; eca-mcp.el ends here
