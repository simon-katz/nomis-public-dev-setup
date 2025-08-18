;;; eca.el --- AI pair programming via ECA (Editor Code Assistant) -*- lexical-binding: t; -*-
;; Copyright (C) 2025 Eric Dallo
;; Author: Eric Dallo <ercdll1337@gmail.com>
;; Maintainer: Eric Dallo <ercdll1337@gmail.com>
;; Package-Version: 20250818.1523
;; Package-Revision: 01c01d031549
;; Package-Requires: ((emacs "28.1") (dash "2.18.0") (f "0.20.0") (markdown-mode "2.3") (compat "30.1"))
;; Keywords: tools
;; Homepage: https://github.com/editor-code-assistant/eca-emacs
;;
;; SPDX-License-Identifier: Apache-2.0
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  The ECA (Editor Code Assistant) client for Emacs to
;;  add AI code assistant tools.  Heavily insipired on
;;  lsp-mode for parsing and handling jsonrpc messages.
;;
;;; Code:

(require 'cl-lib)
(require 'smerge-mode)

(require 'eca-util)
(require 'eca-process)
(require 'eca-api)
(require 'eca-chat)
(require 'eca-mcp)
(require 'eca-editor)

(defgroup eca nil
  "ECA group."
  :group 'eca)

;; Variables

(defcustom eca-before-initialize-hook nil
  "List of functions to be called before ECA has been initialized."
  :type 'hook
  :group 'eca)

(defcustom eca-after-initialize-hook nil
  "List of functions to be called after ECA has been initialized."
  :type 'hook
  :group 'eca)

;; Internal

(defun eca--get-message-type (json-data)
  "Get the message type from JSON-DATA."
  (if (plist-member json-data :id)
      (if (plist-member json-data :error)
          'response-error
        (if (plist-member json-data :method)
            'request
          'response))
    'notification))

(defun eca--handle-show-message (params)
  "Handle the show-message notification with PARAMS."
  (let ((type (plist-get params :type))
        (msg (plist-get params :message)))
    (pcase type
      ("error" (eca-error msg))
      ("warning" (eca-warn msg))
      ("info" (eca-info msg)))))

(defun eca--tool-server-updated (session server)
  "Handle tool server updated message with SERVER for SESSION."
  (setf (eca--session-tool-servers session)
        (eca-assoc (eca--session-tool-servers session)
                   (plist-get server :name)
                   server))
  (eca-chat--handle-mcp-server-updated session server)
  (eca-mcp--handle-mcp-server-updated session server))

(defun eca--handle-server-notification (session notification)
  "Handle NOTIFICATION sent by server for SESSION."
  (let ((method (plist-get notification :method))
        (params (plist-get notification :params)))
    (pcase method
      ("chat/contentReceived" (eca-chat-content-received session params))
      ("tool/serverUpdated" (eca--tool-server-updated session params))
      ("$/showMessage" (eca--handle-show-message params))
      (_ (eca-warn "Unknown server notification %s" method)))))

(defun eca--handle-server-request (session request)
  "Handle REQUEST sent by server for SESSION."
  (let ((method (plist-get request :method))
        (params (plist-get request :params)))
    (pcase method
      ("editor/getDiagnostics" (eca-editor-get-diagnostics session params))
      (_ (eca-warn "Unknown server request %s" method)))))

(defun eca--handle-message (session json-data)
  "Handle raw message JSON-DATA for SESSION."
  (let ((id (plist-get json-data :id))
        (result (plist-get json-data :result)))
    (condition-case _err
        (pcase (eca--get-message-type json-data)
          ('response (-let [(success-callback) (plist-get (eca--session-response-handlers session) id)]
                       (when success-callback
                         (cl-remf (eca--session-response-handlers session) id)
                         (funcall success-callback result))))
          ('response-error (-let [(_ error-callback) (plist-get (eca--session-response-handlers session) id)]
                             (when error-callback
                               (cl-remf (eca--session-response-handlers session) id)
                               (funcall error-callback (plist-get json-data :error)))))
          ('notification (eca--handle-server-notification session json-data))
          ('request (let ((response (eca--handle-server-request session json-data)))
                      (eca-api-send-request-response session json-data response))))
      ;; TODO handle errors
      (error nil))))

(defun eca--initialize (session)
  "Send the initialize request for SESSION."
  (run-hooks 'eca-before-initialize-hook)
  (setf (eca--session-status session) 'starting)
  (eca-api-request-async
   session
   :method "initialize"
   :params (list :processId (emacs-pid)
                 :clientInfo (list :name "emacs"
                                   :version (emacs-version))
                 :capabilities (list :codeAssistant (list :chat t
                                                          :editor (list :diagnostics t)))
                 :initializationOptions (list :chatBehavior eca-chat-custom-behavior)
                 :workspaceFolders (vconcat (-map (lambda (folder)
                                                    (list :uri (eca--path-to-uri folder)
                                                          :name (file-name-nondirectory (directory-file-name folder))))
                                                  (eca--session-workspace-folders session))))
   :success-callback (-lambda ((&plist :chatWelcomeMessage msg
                                       :chatBehaviors chat-behaviors
                                       :chatDefaultBehavior chat-default-behavior
                                       :chatDefaultModel chat-default-model
                                       :models models))
                       (setf (eca--session-status session) 'started)
                       (setf (eca--session-chat-welcome-message session) msg)
                       (setf (eca--session-models session) models)
                       (setf (eca--session-chat-behaviors session) chat-behaviors)
                       (setf (eca--session-chat-default-model session) chat-default-model)
                       (setf (eca--session-chat-default-behavior session) chat-default-behavior)
                       (eca-api-notify session :method "initialized")
                       (eca-info "Started with workspaces: %s" (string-join (eca--session-workspace-folders session) ","))
                       (eca-chat-open session)
                       (run-hooks 'eca-after-initialize-hook))
   :error-callback (lambda (e) (eca-error e))))

(defun eca--discover-workspaces ()
  "Ask user for workspaces."
  (let ((root (eca-find-root-for-buffer))
        (add-workspace? "yes")
        (workspaces '()))
    (while (string= "yes" add-workspace?)
      (let ((new-workspace (read-directory-name "Select the workspace root:" root)))
        (push new-workspace workspaces)
        (setq add-workspace? (completing-read "Add more workspaces?"
                                              (lambda (string pred action)
                                                (if (eq action 'metadata)
                                                    `(metadata (display-sort-function . identity))
                                                  (complete-with-action action '("no" "yes") string pred)))))))
    workspaces))

;; Public

;;;###autoload
(defun eca (&optional arg)
  "Start or switch to a eca session.
When ARG is current prefix, ask for workspace roots to use."
  (interactive "P")
  (let* ((workspaces (if (equal arg '(4))
                         (eca--discover-workspaces)
                       (list (eca-find-root-for-buffer))))
         (session (or (eca-session)
                      (eca-create-session workspaces))))
    (pcase (eca--session-status session)
      ('stopped (eca-process-start session
                                   (lambda ()
                                     (eca--initialize session))
                                   (-partial #'eca--handle-message session)))
      ('started (eca-chat-open session))
      ('starting (eca-info "eca server is already starting")))))

;;;###autoload
(defun eca-stop ()
  "Stop eca if running."
  (interactive)
  (let ((session (eca-session)))
    (when (eca-process-running-p session)
      (eca-info "Shutting down...")
      (eca-api-request-sync session :method "shutdown")
      (eca-api-notify session :method "exit")
      (eca-process-stop session)
      (eca-chat-exit session)
      (eca-mcp-details-exit session)
      (eca-delete-session session))))

;;;###autoload
(defun eca-restart ()
  "Restart eca, if not running just start."
  (interactive)
  (eca-stop)
  (eca))

;;;###autoload
(defun eca-workspaces ()
  "Return workspaces used by current session."
  (interactive)
  (eca-assert-session-running (eca-session))
  (eca-info "Workspaces: %s" (eca--session-workspace-folders (eca-session))))

;;;###autoload
(defun eca-keep-all-suggested-changes ()
  "Keep all of the ECA file change suggestion."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (ignore-errors (funcall #'smerge-keep-lower))
    (while (ignore-errors (not (smerge-next)))
      (funcall #'smerge-keep-lower))))

;;;###autoload
(defun eca-discard-all-suggested-changes ()
  "Discard all of the ECA file change suggestion."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (ignore-errors (funcall #'smerge-keep-upper))
    (while (ignore-errors (not (smerge-next)))
      (funcall #'smerge-keep-upper))))

(provide 'eca)
;;; eca.el ends here
