;;; eca-chat.el --- ECA (Editor Code Assistant) chat -*- lexical-binding: t; -*-
;; Copyright (C) 2025 Eric Dallo
;;
;; SPDX-License-Identifier: Apache-2.0
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  The ECA (Editor Code Assistant) chat.
;;
;;; Code:

(require 'f)
(require 'markdown-mode)
(require 'compat)

(require 'eca-util)
(require 'eca-api)
(require 'eca-mcp)

;; Variables

(defcustom eca-chat-mode-hook '()
  "Hooks to run after entering in eca chat mode hook."
  :type 'hook
  :group 'eca)

(defcustom eca-chat-window-width 0.35
  "The width of `eca' dedicated chat window."
  :type 'integer
  :group 'eca)

(defcustom eca-chat-position-params `((display-buffer-in-side-window)
                                      (side . right)
                                      (window-width . ,eca-chat-window-width))
  "Position params for each chat display."
  :type 'alist
  :group 'eca)

(defcustom eca-chat-prompt-prefix "> "
  "The prompt prefix string used in eca chat buffer."
  :type 'string
  :group 'eca)

(defcustom eca-chat-prompt-prefix-loading "⏳ "
  "The prompt prefix string used in eca chat buffer when loading."
  :type 'string
  :group 'eca)

(defcustom eca-chat-context-prefix "@"
  "The context prefix string used in eca chat buffer."
  :type 'string
  :group 'eca)

(defcustom eca-chat-expandable-block-open-symbol "⏵ "
  "The string used in eca chat buffer for blocks in open mode like tool calls."
  :type 'string
  :group 'eca)

(defcustom eca-chat-expandable-block-close-symbol "⏷ "
  "The string used in eca chat buffer for blocks in close mode like tool calls."
  :type 'string
  :group 'eca)

(defcustom eca-chat-mcp-tool-call-loading-symbol "⏳"
  "The string used in eca chat buffer for mcp tool calls while loading."
  :type 'string
  :group 'eca)

(defcustom eca-chat-mcp-tool-call-error-symbol "❌"
  "The string used in eca chat buffer for mcp tool calls when error."
  :type 'string
  :group 'eca)

(defcustom eca-chat-mcp-tool-call-success-symbol "✅"
  "The string used in eca chat buffer for mcp tool calls when success."
  :type 'string
  :group 'eca)

(defcustom eca-chat-custom-model nil
  "Which model to use during chat, nil means use server's default.
Must be a valid model supported by server, check `eca-chat-select-model`."
  :type 'string
  :group 'eca)

(defcustom eca-chat-custom-behavior nil
  "Which chat behavior to use, if nil use server's default."
  :type 'string
  :group 'eca)

(defcustom eca-chat-usage-string-format '(:message-cost " / " :session-cost)
  "Format to show about chat usage tokens/costs."
  :type '(repeat
          (choice
           (string :tag "any string like separators")
           (const :tag "Last input tokens sent" :message-input-tokens)
           (const :tag "Last output tokens received" :message-output-tokens)
           (const :tag "Total tokens sent + received" :session-tokens)
           (const :tag "Total session cost" :session-cost)
           (const :tag "Last message cost" :mesage-cost)))
  :group 'eca)

;; Faces

(defface eca-chat-prompt-prefix-face
  '((t (:foreground "lime green" :weight bold)))
  "Face for the `eca-chat-prompt-prefix`."
  :group 'eca)

(defface eca-chat-prompt-stop-face
  '((t (:inherit error :underline t :weight bold)))
  "Face for the stop action when loading."
  :group 'eca)

(defface eca-chat-tool-call-run-face
  '((t (:inherit success :underline t :weight bold)))
  "Face for the run tool call action."
  :group 'eca)

(defface eca-chat-tool-call-cancel-face
  '((t (:inherit error :underline t :weight bold)))
  "Face for the cancel tool call action."
  :group 'eca)

(defface eca-chat-context-unlinked-face
  '((t (:foreground "gold")))
  "Face for contexts to be added."
  :group 'eca)

(defface eca-chat-context-file-face
  '((t (:foreground "coral" :underline t)))
  "Face for contexts of file type."
  :group 'eca)

(defface eca-chat-context-repo-map-face
  '((t (:foreground "turquoise" :underline t)))
  "Face for contexts of repoMap type."
  :group 'eca)

(defface eca-chat-user-messages-face
  '((t :inherit font-lock-doc-face))
  "Face for the user sent messages in chat."
  :group 'eca)

(defface eca-chat-system-messages-face
  '((t :inherit font-lock-builtin-face))
  "Face for the system messages in chat."
  :group 'eca)

(defface eca-chat-reason-label-face
  '((t :inherit font-lock-comment-face :underline t))
  "Face for the reason messages in chat."
  :group 'eca)

(defface eca-chat-mcp-tool-call-label-face
  '((t :inherit font-lock-function-call-face :underline t))
  "Face for the MCP tool calls in chat."
  :group 'eca)

(defface eca-chat-mcp-tool-call-content-face
  '()
  "Face for the MCP tool calls in chat."
  :group 'eca)

(defface eca-chat--tool-call-argument-key-face
  '()
  "Face for the MCP tool calls's argument key in chat."
  :group 'eca)

(defface eca-chat--tool-call-argument-value-face
  '((t :weight bold))
  "Face for the MCP tool calls's argument value in chat."
  :group 'eca)

(defface eca-chat-welcome-face
  '((t :inherit font-lock-builtin-face))
  "Face for the welcome message in chat."
  :group 'eca)

(defface eca-chat-option-key-face
  '((t :inherit font-lock-doc-face))
  "Face for the option keys in header-line of the chat."
  :group 'eca)

(defface eca-chat-option-value-face
  '((t :weight bold))
  "Face for the option values in header-line of the chat."
  :group 'eca)

(defface eca-chat-usage-string-face
  '((t :inherit font-lock-doc-face))
  "Face for the strings segments in usage string in mode-line of the chat."
  :group 'eca)

;; Internal

(defvar-local eca-chat--closed nil)
(defvar-local eca-chat--history '())
(defvar-local eca-chat--history-index -1)
(defvar-local eca-chat--id nil)
(defvar-local eca-chat--last-request-id 0)
(defvar-local eca-chat--context '())
(defvar-local eca-chat--spinner-string "")
(defvar-local eca-chat--spinner-timer nil)
(defvar-local eca-chat--progress-text "")
(defvar-local eca-chat--last-user-message-pos nil)
(defvar-local eca-chat--chat-loading nil)
(defvar-local eca-chat--session-cost nil)
(defvar-local eca-chat--message-cost nil)
(defvar-local eca-chat--message-input-tokens nil)
(defvar-local eca-chat--message-output-tokens nil)
(defvar-local eca-chat--session-tokens nil)

(defun eca-chat-buffer-name (session)
  "Return the chat buffer name for SESSION."
  (format "<eca-chat:%s>" (eca--session-id session)))

(defvar eca-chat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<backspace>") #'eca-chat--key-pressed-backspace)
    (define-key map (kbd "DEL") #'eca-chat--key-pressed-backspace)
    (define-key map (kbd "S-<return>") #'eca-chat--key-pressed-newline)
    (define-key map (kbd "C-<up>") #'eca-chat--key-pressed-previous-prompt-history)
    (define-key map (kbd "C-<down>") #'eca-chat--key-pressed-next-prompt-history)
    (define-key map (kbd "C-k") #'eca-chat-reset)
    (define-key map (kbd "C-l") #'eca-chat-clear)
    (define-key map (kbd "C-t") #'eca-chat-talk)
    (define-key map (kbd "<return>") #'eca-chat--key-pressed-return)
    (define-key map (kbd "<tab>") #'eca-chat--key-pressed-tab)
    map)
  "Keymap used by `eca-chat-mode'.")

(defun eca-chat--spinner-start (session)
  "Start modeline spinner for SESSION."
  (setq eca-chat--spinner-timer
        (with-current-buffer (eca-chat--get-buffer session)
          (run-with-timer
           0
           0.5
           (lambda ()
             (when eca-chat--spinner-timer
               (if (eq 3 (length eca-chat--spinner-string))
                   (setq eca-chat--spinner-string ".")
                 (setq eca-chat--spinner-string (concat eca-chat--spinner-string ".")))
               (force-mode-line-update)))))))

(defun eca-chat--spinner-stop ()
  "Stop modeline spinner."
  (when eca-chat--spinner-timer
    (cancel-timer eca-chat--spinner-timer)
    (setq eca-chat--spinner-timer nil))
  (setq eca-chat--spinner-string ""))

(defun eca-chat--behavior (session)
  "The chat behavior considering what's in SESSION and user option."
  (or eca-chat-custom-behavior
      (eca--session-chat-default-behavior session)))

(defun eca-chat--model (session)
  "The chat model considering what's in SESSION and user option."
  (or eca-chat-custom-model
      (eca--session-chat-default-model session)))

(defun eca-chat--mcps-summary (session)
  "The summary of MCP servers for SESSION."
  (let* ((running 0) (starting 0) (failed 0)
         (propertize-fn (lambda (n face &optional add-slash?)
                          (unless (zerop n)
                            (concat
                             (propertize (number-to-string n) 'font-lock-face face)
                             (when add-slash? (propertize "/" 'font-lock-face 'font-lock-comment-face))))))
         (mcp-servers (eca-mcp-servers session)))
    (if (seq-empty-p mcp-servers)
        "0"
      (progn
        (seq-doseq (mcp-server mcp-servers)
          (pcase (plist-get mcp-server :status)
            ("running" (cl-incf running))
            ("starting" (cl-incf starting))
            ("failed" (cl-incf failed))))
        (concat (funcall propertize-fn failed 'error (or (> running 0) (> starting 0)))
                (funcall propertize-fn starting 'warning (> running 0))
                (funcall propertize-fn running 'success))))))

(defun eca-chat--insert-prompt-string ()
  "Insert the prompt and context string adding overlay metadatas."
  (let ((prompt-area-ov (make-overlay (line-beginning-position) (1+ (line-beginning-position)) (current-buffer))))
    (overlay-put prompt-area-ov 'eca-chat-prompt-area t))
  (let ((context-area-ov (make-overlay (line-beginning-position) (line-end-position) (current-buffer))))
    (overlay-put context-area-ov 'eca-chat-context-area t))
  (insert (propertize eca-chat-context-prefix 'font-lock-face 'eca-chat-context-unlinked-face))
  (insert "\n")
  (let ((prompt-field-ov (make-overlay (line-beginning-position) (1+ (line-beginning-position)) (current-buffer))))
    (overlay-put prompt-field-ov 'eca-chat-prompt-field t)
    (overlay-put prompt-field-ov 'before-string (propertize eca-chat-prompt-prefix 'font-lock-face 'eca-chat-prompt-prefix-face))))

(defun eca-chat--clear (session)
  "Clear the chat for SESSION."
  (with-current-buffer (eca-chat--get-buffer session)
    (erase-buffer)
    (remove-overlays (point-min) (point-max))
    (insert "\n")
    (eca-chat--insert-prompt-string)
    (eca-chat--refresh-context)))

(defun eca-chat--buttonize (text callback)
  "Create a actionable TEXT that call CALLBACK when actioned."
  (let ((km (make-sparse-keymap))
        (callback-int (lambda (&rest _)
                        (interactive)
                        (funcall callback))))
    (define-key km (kbd "<mouse-1>") callback-int)
    (define-key km (kbd "<tab>") callback-int)
    (define-key km (kbd "<return>") callback-int)
    (propertize text
                'eca-chat-on-action callback
                'pointer 'hand
                'keymap km)))

(defun eca-chat--stop-prompt (session)
  "Stop the running chat prompt for SESSION."
  (when eca-chat--chat-loading
    (with-current-buffer (eca-chat--get-buffer session)
      (eca-api-notify session
                      :method "chat/promptStop"
                      :params (list :chatId eca-chat--id))
      (eca-chat--set-chat-loading session nil))))

(defun eca-chat--set-chat-loading (session loading)
  "Set the SESSION chat to a loading state if LOADING is non nil.
Otherwise to a not loading state."
  (unless (eq eca-chat--chat-loading loading)
    (setq-local eca-chat--chat-loading loading)
    (let ((prompt-field-ov (eca-chat--prompt-field-ov))
          (stop-text (eca-chat--buttonize
                      (propertize "stop" 'font-lock-face 'eca-chat-prompt-stop-face)
                      (lambda () (eca-chat--stop-prompt session)))))
      (if eca-chat--chat-loading
          (progn
            (overlay-put prompt-field-ov 'before-string (propertize eca-chat-prompt-prefix-loading 'font-lock-face 'default))
            (save-excursion
              (goto-char (overlay-start prompt-field-ov))
              (insert stop-text)))
        (progn
          (overlay-put prompt-field-ov 'before-string (propertize eca-chat-prompt-prefix 'font-lock-face 'eca-chat-prompt-prefix-face))
          (save-excursion
            (goto-char (overlay-start prompt-field-ov))
            (delete-region (point) (+ (point) (length stop-text)))))))))

(defun eca-chat--set-prompt (text)
  "Set the chat prompt to be TEXT."
  (-some-> (eca-chat--prompt-field-start-point) (goto-char))
  (delete-region (point) (line-end-position))
  (insert text))

(defun eca-chat--cycle-history (n)
  "Cycle history by N."
  (when (and eca-chat--history (eca-chat--point-at-prompt-field-p))
    (when (and (>= (+ eca-chat--history-index n) 0)
               (nth (+ eca-chat--history-index n) eca-chat--history))
      (cl-incf eca-chat--history-index n)
      (eca-chat--set-prompt (nth eca-chat--history-index eca-chat--history)))))

(defun eca-chat--key-pressed-previous-prompt-history ()
  "Cycle previous the prompt history."
  (interactive)
  (eca-chat--cycle-history 1))

(defun eca-chat--key-pressed-next-prompt-history ()
  "Cycle next the prompt history."
  (interactive)
  (eca-chat--cycle-history -1))

(defun eca-chat--key-pressed-newline ()
  "Insert a newline character at point."
  (interactive)
  (when (eq (line-beginning-position) (eca-chat--prompt-field-start-point))
    (insert "\n")))

(defun eca-chat--key-pressed-tab ()
  "Expand tool call if point is at expandable content, or use default behavior."
  (interactive)
  (if-let ((ov (eca-chat--expandable-content-at-point)))
      (eca-chat--expandable-content-toggle (overlay-get ov 'eca-chat--expandable-content-id))
    (call-interactively #'markdown-cycle)))

(defun eca-chat--prompt-field-ov ()
  "Return the overlay for the prompt field."
  (-first (-lambda (ov) (eq t (overlay-get ov 'eca-chat-prompt-field)))
          (overlays-in (point-min) (point-max))))

(defun eca-chat--prompt-field-start-point ()
  "Return the metadata overlay for the prompt field start point."
  (overlay-start (eca-chat--prompt-field-ov)))

(defun eca-chat--prompt-context-field-ov ()
  "Return the overlay for the context field."
  (-first (-lambda (ov) (eq t (overlay-get ov 'eca-chat-context-area)))
          (overlays-in (point-min) (point-max))))

(defun eca-chat--prompt-area-ov ()
  "Return the overlay for the prompt area."
  (-first (-lambda (ov) (eq t (overlay-get ov 'eca-chat-prompt-area)))
          (overlays-in (point-min) (point-max))))

(defun eca-chat--prompt-area-start-point ()
  "Return the metadata overlay for the prompt area start point."
  (-some-> (eca-chat--prompt-area-ov)
    (overlay-start)))

(defun eca-chat--key-pressed-backspace ()
  "Delete the character before point, unless at the prompt or context boundary.
Checks if it's in a context, removing it if so.
This is similar to `backward-delete-char' but protects the prompt/context line."
  (interactive)
  (let* ((cur-ov (car (overlays-in (line-beginning-position) (point))))
         (text (thing-at-point 'symbol))
         (context-item (-some->> text
                         (get-text-property 0 'eca-chat-context-item))))
    (cond
     ((and cur-ov
           context-item)
      (setq-local eca-chat--context (delete context-item eca-chat--context))
      (eca-chat--refresh-context)
      (end-of-line))

     ((and cur-ov
           (<= (point) (overlay-start cur-ov)))
      (ding))

     ((and cur-ov
           (overlay-get cur-ov 'eca-chat-context-area)
           (or (string= " " (string (char-before (point))))
               (string= eca-chat-context-prefix (string (char-before (point))))))
      ;; trying to remove a space or context-prefix
      )

     (t (delete-char -1)))))

(defun eca-chat--key-pressed-return ()
  "Send the current prompt to eca process if in prompt."
  (interactive)
  (with-current-buffer (eca-chat--get-buffer (eca-session))
    (let* ((prompt-start (eca-chat--prompt-field-start-point))
           (session (eca-session))
           (prompt (save-excursion
                     (goto-char prompt-start)
                     (string-trim (buffer-substring-no-properties (point) (point-max))))))
      (cond
       ;; check prompt
       ((and (not (string-empty-p prompt))
             (not eca-chat--chat-loading))
        (when (seq-empty-p eca-chat--history) (eca-chat--clear session))
        (add-to-list 'eca-chat--history prompt)
        (setq eca-chat--history-index -1)
        (goto-char prompt-start)
        (delete-region (point) (point-max))
        (eca-chat--set-chat-loading session t)
        (eca-api-request-async
         session
         :method "chat/prompt"
         :params (list :message prompt
                       :request-id (cl-incf eca-chat--last-request-id)
                       :chatId eca-chat--id
                       :model (eca-chat--model session)
                       :behavior (eca-chat--behavior session)
                       :contexts (vconcat eca-chat--context))
         :success-callback (-lambda (res)
                             (setq-local eca-chat--id (plist-get res :chatId)))))

       ;; check it's an actionable text
       ((-some->> (thing-at-point 'symbol) (get-text-property 0 'eca-chat-on-action))
        (-some->> (thing-at-point 'symbol)
          (get-text-property 0 'eca-chat-on-action)
          (funcall)))

       ;; check is inside a expandable text
       ((eca-chat--expandable-content-at-point)
        (let ((ov (eca-chat--expandable-content-at-point)))
          (eca-chat--expandable-content-toggle (overlay-get ov 'eca-chat--expandable-content-id))))

       (t nil)))))

(defun eca-chat--point-at-new-context-p ()
  "Return non-nil if point is at the context area."
  (and (eq (line-number-at-pos (point))
           (line-number-at-pos (eca-chat--prompt-area-start-point)))
       (eolp)))

(defun eca-chat--point-at-prompt-field-p ()
  "Return non-nil if point is at the prompt field area."
  (eq (line-number-at-pos (point))
      (line-number-at-pos (eca-chat--prompt-field-start-point))))

(defun eca-chat--header-line-string (session)
  "Update chat header line for SESSION."
  (let ((model-keymap (make-sparse-keymap))
        (behavior-keymap (make-sparse-keymap))
        (mcp-keymap (make-sparse-keymap)))
    (define-key model-keymap (kbd "<header-line> <mouse-1>") #'eca-chat-select-model)
    (define-key behavior-keymap (kbd "<header-line> <mouse-1>") #'eca-chat-select-behavior)
    (define-key mcp-keymap (kbd "<header-line> <mouse-1>") #'eca-mcp-details)
    (list (propertize "model:"
                      'font-lock-face 'eca-chat-option-key-face
                      'pointer 'hand
                      'keymap model-keymap)
          (propertize (eca-chat--model session)
                      'font-lock-face 'eca-chat-option-value-face
                      'pointer 'hand
                      'keymap model-keymap)
          "  "
          (propertize "behavior:"
                      'font-lock-face 'eca-chat-option-key-face
                      'pointer 'hand
                      'keymap behavior-keymap)
          (propertize (eca-chat--behavior session)
                      'font-lock-face 'eca-chat-option-value-face
                      'pointer 'hand
                      'keymap behavior-keymap)
          "  "
          (propertize "mcps:"
                      'font-lock-face 'eca-chat-option-key-face
                      'pointer 'hand
                      'keymap mcp-keymap)
          (propertize (eca-chat--mcps-summary session)
                      'pointer 'hand
                      'keymap mcp-keymap))))

(defun eca-chat--mode-line-string ()
  "Update chat mode line."
  (let* ((usage-str
          (when (or eca-chat--message-input-tokens
                    eca-chat--message-output-tokens
                    eca-chat--session-tokens
                    eca-chat--message-cost
                    eca-chat--session-cost)
            (-> (-map (lambda (segment)
                        (pcase segment
                          (:message-input-tokens (number-to-string eca-chat--message-input-tokens))
                          (:message-output-tokens (number-to-string eca-chat--message-output-tokens))
                          (:session-tokens (number-to-string eca-chat--session-tokens))
                          (:message-cost (concat "$" eca-chat--message-cost))
                          (:session-cost (concat "$" eca-chat--session-cost))
                          (_ (propertize segment 'font-lock-face 'eca-chat-usage-string-face))))
                      eca-chat-usage-string-format)
                (string-join ""))))
         (fill-space (propertize " "
                                 'display `((space :align-to (- right ,(+ 1 (length usage-str))))))))
    (concat
     (when eca-chat--closed
       (propertize "*Closed session*" 'font-lock-face 'eca-chat-system-messages-face))
     eca-chat--progress-text
     eca-chat--spinner-string
     fill-space
     usage-str)))

(defun eca-chat--get-buffer (session)
  "Get the eca chat buffer for SESSION."
  (get-buffer (eca-chat-buffer-name session)))

(defun eca-chat--create-buffer (session)
  "Create the eca chat buffer for SESSION."
  (get-buffer-create (generate-new-buffer-name (eca-chat-buffer-name session))))

(defun eca-chat--select-window ()
  "Select the Window."
  (select-window (get-buffer-window (buffer-name))))

(defun eca-chat--pop-window ()
  "Pop eca dedicated window if it exists."
  (let ((buffer (current-buffer)))
    (display-buffer buffer eca-chat-position-params)
    (select-window (get-buffer-window buffer))
    (set-window-buffer (get-buffer-window buffer) buffer)))

(defun eca-chat--mark-header ()
  "Mark last messages header."
  (let ((context-start (eca-chat--prompt-area-start-point)))
    (save-excursion
      (goto-char context-start)
      (goto-char (1- (point)))
      (setq-local eca-chat--last-user-message-pos (point)))))

(defun eca-chat--add-header (content)
  "Add CONTENT to the chat just after last user input."
  (when eca-chat--last-user-message-pos
    (save-excursion
      (goto-char eca-chat--last-user-message-pos)
      (insert content))))

(defun eca-chat--add-text-content (text)
  "Add TEXT to the chat current position."
  (let ((context-start (eca-chat--prompt-area-start-point)))
    (save-excursion
      (goto-char context-start)
      (goto-char (1- (point)))
      (insert text)
      (point))))

(defun eca-chat--expandable-content-at-point ()
  "Return expandable content overlay at point, or nil if none."
  (-first (-lambda (ov) (overlay-get ov 'eca-chat--expandable-content-id))
          (overlays-in (line-beginning-position) (point))))

(defun eca-chat--get-expandable-content (id)
  "Return the overlay if there is a expandable content for ID."
  (-first (-lambda (ov) (string= id (overlay-get ov 'eca-chat--expandable-content-id)))
          (overlays-in (point-min) (point-max))))

(defun eca-chat--add-expandable-content (id label content)
  "Add LABEL to the chat current position for ID as a interactive text.
When expanded, shows CONTENT.
Applies LABEL-FACE to label and CONTENT-FACE to content."
  (save-excursion
    (let* ((context-start (eca-chat--prompt-area-start-point))
           (start-point (1- context-start)))
      (goto-char start-point)
      (insert "\n")
      (let ((ov-label (make-overlay (point) (point) (current-buffer))))
        (overlay-put ov-label 'eca-chat--expandable-content-id id)
        (overlay-put ov-label 'eca-chat--expandable-content-toggle nil)
        (insert (propertize label
                            'keymap (let ((km (make-sparse-keymap)))
                                      (define-key km (kbd "<mouse-1>") (lambda () (eca-chat--expandable-content-toggle id)))
                                      (define-key km (kbd "<tab>") (lambda () (eca-chat--expandable-content-toggle id)))
                                      km)
                            'line-prefix eca-chat-expandable-block-open-symbol
                            'help-echo "mouse-1 / tab / RET: expand/collapse"))
        (insert "\n")
        (let* ((start-point (point))
               (_ (insert "\n"))
               (ov-content (make-overlay start-point start-point (current-buffer) nil t)))
          (overlay-put ov-content 'eca-chat--expandable-content-content (propertize content 'line-prefix "   "))
          (overlay-put ov-label 'eca-chat--expandable-content-ov-content ov-content))))))

(defun eca-chat--rename-expandable-content (id label content &optional append-content?)
  "Rename to LABEL and CONTENT the expandable content of id ID."
  (when-let* ((ov-label (eca-chat--get-expandable-content id)))
    (let* ((ov-content (overlay-get ov-label 'eca-chat--expandable-content-ov-content))
           (new-content (if append-content?
                            (concat (overlay-get ov-content 'eca-chat--expandable-content-content) content)
                          content))
           (new-content (propertize new-content 'line-prefix "   "))
           (open? (overlay-get ov-label 'eca-chat--expandable-content-toggle)))
      (overlay-put ov-content 'eca-chat--expandable-content-content new-content)
      (save-excursion
        (goto-char (overlay-start ov-label))
        (delete-region (point) (1- (overlay-start ov-content)))
        (insert (propertize label
                            'line-prefix (if open?
                                             eca-chat-expandable-block-close-symbol
                                           eca-chat-expandable-block-open-symbol)
                            'help-echo "mouse-1 / RET / tab: expand/collapse"))
        (when open?
          (delete-region (overlay-start ov-content) (overlay-end ov-content))
          (goto-char (overlay-start ov-content))
          (insert new-content))))))

(defun eca-chat--expandable-content-toggle (id &optional force? open?)
  "Toggle the expandable-content of ID.
If FORCE? decide to OPEN? or not."
  (when-let* ((ov-label (-first (-lambda (ov) (string= id (overlay-get ov 'eca-chat--expandable-content-id)))
                                (overlays-in (point-min) (point-max)))))
    (let* ((ov-content (overlay-get ov-label 'eca-chat--expandable-content-ov-content))
           (open? (if force?
                      open?
                    (overlay-get ov-label 'eca-chat--expandable-content-toggle)))
           (content (overlay-get ov-content 'eca-chat--expandable-content-content)))
      (save-excursion
        (goto-char (overlay-start ov-label))
        (if open?
            (progn
              (put-text-property (point) (line-end-position)
                                 'line-prefix eca-chat-expandable-block-open-symbol)
              (goto-char (1+ (line-end-position)))
              (delete-region (overlay-start ov-content) (overlay-end ov-content))
              (overlay-put ov-label 'eca-chat--expandable-content-toggle nil))
          (progn
            (put-text-property (point) (line-end-position)
                               'line-prefix eca-chat-expandable-block-close-symbol)
            (goto-char (overlay-start ov-content))
            (insert content "\n")
            (overlay-put ov-label 'eca-chat--expandable-content-toggle t))))
      open?)))

(defun eca-chat--content-table (key-vals)
  "Return a string in table format for KEY-VALS."
  (-reduce-from
   (-lambda (a (k . v))
     (concat a "\n" k ": \n"
             (if (listp v)
                 (string-join (-map-indexed
                               (lambda (i item)
                                 (if (cl-evenp i)
                                     (propertize (concat (substring (symbol-name item) 1) ": ")
                                                 'font-lock-face 'eca-chat--tool-call-argument-key-face)
                                   (propertize (concat (prin1-to-string item) "\n")
                                               'font-lock-face 'eca-chat--tool-call-argument-value-face)))
                               v)
                              "")
               v)))
   ""
   key-vals))

(defun eca-chat--relativize-filename-for-workspace-root (filename roots)
  "Relativize the FILENAME if a workspace root is found for ROOTS."
  (or (-some->> (-first (lambda (root) (f-ancestor-of? root filename)) roots)
        (f-relative filename))
      filename))

(defun eca-chat--refresh-context ()
  "Refresh chat context."
  (save-excursion
    (-some-> (eca-chat--prompt-context-field-ov)
      (overlay-start)
      (goto-char))
    (delete-region (point) (line-end-position))
    (seq-doseq (context eca-chat--context)
      (-let (((&plist :type type) context))
        (insert
         (pcase type
           ("file" (propertize (concat eca-chat-context-prefix (f-filename (plist-get context :path)))
                               'eca-chat-context-item context
                               'font-lock-face 'eca-chat-context-file-face))
           ("directory" (propertize (concat eca-chat-context-prefix (f-filename (plist-get context :path)))
                                    'eca-chat-context-item context
                                    'font-lock-face 'eca-chat-context-file-face))
           ("repoMap" (propertize (concat eca-chat-context-prefix "repoMap")
                                  'eca-chat-context-item context
                                  'font-lock-face 'eca-chat-context-repo-map-face))
           (_ (propertize (concat eca-chat-context-prefix "unkown:" type)
                          'eca-chat-context-item context))))
        (insert " ")))
    (insert (propertize eca-chat-context-prefix 'font-lock-face 'eca-chat-context-unlinked-face))))

(defconst eca-chat--kind->symbol
  '(("file" . file)
    ("directory" . folder)
    ("repoMap" . module)))

(defun eca-chat--completion-candidate-kind (item)
  "Return the kind for ITEM."
  (alist-get (plist-get (get-text-property 0 'eca-chat-completion-item item) :type)
             eca-chat--kind->symbol
             nil
             nil
             #'string=))

(defun eca-chat--add-context (context)
  "Add to chat CONTEXT."
  (add-to-list 'eca-chat--context context t)
  (eca-chat--refresh-context))

(defun eca-chat--completion-annotate (roots item-label)
  "Annonate ITEM-LABEL detail for ROOTS."
  (-let (((&plist :type type :path path) (get-text-property 0 'eca-chat-completion-item item-label)))
    (pcase type
      ("file" (eca-chat--relativize-filename-for-workspace-root path roots))
      ("directory" (eca-chat--relativize-filename-for-workspace-root path roots))
      ("repoMap" "Summary view of workspaces files")
      (_ ""))))

(defun eca-chat--completion-exit-function (item _status)
  "Add to context the selected ITEM."
  (eca-chat--add-context (get-text-property 0 'eca-chat-completion-item item))
  (end-of-line))

(defun eca-chat--context-to-completion (context)
  "Convert CONTEXT to a completion item."
  (propertize
   (pcase (plist-get context :type)
     ("file" (f-filename (plist-get context :path)))
     ("directory" (f-filename (plist-get context :path)))
     ("repoMap" "repoMap")
     (_ (concat "Unknown - " (plist-get context :type))))
   'eca-chat-completion-item context))

;; Public

(define-derived-mode eca-chat-mode markdown-mode "eca-chat"
  "Major mode for ECA chat sessions.
\\{eca-chat-mode-map}"
  :group 'eca
  (visual-line-mode)
  (hl-line-mode -1)
  (setq-local eca-chat--history '())
  (setq-local eca-chat--history-index -1)

  (make-local-variable 'completion-at-point-functions)
  (setq-local completion-at-point-functions (list #'eca-chat-completion-at-point))
  (when (fboundp 'company-mode)
    (company-mode 1)
    (setq-local company-backends '(company-capf)))

  (let ((session (eca-session)))
    (unless (listp header-line-format)
      (setq-local header-line-format (list header-line-format)))
    (add-to-list 'header-line-format `(t (:eval (eca-chat--header-line-string (eca-session)))))

    (when (eq 0 (length (string-trim (buffer-string))))
      (save-excursion
        (goto-char (point-min))
        (insert "\n")
        (insert (propertize (eca--session-chat-welcome-message session)
                            'font-lock-face 'eca-chat-welcome-face))
        (eca-chat--insert-prompt-string)))

    (run-with-timer
     0.05
     nil
     (lambda ()
       (with-current-buffer (eca-chat--get-buffer (eca-session))
         (display-line-numbers-mode -1)
         (when (fboundp 'vi-tilde-fringe-mode) (vi-tilde-fringe-mode -1))
         (setq-local mode-line-format '(t (:eval (eca-chat--mode-line-string))))
         (force-mode-line-update)))))

  (face-remap-add-relative 'markdown-line-break-face
                           '(:underline nil))

  (goto-char (point-max))
  (run-hooks 'eca-chat-mode-hook))

(defun eca-chat-completion-at-point ()
  "Complete at point in the chat."
  (let ((candidates (lambda ()
                      (cond
                       ((eca-chat--point-at-new-context-p)
                        (-let (((&plist :contexts contexts) (eca-api-request-sync
                                                             (eca-session)
                                                             :method "chat/queryContext"
                                                             :params (list :chatId eca-chat--id
                                                                           :query (thing-at-point 'symbol t)
                                                                           :contexts (vconcat eca-chat--context)))))
                          (-map #'eca-chat--context-to-completion contexts)))
                       (t nil)))))
    (list
     (or (cl-first (bounds-of-thing-at-point 'symbol))
         (point))
     (point)
     (lambda (probe pred action)
       (cond
        ((eq action 'metadata)
         '(metadata (category . eca-capf)
           (display-sort-function . identity)
           (cycle-sort-function . identity)))
        ((eq (car-safe action) 'boundaries) nil)
        (t
         (complete-with-action action (funcall candidates) probe pred))))
     :company-kind #'eca-chat--completion-candidate-kind
     :annotation-function (-partial #'eca-chat--completion-annotate (eca--session-workspace-folders (eca-session)))
     :exit-function #'eca-chat--completion-exit-function)))

(defun eca-chat-content-received (session params)
  "Handle the content received notification with PARAMS for SESSION."
  (let* ((role (plist-get params :role))
         (content (plist-get params :content)))
    (with-current-buffer (eca-chat--get-buffer session)
      (pcase (plist-get content :type)
        ("text" (when-let* ((text (plist-get content :text)))
                  (pcase role
                    ("user" (progn
                              (eca-chat--add-text-content
                               (propertize text
                                           'font-lock-face 'eca-chat-user-messages-face
                                           'line-prefix (propertize eca-chat-prompt-prefix 'font-lock-face 'eca-chat-user-messages-face)
                                           'line-spacing 10))
                              (eca-chat--mark-header)
                              (font-lock-ensure)))
                    ("system" (progn
                                (eca-chat--add-text-content
                                 (propertize text
                                             'line-height 20
                                             'font-lock-face 'eca-chat-system-messages-face))))
                    (_ (eca-chat--add-text-content text)))))
        ("url" (eca-chat--add-header
                (concat
                 "🌐 "
                 (eca-chat--buttonize
                  (plist-get content :title)
                  (lambda() (browse-url (plist-get content :url))))
                 "\n\n")))
        ("reasonStarted" (let ((id (plist-get content :id))
                               (label (propertize "Thinking..." 'font-lock-face 'eca-chat-reason-label-face)))
                           (eca-chat--add-expandable-content id label "")))
        ("reasonText" (let ((text (plist-get content :text))
                            (id (plist-get content :id))
                            (label (propertize "Thinking..." 'font-lock-face 'eca-chat-reason-label-face)))
                        (eca-chat--rename-expandable-content id label text t)))
        ("reasonFinished" (let ((id (plist-get content :id))
                                (label (propertize "Thoughts" 'font-lock-face 'eca-chat-reason-label-face)))
                            (eca-chat--rename-expandable-content id label "" t)))
        ("toolCallPrepare" (let* ((name (plist-get content :name))
                                  (origin (plist-get content :origin))
                                  (argsText (plist-get content :argumentsText))
                                  (colorizedArgsText (concat "```javascript\n" argsText "\n```"))
                                  (id (plist-get content :id))
                                  (label (concat (propertize (format "Preparing %s tool call: "
                                                                     (if (string= "mcp" origin) "MCP" "ECA"))
                                                             'font-lock-face 'eca-chat-mcp-tool-call-label-face)
                                                 (propertize name 'font-lock-face 'eca-chat-mcp-tool-call-label-face)
                                                 " "
                                                 eca-chat-mcp-tool-call-loading-symbol)))
                             (if (eca-chat--get-expandable-content id)
                                 (eca-chat--rename-expandable-content id label colorizedArgsText)
                               (eca-chat--add-expandable-content id label (eca-chat--content-table `(("arguments" . ,colorizedArgsText)))))))
        ("toolCallRun" (let* ((name (plist-get content :name))
                              (origin (plist-get content :origin))
                              (args (plist-get content :arguments))
                              (id (plist-get content :id))
                              (manual? (plist-get content :manualApproval)))
                         (eca-chat--rename-expandable-content
                          id
                          (concat (propertize (format "Calling %s tool: "
                                                      (if (string= "mcp" origin) "MCP" "ECA"))
                                              'font-lock-face 'eca-chat-mcp-tool-call-label-face)
                                  (propertize name 'font-lock-face 'eca-chat-mcp-tool-call-label-face)
                                  " "
                                  eca-chat-mcp-tool-call-loading-symbol
                                  (when manual?
                                    (concat
                                     " "
                                     (eca-chat--buttonize
                                      (propertize "cancel" 'font-lock-face 'eca-chat-tool-call-cancel-face)
                                      (lambda () (eca-api-notify session
                                                                 :method "chat/toolCallReject"
                                                                 :params (list :chatId eca-chat--id :toolCallId id))))
                                     " "
                                     (eca-chat--buttonize
                                      (propertize "run" 'font-lock-face 'eca-chat-tool-call-run-face)
                                      (lambda () (eca-api-notify session
                                                                 :method "chat/toolCallApprove"
                                                                 :params (list :chatId eca-chat--id :toolCallId id)))))))
                          (eca-chat--content-table `(("arguments" . ,args))))))
        ("toolCallRejected" (let* ((name (plist-get content :name))
                                   (origin (plist-get content :origin))
                                   (args (plist-get content :arguments))
                                   (id (plist-get content :id)))
                              (eca-chat--rename-expandable-content
                               id
                               (concat (propertize (format "Rejected %s tool: "
                                                           (if (string= "mcp" origin) "MCP" "ECA"))
                                                   'font-lock-face 'eca-chat-mcp-tool-call-label-face)
                                       (propertize name 'font-lock-face 'eca-chat-mcp-tool-call-label-face)
                                       " "
                                       eca-chat-mcp-tool-call-error-symbol)
                               (eca-chat--content-table `(("arguments" . ,args))))))
        ("toolCalled" (let* ((id (plist-get content :id))
                             (name (plist-get content :name))
                             (origin (plist-get content :origin))
                             (args (plist-get content :arguments))
                             (outputs (append (plist-get content :outputs) nil))
                             (any-error? (-any-p (lambda (output) (plist-get output :error)) outputs))
                             (output-contents (-reduce-from (lambda (txt output) (concat txt "\n" (plist-get output :content)))
                                                            ""
                                                            outputs)))
                        (eca-chat--rename-expandable-content
                         id
                         (concat (propertize (format "Called %s tool: "
                                                     (if (string= "mcp" origin) "MCP" "ECA") )
                                             'font-lock-face 'eca-chat-mcp-tool-call-label-face)
                                 (propertize name 'font-lock-face 'eca-chat-mcp-tool-call-label-face)
                                 " "
                                 (if any-error?
                                     eca-chat-mcp-tool-call-error-symbol
                                   eca-chat-mcp-tool-call-success-symbol))
                         (eca-chat--content-table `(("Arguments" . ,args)
                                                    ("Output" . ,output-contents))))))
        ("progress" (pcase (plist-get content :state)
                      ("running" (progn
                                   (unless eca-chat--spinner-timer
                                     (eca-chat--spinner-start session))
                                   (setq-local eca-chat--progress-text (propertize (plist-get content :text) 'font-lock-face 'eca-chat-system-messages-face))))
                      ("finished" (progn
                                    (eca-chat--spinner-stop)
                                    (eca-chat--add-text-content (propertize "\n" 'line-spacing 10))
                                    (eca-chat--set-chat-loading session nil)
                                    (setq-local eca-chat--progress-text "")))))
        ("usage" (progn
                   (setq-local eca-chat--message-input-tokens (plist-get content :messageInputTokens))
                   (setq-local eca-chat--message-output-tokens (plist-get content :messageOutputTokens))
                   (setq-local eca-chat--session-tokens (plist-get content :sessionTokens))
                   (setq-local eca-chat--message-cost (plist-get content :messageCost))
                   (setq-local eca-chat--session-cost (plist-get content :sessionCost))))))))

(defun eca-chat--handle-mcp-server-updated (session _server)
  "Handle mcp SERVER updated for SESSION."
  (with-current-buffer (eca-chat--get-buffer session)
    (force-mode-line-update)))

(defun eca-chat-open (session)
  "Open or create dedicated eca chat window for SESSION."
  (eca-assert-session-running session)
  (unless (buffer-live-p (eca-chat--get-buffer session))
    (eca-chat--create-buffer session))
  (with-current-buffer (eca-chat--get-buffer session)
    (unless (derived-mode-p 'eca-chat-mode)
      (eca-chat-mode)
      (eca-chat--add-context (list :type "repoMap")))
    (unless (eca--session-chat session)
      (setf (eca--session-chat session) (current-buffer)))
    (if (window-live-p (get-buffer-window (buffer-name)))
        (eca-chat--select-window)
      (eca-chat--pop-window))))

(defun eca-chat-exit (session)
  "Exit the ECA chat for SESSION."
  (when (buffer-live-p (get-buffer (eca-chat-buffer-name session)))
    (with-current-buffer (eca-chat--get-buffer session)
      (setq eca-chat--closed t)
      (force-mode-line-update)
      (goto-char (point-max))
      (rename-buffer (concat (buffer-name) ":closed") t)
      (when-let* ((window (get-buffer-window (eca-chat--get-buffer session))))
        (quit-window nil window)))))

;;;###autoload
(defun eca-chat-clear ()
  "Clear the eca chat."
  (interactive)
  (eca-chat--clear (eca-session)))

;;;###autoload
(defun eca-chat-select-model ()
  "Select which model to use in the chat from what server supports."
  (interactive)
  (eca-assert-session-running (eca-session))
  (when-let* ((model (completing-read "Select a model:" (append (eca--session-models (eca-session)) nil) nil t)))
    (setq eca-chat-custom-model model)))

;;;###autoload
(defun eca-chat-select-behavior ()
  "Select which chat behavior to use from what server supports."
  (interactive)
  (eca-assert-session-running (eca-session))
  (when-let* ((behavior (completing-read "Select a behavior:" (append (eca--session-chat-behaviors (eca-session)) nil) nil t)))
    (setq eca-chat-custom-behavior behavior)))

;;;###autoload
(defun eca-chat-reset ()
  "Request a chat reset."
  (interactive)
  (when eca-chat--id
    (eca-api-request-sync (eca-session)
                          :method "chat/delete"
                          :params (list :chatId eca-chat--id))
    (setq-local eca-chat--message-input-tokens nil)
    (setq-local eca-chat--message-output-tokens nil)
    (setq-local eca-chat--session-tokens nil)
    (setq-local eca-chat--message-cost nil)
    (setq-local eca-chat--session-cost nil)
    (eca-chat--clear (eca-session))))

(declare-function whisper-run "ext:whisper" ())

;;;###autoload
(defun eca-chat-talk ()
  "Talk to the assistent by recording audio and transcribing it."
  (interactive)
  (unless (require 'whisper nil t)
    (user-error "Whisper.el is not available, please install it first"))
  (let ((session (eca-session)))
    (eca-assert-session-running session)
    (eca-chat-open session)
    (with-current-buffer (eca-chat--get-buffer session)
      (goto-char (point-max)))
    (let ((buffer (get-buffer-create "*whisper-stdout*")))
      (with-current-buffer buffer
        (erase-buffer)
        (make-local-variable 'whisper-after-transcription-hook)
        (add-hook 'whisper-after-transcription-hook
                  (lambda ()
                    (let ((transcription (buffer-substring
                                          (line-beginning-position)
                                          (line-end-position))))
                      (with-current-buffer (eca-chat-buffer-name session)
                        (insert transcription)
                        (newline)
                        (eca-chat--key-pressed-return))))
                  nil t)
        (whisper-run)
        (eca-info "Recording audio. Press RET when you are done.")
        (while (not (equal ?\r (read-char)))
          (sit-for 0.5))
        (whisper-run)))))

(provide 'eca-chat)
;;; eca-chat.el ends here
