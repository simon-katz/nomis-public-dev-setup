;;; nomis-fix-post-command-hook-slow-with-m-x-commands.el --- Fix for post-sommand hooks being slow with M-x commands      -*- lexical-binding: t; -*-

(cond
 ;; See https://debbugs.gnu.org/cgi/bugreport.cgi?bug=50042
 ;; See https://github.com/emacs-lsp/lsp-ui/issues/647
 ;; Without this, various commands take ages to refresh the UI:
 ;; - `M-x nomis/lsp-toggle-lsp-ui-sideline-show-code-actions`
 ;; - `M-x flycheck-previous-error`
 ;; - `M-x flycheck-next-error`
 ((member emacs-version
          '("26.3"))
  ;; The original `execute-extended-command` is defined in `simple`.
  (defun execute-extended-command (prefixarg &optional command-name typed)
    ;; Based on Fexecute_extended_command in keyboard.c of Emacs.
    ;; Aaron S. Hawley <aaron.s.hawley(at)gmail.com> 2009-08-24
    "Read a command name, then read the arguments and call the command.
To pass a prefix argument to the command you are
invoking, give a prefix argument to `execute-extended-command'."
    (declare (interactive-only command-execute))
    ;; FIXME: Remember the actual text typed by the user before completion,
    ;; so that we don't later on suggest the same shortening.
    (interactive
     (let ((execute-extended-command--last-typed nil))
       (list current-prefix-arg
             (read-extended-command)
             execute-extended-command--last-typed)))
    ;; Emacs<24 calling-convention was with a single `prefixarg' argument.
    (unless command-name
      (let ((current-prefix-arg prefixarg) ; for prompt
            (execute-extended-command--last-typed nil))
        (setq command-name (read-extended-command))
        (setq typed execute-extended-command--last-typed)))
    (let* ((function (and (stringp command-name) (intern-soft command-name)))
           (binding (and suggest-key-bindings
                         (not executing-kbd-macro)
                         (where-is-internal function overriding-local-map t))))
      (unless (commandp function)
        (error "`%s' is not a valid command name" command-name))
      ;; Some features, such as novice.el, rely on this-command-keys
      ;; including M-x COMMAND-NAME RET.
      (set--this-command-keys (concat "\M-x" (symbol-name function) "\r"))
      (setq this-command function)
      ;; Normally `real-this-command' should never be changed, but here we really
      ;; want to pretend that M-x <cmd> RET is nothing more than a "key
      ;; binding" for <cmd>, so the command the user really wanted to run is
      ;; `function' and not `execute-extended-command'.  The difference is
      ;; visible in cases such as M-x <cmd> RET and then C-x z (bug#11506).
      (setq real-this-command function)
      (let ((prefix-arg prefixarg))
        (command-execute function 'record))
      ;; If enabled, show which key runs this command.
      ;; But first wait, and skip the message if there is input.
      (let* ((waited
              ;; If this command displayed something in the echo area;
              ;; wait a few seconds, then display our suggestion message.
              ;; FIXME: Wait *after* running post-command-hook!
              ;; FIXME: Don't wait if execute-extended-command--shorter won't
              ;; find a better answer anyway!
              (when suggest-key-bindings
                (sit-for (cond
                          ((zerop (length (current-message))) 0)
                          ((numberp suggest-key-bindings) suggest-key-bindings)
                          (t 2))))))
        (when (and waited (not (consp unread-command-events)))
          (unless (or (not extended-command-suggest-shorter)
                      binding executing-kbd-macro (not (symbolp function))
                      (<= (length (symbol-name function)) 2))
            ;; There's no binding for CMD.  Let's try and find the shortest
            ;; string to use in M-x.
            ;; FIXME: Can be slow.  Cache it maybe?
            (while-no-input
              (setq binding (execute-extended-command--shorter
                             (symbol-name function) typed))))
          (when binding
            ;; nomis-hack:
            ;; - See https://debbugs.gnu.org/cgi/bugreport.cgi?bug=50042
            ;; - Introducing `run-at-time` as a hack.
            (run-at-time
             ;; Using `0` as the first arg works in simple cases, but not
             ;; when using `lsp-ui` sidebars for diagnostics.
             0.2
             nil
             (lambda ()
               (with-temp-message
                   (format-message "You can run the command `%s' with %s"
                                   function
                                   (if (stringp binding)
                                       (concat "M-x " binding " RET")
                                     (key-description binding)))
                 (sit-for (if (numberp suggest-key-bindings)
                              suggest-key-bindings
                            2)))))))))))
 (t
  (message-box
   "You need to fix `execute-extended-command` for this version of Emacs.")))

(provide 'nomis-fix-post-command-hook-slow-with-m-x-commands)
