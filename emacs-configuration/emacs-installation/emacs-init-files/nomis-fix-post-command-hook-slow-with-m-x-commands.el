;;; nomis-fix-post-command-hook-slow-with-m-x-commands.el --- Fix for post-sommand hooks being slow with M-x commands      -*- lexical-binding: t; -*-

(cond
 ;; See https://debbugs.gnu.org/cgi/bugreport.cgi?bug=50042
 ;; See https://github.com/emacs-lsp/lsp-ui/issues/647
 ;; Without this, various commands take ages to refresh the UI:
 ;; - `M-x nomis/lsp-toggle-lsp-ui-sideline-show-code-actions`
 ;; - `M-x flycheck-previous-error`
 ;; - `M-x flycheck-next-error`
 ((member emacs-version
          '("26.3"
            "27.1"
            "27.2"))
  ;; The original `execute-extended-command` is defined in `simple`.
  ;; This fix comes from Emacs commit 00a9c50ad7.

  (defvar execute-extended-command--binding-timer nil)

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
                         (where-is-internal function overriding-local-map t)))
           (delay-before-suggest 0)
           (find-shorter nil))
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
      ;; Ensure that we never have two of the suggest-binding timers in
      ;; flight.
      (when execute-extended-command--binding-timer
        (cancel-timer execute-extended-command--binding-timer))
      ;; If this command displayed something in the echo area; then
      ;; postpone display our suggestion message a bit.
      (when (and suggest-key-bindings
                 (or binding
                     (and extended-command-suggest-shorter typed)))
        (setq delay-before-suggest
              (cond
               ((zerop (length (current-message))) 0)
               ((numberp suggest-key-bindings) suggest-key-bindings)
               (t 2)))
        (when (and extended-command-suggest-shorter
                   (not binding)
                   (not executing-kbd-macro)
                   (symbolp function)
                   (> (length (symbol-name function)) 2))
          ;; There's no binding for CMD.  Let's try and find the shortest
          ;; string to use in M-x.
          (setq find-shorter t))
        (when (or binding find-shorter)
          (setq execute-extended-command--binding-timer
                (run-at-time
                 delay-before-suggest nil
                 (lambda ()
                   ;; If the user has typed any other commands in the
                   ;; meantime, then don't display anything.
                   (when (eq function real-last-command)
                     ;; Find shorter string.
                     (when find-shorter
                       (while-no-input
                         ;; FIXME: Can be slow.  Cache it maybe?
                         (setq binding (execute-extended-command--shorter
                                        (symbol-name function) typed))))
                     (when binding
                       (with-temp-message
                           (format-message "You can run the command `%s' with %s"
                                           function
                                           (if (stringp binding)
                                               (concat "M-x " binding " RET")
                                             (key-description binding)))
                         (sit-for (if (numberp suggest-key-bindings)
                                      suggest-key-bindings
                                    2)))))))))))))

 ((version<= "28.1" emacs-version)
  ;; This is fixed.
  )

 (t
  (message-box
   "You need to fix `execute-extended-command` for this version of Emacs.")))

(provide 'nomis-fix-post-command-hook-slow-with-m-x-commands)
