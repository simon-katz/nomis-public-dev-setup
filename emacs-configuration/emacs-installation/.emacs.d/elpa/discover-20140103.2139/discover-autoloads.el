;;; discover-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:

(add-to-list 'load-path (or (and load-file-name (directory-file-name (file-name-directory load-file-name))) (car load-path)))



;;; Generated autoloads from discover.el

(autoload 'discover-show-context-menu "discover" "\
Shows a context menu GROUP-NAME

(fn GROUP-NAME)")
(autoload 'discover-get-context-symbol "discover" "\
Macro that returns the context menu symbol for GROUP-NAME

(fn GROUP-NAME)" nil t)
(autoload 'discover-add-context-menu "discover" "\
Save a context menu to Discover and bind it to the correct keys.


Example 1. Enable Discover in a mode:

    (discover-add-context-menu
       :context-menu (mygroup ... )
       :mode 'dired-mode
       :mode-hook 'dired-mode-hook
       :bind \"?\")

This will bind a function named `dired-mode-turn-on-mygroup' to
the hook `dired-mode-hook' specified in :mode-hook. The name for
the function is `<foo>-turn-on-discover' where `<foo>' is the
`car' symbol in :context-menu - better known as the name of the
context menu.

The function will call `local-set-key' with the binding given
in :bind.


Example 2. Globalized Discover Support:

    (discover-add-context-menu
       :context-menu (mygroup ...)
       :bind \"C-x r\")

As above, this will bind a function but this one is called
`discover--turn-on-mygroup' and is set when `discover-mode' is
set. This enables you to create \"global\" keybindings (that
nevertheless only take effect when `discover-mode' or
`global-discover-mode' is enabled) instead of local
ones. Omitting :mode and :mode-hook is all it takes.

PList Definitions:

:context-menu is a menu definition. See `discover-context-menus'.

:mode is a major mode symbol where the key in :bind take
effect. If major mode is `nil' then the key is defined against
`discover-mode' and is thus in effect when `discover-mode' is
enabled.

:mode-hook is the name of the mode hook where the context menu
key gets bound. Usually it's `<name>-mode-hook'. This property is
redundant if :mode is nil.

:bind is a string, to be passed to `kbd', that the context menu
will be bound to.

Notes:

You can only bind one menu per call to discover. The bound name
given to the key group is taken from the `car' in the list passed
to :context-menu. You can retrieve the command symbol for the
context menu by calling `discover-get-context-menu-command-name'
with the symbol name of the context menu..

(fn &rest PROPERTIES)")
(autoload 'discover-mode "discover" "\
Helps you discover Emacs with interactive context menus.

Key bindings:
\\{discover-map}

This is a minor mode.  If called interactively, toggle the
`Discover mode' mode.  If the prefix argument is positive, enable
the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `discover-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t)
(put 'global-discover-mode 'globalized-minor-mode t)
(defvar global-discover-mode nil "\
Non-nil if Global Discover mode is enabled.
See the `global-discover-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-discover-mode'.")
(custom-autoload 'global-discover-mode "discover" nil)
(autoload 'global-discover-mode "discover" "\
Toggle Discover mode in all buffers.
With prefix ARG, enable Global Discover mode if ARG is positive;
otherwise, disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Discover mode is enabled in all buffers where `discover-mode-turn-on'
would do it.

See `discover-mode' for more information on Discover mode.

(fn &optional ARG)" t)
(register-definition-prefixes "discover" '("discover-"))

;;; End of scraped data

(provide 'discover-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; discover-autoloads.el ends here
