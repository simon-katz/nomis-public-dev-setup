;;;; Init stuff -- nomis-ielm-persistent-history  -*- lexical-binding: t; -*-

;;;; ___________________________________________________________________________
;;;; Persistent ielm history

;;;; A first attempt.
;;;;
;;;; Doesn't work well when multiple Emacs instances exist -- they will blat
;;;; each other's work.

;;;; Inspired by https://emacs.stackexchange.com/a/4226/8584

;;;; Copy the buffer-local value of `comint-input-ring` to a global and use
;;;; `savehist` to make it persistent.
;;;;
;;;; Q. Does `savehist` only work with globals?

;;;; Maybe look to a solution like you have for CIDER REPLs, where you save
;;;; whenever a new command is entered.

;;;; There might be better ideas at https://oleksandrmanzyuk.wordpress.com/2011/10/23/a-persistent-command-history-in-emacs/

(require 'ielm)
(require 'nomis-savehist)

(setq savehist-autosave-interval 60)

(defvar nomis/ielm/-copy-of-comint-input-ring nil)

(add-to-list 'savehist-additional-variables
             'nomis/ielm/-copy-of-comint-input-ring)

(defvar nomis/ielm/-ielm-buffer nil)

(defun nomis/ielm/-save-history ()
  (message "Saving ielm history")
  (setq nomis/ielm/-copy-of-comint-input-ring comint-input-ring)
  (savehist-save))

(defun nomis/ielm/-restore-history ()
  (message "Restoring ielm history")
  (when nomis/ielm/-copy-of-comint-input-ring
    (message "Restoring comint-input-ring...")
    (setq comint-input-ring nomis/ielm/-copy-of-comint-input-ring)))

(defun nomis/ielm/-buffer-killed-stuff ()
  (nomis/ielm/-save-history)
  (setq nomis/ielm/-ielm-buffer nil))

(defun nomis/ielm/-emacs-killed-stuff ()
  (when nomis/ielm/-ielm-buffer
    (with-current-buffer nomis/ielm/-ielm-buffer
      (nomis/ielm/-buffer-killed-stuff))))

(defun nomis/ielm/-set-up-persistent-history ()
  (nomis/ielm/-restore-history)
  (setq nomis/ielm/-ielm-buffer (current-buffer))
  (add-hook 'kill-buffer-hook #'nomis/ielm/-buffer-killed-stuff nil t)
  (add-hook 'kill-emacs-hook  #'nomis/ielm/-emacs-killed-stuff nil t))

(add-hook 'inferior-emacs-lisp-mode-hook
          #'nomis/ielm/-set-up-persistent-history)

;;;; ___________________________________________________________________________

(provide 'nomis-ielm-persistent-history)
