;;; ob-screen.el --- Babel Support for Interactive Terminal -*- lexical-binding: t; -*-

;; Copyright (C) 2009-2025 Free Software Foundation, Inc.

;; Author: Benjamin Andresen
;; Maintainer: Ken Mankoff <mankoff@gmail.com>
;; Keywords: literate programming, interactive shell
;; URL: https://orgmode.org

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Org-Babel support for interactive terminals.  Mostly shell scripts.
;; Heavily inspired by 'eev' from Eduardo Ochs
;;
;; Adding :cmd and :terminal as header arguments
;; :terminal must support the -T (title) and -e (command) parameter
;;
;; You can test the default setup (xterm + sh) with
;; M-x org-babel-screen-test RET

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'ob)

(defvar org-babel-screen-location "screen"
  "The command location for screen.
In case you want to use a different screen than one selected by your $PATH")

(defvar org-babel-default-header-args:screen
  `((:results . "silent") (:session . "default") (:cmd . "sh")
    (:terminal . "xterm") (:screenrc . ,null-device))
  "Default arguments to use when running screen source blocks.")

(defun org-babel-execute:screen (body params)
  "Send BODY via screen to a terminal using Babel, according to PARAMS.
\"default\" session is used when none is specified in the PARAMS."
  (unless noninteractive (message "Sending source code block to interactive terminal session..."))
  (save-window-excursion
    (let* ((session (cdr (assq :session params)))
           (socket (org-babel-screen-session-socketname session)))
      (unless socket (org-babel-prep-session:screen session params))
      (org-babel-screen-session-execute-string
       session (org-babel-expand-body:generic body params)))))

(defun org-babel-prep-session:screen (_session params)
  "Prepare SESSION according to the header arguments specified in PARAMS."
  (let* ((session (cdr (assq :session params)))
         (cmd (cdr (assq :cmd params)))
         (terminal (cdr (assq :terminal params)))
         (screenrc (cdr (assq :screenrc params)))
         (process-name (concat "org-babel: terminal (" session ")")))
    (apply 'start-process process-name "*Messages*"
           terminal `("-T" ,(concat "org-babel: " session) "-e" ,org-babel-screen-location
		      "-c" ,screenrc "-mS" ,session ,cmd))
    ;; XXX: Is there a better way than the following?
    (while (not (org-babel-screen-session-socketname session))
      ;; wait until screen session is available before returning
      )))

;; helper functions

(defun org-babel-screen-session-execute-string (session body)
  "If SESSION exists, send BODY to it."
  (let ((socket (org-babel-screen-session-socketname session)))
    (when socket
      (let ((tmpfile (org-babel-screen-session-write-temp-file session body)))
        (apply 'start-process (concat "org-babel: screen (" session ")") "*Messages*"
               org-babel-screen-location
               `("-S" ,socket "-X" "eval" "msgwait 0"
		 ,(concat "readreg z " tmpfile)
		 "paste z"))))))

(defun org-babel-screen-session-socketname (session)
  "Check if SESSION exists by parsing output of \"screen -ls\"."
  (let* ((screen-ls (shell-command-to-string "screen -ls"))
         (sockets (delq
		   nil
                   (mapcar
		    (lambda (x)
		      (when (string-match (rx (or "(Attached)" "(Detached)")) x)
			x))
		    (split-string screen-ls "\n"))))
         (match-socket (car
			(delq
			 nil
			 (mapcar
			  (lambda (x)
			    (and (string-match-p (regexp-quote session) x)
				 x))
			  sockets)))))
    (when match-socket (car (split-string match-socket)))))

(defun org-babel-screen-session-write-temp-file (_session body)
  "Save BODY in a temp file that is named after SESSION."
  (let ((tmpfile (org-babel-temp-file "screen-")))
    (with-temp-file tmpfile
      (insert body)
      (insert "\n")

      ;; org-babel has superfluous spaces
      (goto-char (point-min))
      (delete-matching-lines "^ +$"))
    tmpfile))

(defun org-babel-screen-test ()
  "Test if the default setup works.
The terminal should shortly flicker."
  (interactive)
  (let* ((random-string (format "%s" (random 99999)))
         (tmpfile (org-babel-temp-file "ob-screen-test-"))
         (body (concat "echo '" random-string "' > " tmpfile "\nexit\n"))
         tmp-string)
    (org-babel-execute:screen body org-babel-default-header-args:screen)
    ;; XXX: need to find a better way to do the following
    (while (not (file-readable-p tmpfile))
      ;; do something, otherwise this will be optimized away
      (message "org-babel-screen: File not readable yet."))
    (setq tmp-string (with-temp-buffer
                       (insert-file-contents-literally tmpfile)
                       (buffer-substring (point-min) (point-max))))
    (delete-file tmpfile)
    (message (concat "org-babel-screen: Setup "
                     (if (string-match random-string tmp-string)
                         "WORKS."
		       "DOESN'T work.")))))

(provide 'ob-screen)

;;; ob-screen.el ends here
