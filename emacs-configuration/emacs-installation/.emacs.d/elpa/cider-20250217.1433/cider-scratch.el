;;; cider-scratch.el --- *scratch* buffer for Clojure -*- lexical-binding: t -*-

;; Copyright © 2014-2025 Bozhidar Batsov and CIDER contributors
;;
;; Author: Tim King <kingtim@gmail.com>
;;         Phil Hagelberg <technomancy@gmail.com>
;;         Bozhidar Batsov <bozhidar@batsov.dev>
;;         Artur Malabarba <bruce.connor.am@gmail.com>
;;         Hugo Duncan <hugo@hugoduncan.org>
;;         Steve Purcell <steve@sanityinc.com>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Imitate Emacs's *scratch* buffer.

;;; Code:

(require 'cider-eval)
(require 'clojure-mode)
(require 'easymenu)

(defcustom cider-scratch-initial-message
  ";; This buffer is for Clojure experiments and evaluation.\n
;; Press C-j to evaluate the last expression.\n
;; You can also press C-u C-j to evaluate the expression and pretty-print its result.\n\n"
  "The initial message displayed in new scratch buffers."
  :type 'string
  :group 'cider
  :package-version '(cider . "0.18.0"))

(defvar cider-clojure-interaction-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map clojure-mode-map)
    (define-key map (kbd "C-j") #'cider-eval-print-last-sexp)
    (define-key map [remap paredit-newline] #'cider-eval-print-last-sexp)
    (define-key map [remap paredit-C-j] #'cider-eval-print-last-sexp)
    (easy-menu-define cider-clojure-interaction-mode-menu map
      "Menu for Clojure Interaction mode"
      '("Clojure Interaction"
        (["Eval and print last sexp" #'cider-eval-print-last-sexp]
         "--"
         ["Reset" #'cider-scratch-reset])))
    map))

(defconst cider-scratch-buffer-name "*cider-scratch*")

;;;###autoload
(defun cider-scratch ()
  "Go to the scratch buffer named `cider-scratch-buffer-name'."
  (interactive)
  (pop-to-buffer (cider-scratch-find-or-create-buffer)))

(defun cider-scratch-find-or-create-buffer ()
  "Find or create the scratch buffer."
  (or (get-buffer cider-scratch-buffer-name)
      (cider-scratch--create-buffer)))

(define-derived-mode cider-clojure-interaction-mode clojure-mode "Clojure Interaction"
  "Major mode for typing and evaluating Clojure forms.
Like `clojure-mode' except that \\[cider-eval-print-last-sexp] evals the Lisp expression
before point, and prints its value into the buffer, advancing point.

\\{cider-clojure-interaction-mode-map}"
  (setq-local sesman-system 'CIDER))

(defun cider-scratch--insert-welcome-message ()
  "Insert the welcome message for the scratch buffer."
  (insert cider-scratch-initial-message))

(defun cider-scratch--create-buffer ()
  "Create a new scratch buffer."
  (with-current-buffer (get-buffer-create cider-scratch-buffer-name)
    (cider-clojure-interaction-mode)
    (cider-scratch--insert-welcome-message)
    (current-buffer)))

(defun cider-scratch-reset ()
  "Reset the current scratch buffer."
  (interactive)
  (erase-buffer)
  (cider-scratch--insert-welcome-message))

(provide 'cider-scratch)

;;; cider-scratch.el ends here
