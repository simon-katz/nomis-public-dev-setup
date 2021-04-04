;;; nomis-scrolling.el --- Scrolling hacks  -*- lexical-binding: t -*-

;; Copyright (C) 2019-2021 Simon Katz.

;; Author: Simon Katz
;; Version: 0.0.1-SNAPSHOT
;; TODO: Add a contact details (or, probably, a GitHub URL).
;; TODO: Add Keywords
;; TODO: Add Package-Requires

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; TODO: When we make a repo for this, include the GNU General Public License
;;       mentioned above.

;;; Commentary:

;; Scrolling hacks.

;; TODO: Write webby documentation and add a URL here.

;;; Code:

;;;; ___________________________________________________________________________

(defvar nomis/scrolling/-debug? nil)

(defun nomis/scrolling/-debug (format-string &rest format-args)
  (when nomis/scrolling/-debug?
    (apply #'message format-string format-args)))

;;;; ___________________________________________________________________________

(defvar nomis/scrolling/maintain-line-no-in-window? nil)

(defun nomis/scrolling/toggle-maintain-line-no-in-window ()
  (interactive)
  (message "nomis/scrolling/maintain-line-no-in-window? = %s"
           (setq nomis/scrolling/maintain-line-no-in-window?
                 (not nomis/scrolling/maintain-line-no-in-window?))))

(defun nomis/scrolling/-line-no-in-window ()
  (if (= (point) (window-start))
      ;; Bug in `count-screen-lines`? It's returning 0.
      1
    (count-screen-lines (window-start) (point) t)))

(defun nomis/scrolling/-with-maybe-maintain-line-no-in-window* (fun)
  (cl-flet* ((do-it () (funcall fun)))
    (if (not nomis/scrolling/maintain-line-no-in-window?)
        (do-it)
      (let* ((old-line-no (nomis/scrolling/-line-no-in-window)))
        (prog1 (do-it)
          ;; Ensure cursor is on screen, so that scrolling doesn't make
          ;; any unwanted adjustments.
          (let* ((recenter-redisplay nil))
            (recenter))
          ;; Reset scroll position
          (ignore-errors
            ;; `ignore-errors` because if we're near the top of the buffer
            ;; we may not be able to do this.
            (scroll-up-line (- (nomis/scrolling/-line-no-in-window)
                               old-line-no))))))))

(cl-defmacro nomis/with-maybe-maintain-line-no-in-window (&body body)
  (declare (indent 0))
  `(nomis/scrolling/-with-maybe-maintain-line-no-in-window* (lambda () ,@body)))

;;;; ___________________________________________________________________________

(provide 'nomis-scrolling)
;;; nomis-scrolling.el ends here
