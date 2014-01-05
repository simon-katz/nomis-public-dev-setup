;;;; nomis-dirtree.el --- Directory tree views
;;;; Based on dirtree.el (see https://github.com/zk/emacs-dirtree).
;;;; This work was started on 2013-12-22.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Suite 500, Boston, MA 02110.

;;;; ___________________________________________________________________________

;;;; TODO:

;;;; - When hitting up and down arrow keys, when you hit directories
;;;;   there are messages saying "Expand" and "Collapse" (depending on
;;;;   whether the directory is collapsed or expanded).

;;;; - Navigate to node in tree from a file.

;;;; - Look into the Tree menu.
;;;;   - Is stuff not as it says?
;;;;   - Can you add to it?  Do you want to?

;;;; - Scan all for badness.
;;;; - Tidy.

;;;; ---------------------------------------------------------------------------

;;;; Probably won't do the following:

;;;; - Could add backward and forward navigation (to previous and next selections).
;;;;   - You have `nomis-dirtree-goto-previous-up-from-position`.
;;;;     Maybe that's good enough.
;;;;     It's broken after collapsing/expanding/refreshing, though.
;;;;   - Using positions would be no good.
;;;;     - A refresh changes things.
;;;;     - Needs to work with collapsing and expanding.
;;;;   - Need to use filenames.
;;;;     But how can you go to a particular widget? Maybe use its :start or :end.




;;;; ___________________________________________________________________________
;;;; Initially we have, more-or-less, the original dirtree.
;;;; I don't have a deep understanding of this, but I've hacked it a bit.

(eval-when-compile
  (require 'cl))
(require 'tree-mode)
(require 'windata)
(require 'dired-x)

(defgroup nomis-dirtree nil
  "Directory tree views"
  :group 'tools)

(defcustom nomis-dirtree-windata '(frame left 0.5 delete)
  "*Arguments to set the window buffer display.
See `windata-display-buffer' for setup the arguments."
  :type 'sexp
  :group 'nomis-dirtree)

(defcustom nomis-dirtree-buffer "*nomis-dirtree*"
  "*Buffer name for `nomis-dirtree'"
  :type 'string
  :group 'nomis-dirtree)

(define-widget 'nomis-dirtree-directory-widget 'tree-widget
  "Directory Tree widget."
  :dynargs        'nomis-dirtree-setup-children
  :has-children   t)

(define-widget 'nomis-dirtree-file-widget-for-file 'push-button
  "File widget."
  :format         "%[%t%]\n"
  :button-face    'default)

(define-widget 'nomis-dirtree-file-widget-for-directory 'push-button
  "File widget."
  :format         "%[%t%]\n"
  :button-face    'default)

(defun nomis-dirtree-directory-widget-p (widget)
  (eql (car widget) 'nomis-dirtree-directory-widget))

(defun nomis-dirtree-file-widget-for-directory-p (widget)
  (eql (car widget) 'nomis-dirtree-file-widget-for-directory))

(defun nomis-dirtree-show ()
  "Show `nomis-dirtree-buffer'. Create tree when no parent directory find."
  (interactive)
  (let ((buffer (get-buffer-create nomis-dirtree-buffer))
        (dir default-directory)
        trees tree button path)
    (with-current-buffer buffer
      (setq trees tree-mode-list)
      (while (and trees
                  (not tree))
        (if (string-match (concat "^" (regexp-quote (widget-get (car trees) :file))) dir)
            ;; if parent directory in buffer
            (setq tree (car trees))
          (setq trees (cdr trees)))))
    (if tree
        (progn
          (setq path (split-string (file-relative-name buffer-file-name (widget-get tree :file)) "/"))
          (nomis-dirtree (widget-get tree :file) t)
          (setq button (tree-mode-find-node tree path))
          (if button
              (goto-char (widget-get (car button) :from))))
      (call-interactively 'nomis-dirtree))))

(defun nomis-dirtree (root select)
  "Create tree of `root' directory.
With prefix argument select `nomis-dirtree-buffer'"
  (interactive "DDirectory: \nP")
  (let ((buffer (get-buffer-create nomis-dirtree-buffer))
        tree win)
    (with-current-buffer buffer
      (unless (eq major-mode 'nomis-dirtree-mode)
        (nomis-dirtree-mode))
      (dolist (atree tree-mode-list)
        (if (string= (widget-get atree :file) root)
            (setq tree atree)))
      (or tree
          (setq tree (tree-mode-insert (nomis-dirtree-root-widget root)))))
    ;; (setq win (get-buffer-window nomis-dirtree-buffer))
    (unless win
      ;;(setq win (get-buffer-window nomis-dirtree-buffer))
      (setq win (apply 'windata-display-buffer nomis-dirtree-buffer nomis-dirtree-windata))
      (select-window win))
    (with-selected-window win
      (unless (widget-get tree :open)
        (widget-apply-action tree))
      (goto-char (widget-get tree :from))
      (recenter 1))
    (if select
        (select-window win))))

(defun nomis-dirtree-in-buffer (root select)
  "Create tree of `root' directory.
With prefix argument select `nomis-dirtree-buffer'"
  (interactive "DDirectory: \nP")
  (let ((buffer (get-buffer-create nomis-dirtree-buffer))
        tree win)
    (with-current-buffer buffer
      (unless (eq major-mode 'nomis-dirtree-mode)
        (nomis-dirtree-mode))
      (dolist (atree tree-mode-list)
        (if (string= (widget-get atree :file) root)
            (setq tree atree)))
      (or tree
          (setq tree (tree-mode-insert (nomis-dirtree-root-widget root)))))
    (if select
        (switch-to-buffer nomis-dirtree-buffer))))

(define-derived-mode nomis-dirtree-mode tree-mode "Dir-Tree"
  "A mode to display tree of directory"
  (tree-widget-set-theme "folder"))

(defun nomis-dirtree-root-widget (directory)
  "create the root directory"
  `(nomis-dirtree-directory-widget
    :node (nomis-dirtree-file-widget-for-directory
           :tag ,directory
           :file ,directory)
    :file ,directory
    :open t
    :nomis-root t))

(defun nomis-dirtree-setup-children (tree)
  "expand directory"
  (or (widget-get tree :args)
      (let ((directory (widget-get tree :file))
            (re (dired-omit-regexp))
            dirs files basename)
        (dolist (file (directory-files directory t))
          (setq basename (file-name-nondirectory file))
          (unless (string-match re basename)
            (if (file-directory-p file)
                (push (cons file basename) dirs)
              (push (cons file basename) files))))
        (setq dirs (sort dirs (lambda (a b) (string< (cdr a) (cdr b)))))
        (setq files (sort files (lambda (a b) (string< (cdr a) (cdr b)))))
        (append
         (mapcar (lambda (file)
                   `(nomis-dirtree-directory-widget
                     :file ,(car file)
                     :node (nomis-dirtree-file-widget-for-directory
                            :tag ,(cdr file)
                            :file ,(car file))))
                 dirs)
         (mapcar (lambda (file)
                   `(nomis-dirtree-file-widget-for-file
                     :file ,(car file)
                     :tag ,(cdr file)))
                 files)))))

;;;; ___________________________________________________________________________
;;;; My stuff.

;;;; ---------------------------------------------------------------------------
;;;; Support for expand/collapse.

(defun nomis-tree-mode-expand (widget)
  (when (tree-widget-p widget)
    (unless (widget-get widget :open)
      (widget-apply-action widget))))

(defun nomis-tree-mode-collapse (widget)
  (when (tree-widget-p widget)
    (when (widget-get widget :open)
      (widget-apply-action widget))))

(defvar *dirs-to-keep-collapsed-unless-forced*
  '(".git"
    "target"))

(defun directory-to-keep-collapsed-p (name)
  (some (lambda (no-expand-name)
          (string-match (concat "/" no-expand-name "$")
                        name))
        *dirs-to-keep-collapsed-unless-forced*))

;;;; ---------------------------------------------------------------------------
;;;; The stack of previous up-from positions.

(defvar *nomis-dirtree-previous-up-from-positions* '())

(defun nomis-dirtree-note-previous-up-from-position ()
  (push (point) *nomis-dirtree-previous-up-from-positions*))

(defun no-previous-up-from-position-p? ()
  (null *nomis-dirtree-previous-up-from-positions*))

(defun pop-to-previous-up-from-position ()
  (assert (not (no-previous-up-from-position-p?)))
  (goto-char (first *nomis-dirtree-previous-up-from-positions*))
  (pop *nomis-dirtree-previous-up-from-positions*))

;;;; ---------------------------------------------------------------------------
;;;; Helper functions to do with the selection.

(defun nomis-dirtree-selected-widget ()
  (let* ((widget (widget-at (1- (line-end-position)))))
    (if (nomis-dirtree-file-widget-for-directory-p widget)
        (widget-get widget :parent)
      widget)))

(defun nomis-dirtree-selected-file-or-dir ()
  (let* ((widget (nomis-dirtree-selected-widget))
         (file (widget-get widget :file)))
    file))

;;;; ---------------------------------------------------------------------------
;;;; User-visible commands.

(defun nomis-dirtree-display-file ()
  "Display contents of file under point in other window."
  (interactive)
  (save-selected-window
    (let* ((file (nomis-dirtree-selected-file-or-dir)))
      (when file
        (find-file-other-window file)))))

(defun nomis-dirtree-open-in-default-app ()
  "Open file under point using its default app."
  (interactive)
  (let* ((file (nomis-dirtree-selected-file-or-dir)))
    (when file
      (shell-command (concat "open \"" file "\"")))))

(defun nomis-dirtree-previous-line (arg)
  "Move up <arg> lines."
  (interactive "p")
  (tree-mode-previous-node arg))

(defun nomis-dirtree-previous-line-and-display (arg)
  "Move up <arg> lines.
Then display contents of file under point in other window."
  (interactive "p")
  (nomis-dirtree-previous-line arg)
  (nomis-dirtree-display-file))

(defun nomis-dirtree-next-line (arg)
  "Move down <arg> lines."
  (interactive "p")
  (tree-mode-next-node arg))

(defun nomis-dirtree-next-line-and-display (arg)
  "Move down <arg> lines.
Then display contents of file under point in other window."
  (interactive "p")
  (nomis-dirtree-next-line arg)
  (nomis-dirtree-display-file))

(defun nomis-dirtree-up-directory (arg)
  "Move to parent directory. Repeat <arg> times if <arg> supplied.
Before doing this, push the current line onto a stack of previous up-from
positions."
  (interactive "p")
  (if (plist-get (rest (nomis-dirtree-selected-widget))
                 :nomis-root)
      (progn
        (message "Already at root.")
        (beep))
    (progn
      (nomis-dirtree-note-previous-up-from-position)
      (tree-mode-goto-parent arg))))

(defun nomis-dirtree-up-directory-and-display (arg)
  "Move to parent directory. Repeat <arg> times if <arg> supplied.
Before doing this, push the current line onto a stack of previous up-from
positions.
Then display contents of file under point in other window."
  (interactive "p")
  (nomis-dirtree-up-directory arg)
  (nomis-dirtree-display-file))

(defun nomis-dirtree-next-line-with-expansion (arg)
  "Move down <arg> lines, expanding any encountered collapsed directories
and showing previous expansion of subdirectories."
  (interactive "p")
  (unless (< arg 1)
    (let* ((widget (nomis-dirtree-selected-widget)))
      (when (nomis-dirtree-directory-widget-p widget)
        (nomis-tree-mode-expand widget))
      (nomis-dirtree-next-line 1))
    (nomis-dirtree-next-line-with-expansion (1- arg))))

(defun nomis-dirtree-next-line-with-expansion-and-display (arg)
  "Move down <arg> lines, expanding any encountered collapsed directories
and showing previous expansion of subdirectories.
Then display contents of file under point in other window."
  (interactive "p")
  (nomis-dirtree-next-line-with-expansion arg)
  (nomis-dirtree-display-file))

(defun nomis-dirtree-expand (arg)
  "Expand directory under point, showing previous expansion of subdirectories.
If <arg> is supplied, first collapse all and then expand to <arg> levels."
  (interactive "P")
  (labels ((expand-recursively
            (widget n-times &optional force-expand-p)
            (when (and (nomis-dirtree-directory-widget-p widget)
                       (>= n-times 1)
                       (or force-expand-p
                           (not (directory-to-keep-collapsed-p (widget-get widget :file)))))
              (nomis-tree-mode-expand widget)
              (mapc (lambda (x) (expand-recursively x (1- n-times)))
                    (widget-get widget :children)))))
    (let* ((widget (nomis-dirtree-selected-widget)))
      (if (nomis-dirtree-directory-widget-p widget)
          (progn
            (unless (null arg)
              (nomis-dirtree-collapse-all))
            (expand-recursively widget
                                (or arg 1)
                                t))
        (beep)))))

(defun nomis-dirtree-collapse ()
  "Collapse directory under point, retaining previous expansion of subdirectories."
  (interactive)
  (let* ((widget (nomis-dirtree-selected-widget)))
    (if (nomis-dirtree-directory-widget-p widget)
        (when (widget-get widget :open)
          (nomis-tree-mode-collapse widget))
      (beep))))

(defun nomis-dirtree-expand-all ()
  "Expand directory under point to show all subdirectories,
sub-subdirectories, etc."
  (interactive)
  (nomis-dirtree-expand 1000000))

(defun nomis-dirtree-collapse-all ()
  "Collapse directory under point and all of its subdirectories,
sub-subdirectories, etc, so that subsequent expansion shows only one level."
  (interactive)
  (labels ((collapse-recursively (widget)
                                 (mapc 'collapse-recursively
                                       (widget-get widget :children))
                                 (nomis-tree-mode-collapse widget)))
    (let* ((widget (nomis-dirtree-selected-widget)))
      (if (nomis-dirtree-directory-widget-p widget)
          (collapse-recursively widget)
        (beep)))))

(defun nomis-dirtree-goto-previous-up-from-position ()
  "Return to the line at the front of the stack of previous up-from
positions, popping the stack."
  (interactive)
  (if (no-previous-up-from-position-p?)
      (progn
        (beep)
        (message "There is no previous up-from position."))
    (pop-to-previous-up-from-position)))

(defun nomis-dirtree-goto-previous-up-from-position-and-display ()
  "Return to the line at the front of the stack of previous up-from
positions, popping the stack.
Then display contents of file under point in other window."
  (interactive)
  (nomis-dirtree-goto-previous-up-from-position)
  (nomis-dirtree-display-file))

(defun nomis-dirtree-show-selection-info ()
  "Display some details of the file under point in a message dialog.
Mostly for debugging purposes."
  (interactive)
  (let* ((widget (nomis-dirtree-selected-widget))
         (file (widget-get widget :file)))
    (message-box "                              (car widget) = %s
                              file = %s
                              (line-end-position) = %s
                              from = %s
                              to = %s
                              widget keys = %s"
                 (car widget)
                 file
                 (line-end-position)
                 (plist-get (rest widget) :from)
                 (plist-get (rest widget) :to)
                 (loop for k in (rest widget) by 'cddr
                       collect k))))

(labels ((dk (k f)
             (define-key nomis-dirtree-mode-map k f)))

  (define-key widget-keymap (kbd "<RET>") nil)
  (dk (kbd "<RET>")       'nomis-dirtree-display-file)
  (dk (kbd "C-<return>")  'nomis-dirtree-display-file)

  (dk (kbd "<up>")        'nomis-dirtree-previous-line)
  (dk (kbd "C-<up>")      'nomis-dirtree-previous-line-and-display)
  (dk (kbd "<down>")      'nomis-dirtree-next-line)
  (dk (kbd "C-<down>")    'nomis-dirtree-next-line-and-display)
  (dk (kbd "<left>")      'nomis-dirtree-up-directory)
  (dk (kbd "C-<left>")    'nomis-dirtree-up-directory-and-display)
  (dk (kbd "<right>")     'nomis-dirtree-next-line-with-expansion)
  (dk (kbd "C-<right>")   'nomis-dirtree-next-line-with-expansion-and-display)

  (dk (kbd "M-<right>")   'nomis-dirtree-expand)
  (dk (kbd "M-<left>")    'nomis-dirtree-collapse)
  (dk (kbd "M-S-<right>") 'nomis-dirtree-expand-all)
  (dk (kbd "M-S-<left>")  'nomis-dirtree-collapse-all)

  (dk (kbd "M-[")         'nomis-dirtree-goto-previous-up-from-position)
  (dk (kbd "C-M-[")       'nomis-dirtree-goto-previous-up-from-position-and-display)
  
  (dk (kbd "M-o")         'nomis-dirtree-open-in-default-app)
  (dk (kbd "M-d")         'nomis-dirtree-show-selection-info))

(provide 'nomis-dirtree)
