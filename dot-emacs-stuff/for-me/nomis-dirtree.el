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

(eval-when-compile
  (require 'cl))
(require 'tree-mode)
(require 'windata)
(require 'dired-x)

(defgroup nomis-dirtree nil
  "Directory tree views"
  :group 'tools)

(defcustom nomis-dirtree-windata '(frame left 0.3 delete)
  "*Arguments to set the window buffer display.
See `windata-display-buffer' for setup the arguments."
  :type 'sexp
  :group 'nomis-dirtree)

(defcustom nomis-dirtree-buffer "*nomis-dirtree*"
  "*Buffer name for `nomis-dirtree'"
  :type 'string
  :group 'nomis-dirtree)

(define-widget 'nomis-dirtree-dir-widget 'tree-widget
  "Directory Tree widget."
  :dynargs        'nomis-dirtree-expand
  :has-children   t)

(define-widget 'nomis-dirtree-file-widget 'push-button
  "File widget."
  :format         "%[%t%]\n"
  :button-face    'default
  :notify         'nomis-dirtree-display-file-using-node)

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
  "create tree of `root' directory
With prefix arguement select `nomis-dirtree-buffer'"
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
  "create tree of `root' directory
With prefix arguement select `nomis-dirtree-buffer'"
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
  `(nomis-dirtree-dir-widget
    :node (nomis-dirtree-file-widget
           :tag ,directory
           :file ,directory)
    :file ,directory
    :open t))

(defun nomis-dirtree-expand (tree)
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
                   `(nomis-dirtree-dir-widget
                     :file ,(car file)
                     :node (nomis-dirtree-file-widget
                            :tag ,(cdr file)
                            :file ,(car file))))
                 dirs)
         (mapcar (lambda (file)
                   `(nomis-dirtree-file-widget
                     :file ,(car file)
                     :tag ,(cdr file)))
                 files)))))

(defun nomis-dirtree-display-file-using-node (node &rest ignore)
  "Open file in other window"
  (let ((window (selected-window))
        (file (widget-get node :file)))
    (when file
      (find-file-other-window file)
      (select-window window))))

(defun nomis-dirtree-selected-file-or-dir ()
  (let* ((widget (widget-at (1- (line-end-position))))
         (file (widget-get widget :file)))
    file))

(defun nomis-dirtree-display-file ()
  "Open file under point"
  (interactive)
  (let ((window (selected-window))
        (file (nomis-dirtree-selected-file-or-dir)))
    (when file
      (find-file-other-window file)
      (select-window window))))

(defun nomis-dirtree-previous-line (arg)
  (interactive "p")
  (tree-mode-previous-node arg))

(defun nomis-dirtree-next-line (arg)
  (interactive "p")
  (tree-mode-next-node arg))

(defun nomis-dirtree-up-directory (arg)
  (interactive "p")
  (tree-mode-goto-parent arg))

(defun* nomis-dirtree-find-file-if-dir-helper (&key (beep-if-not-dir t))
  (if (file-directory-p (nomis-dirtree-selected-file-or-dir))
      (nomis-dirtree-display-file)
    (when beep-if-not-dir
      (beep))))

(defun nomis-dirtree-find-file-if-dir ()
  "Nomis Dirtree:
If selected entry is a directory go into it."
  (interactive)
  (nomis-dirtree-find-file-if-dir-helper :beep-if-not-dir t))

(defun nomis-dirtree-previous-line-and-display (arg)
  "Nomis Dirtree:
Move up lines and display file in other window."
  (interactive "p")
  (nomis-dirtree-previous-line arg)
  (nomis-dirtree-display-file))

(defun nomis-dirtree-next-line-and-display (arg)
  "Nomis Dirtree:
Move down lines and display file in other window."
  (interactive "p")
  (nomis-dirtree-next-line arg)
  (nomis-dirtree-display-file))

(defun nomis-dirtree-down-directory-and-display ()
  "Nomis Dirtree:
Go into selected directory and display its contents in other window."
  (interactive)
  (nomis-dirtree-find-file-if-dir-helper :beep-if-not-dir nil)
  (nomis-dirtree-display-file))

(defun nomis-dirtree-up-directory-and-display (arg)
  "Nomis Dirtree:
Go up a directory and display its contents in other window."
  (interactive "p")
  (nomis-dirtree-up-directory arg)
  (nomis-dirtree-display-file))

(progn

  (define-key nomis-dirtree-mode-map (kbd "M-<RET>") 'nomis-dirtree-display-file)
   
  (define-key nomis-dirtree-mode-map (kbd "M-<up>") 'nomis-dirtree-previous-line)
  (define-key nomis-dirtree-mode-map (kbd "M-<down>") 'nomis-dirtree-next-line)
  (define-key nomis-dirtree-mode-map (kbd "M-<left>") 'nomis-dirtree-up-directory)
  (define-key nomis-dirtree-mode-map (kbd "M-<right>") 'nomis-dirtree-find-file-if-dir)
   
  (define-key nomis-dirtree-mode-map (kbd "M-S-<up>") 'nomis-dirtree-previous-line-and-display)
  (define-key nomis-dirtree-mode-map (kbd "M-S-<down>") 'nomis-dirtree-next-line-and-display)
  (define-key nomis-dirtree-mode-map (kbd "M-S-<left>") 'nomis-dirtree-up-directory-and-display)
  (define-key nomis-dirtree-mode-map (kbd "M-S-<right>") 'nomis-dirtree-down-directory-and-display))

(provide 'nomis-dirtree)
