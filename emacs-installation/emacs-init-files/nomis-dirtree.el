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

;;;; - Bug: `nomis-dirtree-history-step-back-and-display`
;;;;        does the display part when the first part fails.
;;;;        We need to be thowing errors, probably at all places
;;;;        where we currenty beep.

;;;; - Navigation with (eg) control key skipping to only files
;;;;   that were displayed would be good.  (Right?)

;;;; - When hitting up and down arrow keys, when you hit directories
;;;;   there are messages saying "Expand" and "Collapse" (depending on
;;;;   whether the directory is collapsed or expanded).
;;;;   - I've looked, but I can't even work out where the messages are
;;;;     coming from.

;;;; - Navigate to node in tree from a file.

;;;; - Look into the Tree menu.
;;;;   - Is stuff not as it says?
;;;;   - Can you add to it?  Do you want to?

;;;; - Scan all for badness.
;;;; - Tidy.

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
          (setq tree (tree-mode-insert
                      (nomis-dirtree-make-root-widget-spec root)))))
    ;; (setq win (get-buffer-window nomis-dirtree-buffer))
    (unless win
      ;;(setq win (get-buffer-window nomis-dirtree-buffer))
      (setq win (apply 'windata-display-buffer nomis-dirtree-buffer nomis-dirtree-windata))
      (select-window win))
    (with-selected-window win
      (unless (widget-get tree :open)
        (widget-apply-action tree))
      (goto-char (widget-get tree :from))
      (recenter 1)
      (nomis-dirtree-note-current-selection))
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
          (setq tree (tree-mode-insert
                      (nomis-dirtree-make-root-widget-spec root)))))
    (if select
        (switch-to-buffer nomis-dirtree-buffer))))

(define-derived-mode nomis-dirtree-mode tree-mode "Dir-Tree"
  "A mode to display tree of directory"
  (make-local-variable '*nomis-dirtree/paths/current*)
  (make-local-variable '*nomis-dirtree/paths/history-list*)
  (make-local-variable '*nomis-dirtree/paths/future-list*)
  (tree-widget-set-theme "folder"))

(defun nomis-dirtree-make-root-widget-spec (directory)
  "create the root directory"
  `(nomis-dirtree-directory-widget
    :node (nomis-dirtree-file-widget-for-directory
           :tag ,directory
           :file ,directory)
    :file ,directory
    :open t
    :nomis-root t))

(defconst nomis-dirtree/approach-to-children :new)
(defconst nomis-dirtree/dirs-at-top? nil)

(defun nomis-dirtree-setup-children (tree)
  "expand directory"
  (case nomis-dirtree/approach-to-children
    (:old
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
    (:new
     (or (widget-get tree :args)
         (let* ((directory (widget-get tree :file))
                (re (dired-omit-regexp))
                (file-&-basename-pairs
                 (->> (directory-files directory t)
                      (-map (lambda (f)
                              (let* ((basename (file-name-nondirectory f)))
                                (unless (string-match re basename)
                                  (cons f basename)))))
                      (-remove #'null))))
           (cl-labels ((make-directory-widget
                        (file-&-basename)
                        `(nomis-dirtree-directory-widget
                          :file ,(car file-&-basename)
                          :node (nomis-dirtree-file-widget-for-directory
                                 :tag ,(cdr file-&-basename)
                                 :file ,(car file-&-basename))))
                       (make-file-widget
                        (file-&-basename)
                        `(nomis-dirtree-file-widget-for-file
                          :file ,(car file-&-basename)
                          :tag ,(cdr file-&-basename)))
                       (make-widget
                        (file-&-basename)
                        (if (directory? file-&-basename)
                            (make-directory-widget file-&-basename)
                          (make-file-widget file-&-basename)))
                       (directory?
                        (file-&-basename)
                        (file-directory-p (car file-&-basename)))
                       (sort-files-or-dirs
                        (file-&-basename-pairs)
                        (sort file-&-basename-pairs
                              (lambda (a b) (string< (cdr a) (cdr b))))))
             (if nomis-dirtree/dirs-at-top?
                 (let ((dirs  (-filter #'directory? file-&-basename-pairs))
                       (files (-remove #'directory? file-&-basename-pairs)))
                   (->> (append (sort-files-or-dirs dirs)
                                (sort-files-or-dirs files))
                        (-map #'make-widget)))
               (->> file-&-basename-pairs
                    sort-files-or-dirs
                    (-map #'make-widget)))))))))

;;;; ___________________________________________________________________________
;;;; My stuff.

;;;; ---------------------------------------------------------------------------
;;;; Support for expand/collapse.

(defun nomis-dirtree-expand-node (widget)
  (when (tree-widget-p widget)
    (unless (widget-get widget :open)
      (widget-apply-action widget))))

(defun nomis-dirtree-collapse-node (widget)
  (when (tree-widget-p widget)
    (when (widget-get widget :open)
      (widget-apply-action widget))))

(defvar *dirs-to-keep-collapsed-unless-forced*
  '("\\.git"
    "\\.idea"
    "\\.repl"
    "\\.vagrant"
    "\\.sync"
    "checkouts"
    "out"
    "target"))

(defun directory-to-keep-collapsed-p (name)
  (some (lambda (no-expand-name)
          (string-match (concat "/" no-expand-name "$")
                        name))
        *dirs-to-keep-collapsed-unless-forced*))

;;;; ---------------------------------------------------------------------------
;;;; Widget and file stuff.

;;;; FIXME This mixes the domains of widgets and files.

(defun nomis-dirtree-widget-file (widget)
  (widget-get widget :file))

(defun nomis-dirtree-widget-children (widget)
  (widget-get widget :children))

(defun nomis-dirtree-selected-widget ()
  (let* ((widget (widget-at (1- (line-end-position)))))
    (if (nomis-dirtree-file-widget-for-directory-p widget)
        (widget-get widget :parent)
      widget)))

(defun nomis-dirtree-selected-file ()
  (-> (nomis-dirtree-selected-widget)
      nomis-dirtree-widget-file))

(defun nomis-dirtree-root-p (widget)
  (plist-get (rest widget)
             :nomis-root))

(defun nomis-dirtree-parent-widget (widget)
  (widget-get widget :parent))

(defun nomis-dirtree-widget-path (widget)
  (cl-labels ((helper
               (w)
               (if (nomis-dirtree-root-p w)
                   (list w)
                 (cons w
                       (-> (nomis-dirtree-parent-widget w)
                           helper)))))
    (-> (helper widget)
        reverse)))

(defun nomis-dirtree-root-widget (widget)
  (if (nomis-dirtree-root-p widget)
      widget
    (-> widget
        nomis-dirtree-parent-widget
        nomis-dirtree-root-widget)))

(defun nomis-dirtree-file-path ()
  (->> (nomis-dirtree-selected-widget)
       nomis-dirtree-widget-path
       (-map #'nomis-dirtree-widget-file)))

(defun nomis-dirtree-root-file ()
  (-> (nomis-dirtree-selected-widget)
      nomis-dirtree-root-widget
      nomis-dirtree-widget-file))

(defun nomis-dirtree-file-path-filenames-FOR-DEBUG ()
  (->> (nomis-dirtree-file-path)
       (-map (lambda (s)
               (s-replace (nomis-dirtree-root-file)
                          ""
                          s)))))

;;;; ---------------------------------------------------------------------------

;;;; FIXME Move the related stuff to here.

(defvar *nomis-dirtree-inhibit-history?* nil)

;;;; ---------------------------------------------------------------------------
;;;; Navigation

(defun nomis-dirtree-goto-widget (widget)
  (goto-char (widget-get widget :from)))

(defun nomis-dirtree-tree-mode-goto-parent-CAN-DELETE-ME (arg)
  "Move to parent node.
   Like `tree-mode-goto-parent`, but throws an exception when there's no parent."
  (interactive "p")
  (let ((parent (tree-mode-parent-current-line)))
    (setq arg (1- arg))
    (if parent
        (progn
          (while (and (> arg 0)
                      (setq parent (widget-get parent :parent))
                      (setq arg (1- arg))))
          (nomis-dirtree-goto-widget parent))
      (error "No parent!"))))

(defun nomis-dirtree-goto-root ()
  ;; Sometimes we don't find the selected widget. I noticed it with:
  ;; - on "test" dir in the "clojure-the-language" project
  ;; - that dir is in the expanded state
  ;; - we do a `nomis/toggle-dirtree-dirs-at-top` to set
  ;;   `nomis-dirtree/dirs-at-top?` to true
  ;; I wonder why that's different to other dirs in the project.
  ;; - If I call `nomis-dirtree-root-file` from M-: it always finds
  ;;   the selection. Ah, but the difference is that you have recreated the
  ;;   tree when you toggle.
  ;; - Here's a clue: the cursor position is different for "test"
  ;;   after you do a `nomis/toggle-dirtree-dirs-at-top`.
  ;; - I added a new directory called "zzzz", and then that was
  ;;   bad and "test" was OK -- so the problem is with the last
  ;;   directory (alphabetically).
  (case 2
    (1
     (while (and (not (nomis-dirtree-root-p (nomis-dirtree-selected-widget)))
                 (tree-mode-parent-current-line))
       (nomis-dirtree-tree-mode-goto-parent-CAN-DELETE-ME 1)))
    (2
     (let* ((selected-widget (nomis-dirtree-selected-widget)))
       (if selected-widget
           (let* ((root-widget (-> selected-widget
                                   nomis-dirtree-root-widget)))
             (nomis-dirtree-goto-widget root-widget))
         (message "Didn't find selected widget."))))))

(defun nomis-dirtree-goto-file-that-is-displayed-in-tree (target-file)
  (let* ((*nomis-dirtree-inhibit-history?* t))
    (nomis-dirtree-goto-root)
    (let* ((root-file (nomis-dirtree-selected-file)))
      (while (not (equal target-file
                         (nomis-dirtree-selected-file)))
        ;; Be defensive: we should always find the file, but, in case we screw
        ;; something up, take care not to cycle around forever not finding it.
        ;; We could rely on an error thrown by `nomis-dirtree-next-line` when
        ;; it wraps, but we prefer not to.
        (ignore-errors (nomis-dirtree-next-line 1))
        (when (equal root-file
                     (nomis-dirtree-selected-file))
          ;; We looped around. This shouldn't happen.
          (error "Couldn't find target-file %s" target-file))))))

(defun nomis-dirtree/with-return-to-selected-file-fun (fun)
  (let* ((file-to-return-to
          (case 2
            (1 (-> (tree-mode-icon-current-line)
                   (widget-get :node)
                   nomis-dirtree-widget-file))
            (2 (nomis-dirtree-selected-file))))
         (res (funcall fun)))
    (nomis-dirtree-goto-file-that-is-displayed-in-tree file-to-return-to)
    res))

(defmacro nomis-dirtree/with-return-to-selected-file (&rest body)
  `(nomis-dirtree/with-return-to-selected-file-fun (lambda () ,@body)))

(defun nomis-dirtree-goto-path (path)
  (nomis-dirtree-goto-root)
  (nomis-dirtree-expand nil)
  (cl-loop for (f . r) on path
           ;; FIXME Want to do this without going to the root every time.
           ;;       This is O(n^2).
           do (progn
                (nomis-dirtree-goto-file-that-is-displayed-in-tree f)
                (when r
                  (nomis-dirtree-expand nil)))))

;;;; ---------------------------------------------------------------------------
;;;; History

;;;; FIXME Limit history growth.

;;;; FIXME You have these as buffer local, but they need to be per tree.
(defvar *nomis-dirtree/paths/current* nil)
(defvar *nomis-dirtree/paths/history-list* '())
(defvar *nomis-dirtree/paths/future-list* '())

(defun nomis-dirtree-note-current-selection ()
  (unless *nomis-dirtree-inhibit-history?*
    (when *nomis-dirtree/paths/current*
      (push *nomis-dirtree/paths/current*
            *nomis-dirtree/paths/history-list*))
    (setq *nomis-dirtree/paths/current* (nomis-dirtree-file-path))
    (setq *nomis-dirtree/paths/future-list* '())))

(defun nomis-dirtree/with-note-selection-when-done-fun (fun)
  (let* ((res (let* ((*nomis-dirtree-inhibit-history?* t))
                (funcall fun))))
    (nomis-dirtree-note-current-selection)
    res))

(defmacro nomis-dirtree/with-note-selection-when-done (&rest body)
  `(nomis-dirtree/with-note-selection-when-done-fun (lambda () ,@body)))

(defun nomis-dirtree-no-history? ()
  (null *nomis-dirtree/paths/history-list*))

(defun nomis-dirtree-no-future? ()
  (null *nomis-dirtree/paths/future-list*))

(defun nomis-dirtree-history-step-back-impl ()
  (assert (not (nomis-dirtree-no-history?)))
  (let* ((*nomis-dirtree-inhibit-history?* t)
         (path (pop *nomis-dirtree/paths/history-list*)))
    (push *nomis-dirtree/paths/current*
          *nomis-dirtree/paths/future-list*)
    (setq *nomis-dirtree/paths/current* path)
    (nomis-dirtree-goto-path path)))

(defun nomis-dirtree-history-step-forward-impl ()
  (assert (not (nomis-dirtree-no-future?)))
  (let* ((*nomis-dirtree-inhibit-history?* t)
         (path (pop *nomis-dirtree/paths/future-list*)))
    (push *nomis-dirtree/paths/current*
          *nomis-dirtree/paths/history-list*)
    (setq *nomis-dirtree/paths/current* path)
    (nomis-dirtree-goto-path path)))

;;;; ---------------------------------------------------------------------------
;;;; User-visible commands.

(defun nomis-dirtree-display-file ()
  "Display contents of file under point in other window."
  (interactive)
  (save-selected-window
    (let* ((file (nomis-dirtree-selected-file)))
      (when file
        (find-file-other-window file)))))

(defun nomis-dirtree-display-file-in-new-frame ()
  "Display contents of file under point in other window."
  (interactive)
  (save-selected-window
    (let* ((file (nomis-dirtree-selected-file)))
      (when file
        (find-file-other-frame file)))))

(defun nomis-dirtree-open-in-default-app ()
  "Open file under point using its default app."
  (interactive)
  (let* ((file (nomis-dirtree-selected-file)))
    (when file
      (shell-command (concat "open \"" file "\"")))))

(defun nomis-dirtree-next-line (arg)
  "Move down <arg> lines."
  (interactive "p")
  (nomis-dirtree/with-note-selection-when-done
   (tree-mode-next-node arg)))

(defun nomis-dirtree-next-line-and-display (arg)
  "Move down <arg> lines.
Then display contents of file under point in other window."
  (interactive "p")
  (nomis-dirtree-next-line arg)
  (nomis-dirtree-display-file))

(defun nomis-dirtree-previous-line (arg)
  "Move up <arg> lines."
  (interactive "p")
  (nomis-dirtree/with-note-selection-when-done
   (tree-mode-previous-node arg)))

(defun nomis-dirtree-previous-line-and-display (arg)
  "Move up <arg> lines.
Then display contents of file under point in other window."
  (interactive "p")
  (nomis-dirtree-previous-line arg)
  (nomis-dirtree-display-file))

(defun nomis-dirtree-next-line-with-expansion* (arg)
  (unless (< arg 1)
    (let* ((widget (nomis-dirtree-selected-widget)))
      (when (nomis-dirtree-directory-widget-p widget)
        (nomis-dirtree-expand-node widget))
      (nomis-dirtree-next-line 1))
    (nomis-dirtree-next-line-with-expansion* (1- arg))))

(defun nomis-dirtree-next-line-with-expansion (arg)
  "Move down <arg> lines, expanding any encountered collapsed directories
and showing previous expansion of subdirectories."
  (interactive "p")
  (nomis-dirtree/with-note-selection-when-done
   (nomis-dirtree-next-line-with-expansion* arg)))

(defun nomis-dirtree-next-line-with-expansion-and-display (arg)
  "Move down <arg> lines, expanding any encountered collapsed directories
and showing previous expansion of subdirectories.
Then display contents of file under point in other window."
  (interactive "p")
  (nomis-dirtree-next-line-with-expansion arg)
  (nomis-dirtree-display-file))

(defun nomis-dirtree-up-directory (arg)
  "Move to parent directory. Repeat <arg> times if <arg> supplied.
Then add the then-current line to the history."
  (interactive "p")
  (if (nomis-dirtree-root-p (nomis-dirtree-selected-widget))
      (progn
        (message "Already at root.")
        (beep))
    (progn
      (nomis-dirtree/with-note-selection-when-done
       (tree-mode-goto-parent arg)))))

(defun nomis-dirtree-up-directory-and-display (arg)
  "Move to parent directory. Repeat <arg> times if <arg> supplied.
Then add the then-current line to the history.
Then display contents of file under point in other window."
  (interactive "p")
  (nomis-dirtree-up-directory arg)
  (nomis-dirtree-display-file))

(defun nomis-dirtree-history-step-back ()
  "Go back in history."
  (interactive)
  (if (nomis-dirtree-no-history?)
      (progn
        (beep)
        (message "There is no more history."))
    (nomis-dirtree-history-step-back-impl)))

(defun nomis-dirtree-history-step-forward ()
  "Go forward in history."
  (interactive)
  (if (nomis-dirtree-no-future?)
      (progn
        (beep)
        (message "There is no more future."))
    (nomis-dirtree-history-step-forward-impl)))

(defun nomis-dirtree-history-step-back-and-display ()
  "Go back in history.
Then display contents of file under point in other window."
  (interactive)
  (nomis-dirtree-history-step-back)
  (nomis-dirtree-display-file))

(defun nomis-dirtree-history-step-forward-and-display ()
  "Go forward in history.
Then display contents of file under point in other window."
  (interactive)
  (nomis-dirtree-history-step-forward)
  (nomis-dirtree-display-file))

(defun nomis-dirtree-expand (arg)
  "Expand directory under point, showing previous expansion of subdirectories.
If <arg> is supplied, first collapse all and then expand to <arg> levels."
  (interactive "P")
  (cl-labels ((expand-recursively
               (widget n-times &optional force-expand-p)
               (when (and (nomis-dirtree-directory-widget-p widget)
                          (>= n-times 1)
                          (or force-expand-p
                              (not (directory-to-keep-collapsed-p
                                    (nomis-dirtree-widget-file widget)))))
                 (nomis-dirtree-expand-node widget)
                 (mapc (lambda (x) (expand-recursively x (1- n-times)))
                       (nomis-dirtree-widget-children widget)))))
    (let* ((widget (nomis-dirtree-selected-widget)))
      (if (nomis-dirtree-directory-widget-p widget)
          (progn
            (unless (null arg)
              (nomis-dirtree-collapse-all))
            (expand-recursively widget
                                (or arg 1)
                                t))
        (progn
          (message "Not a directory, so can't expand.")
          (beep))))))

(defun nomis-dirtree-collapse ()
  "Collapse directory under point, retaining previous expansion of subdirectories."
  (interactive)
  (let* ((widget (nomis-dirtree-selected-widget)))
    (if (nomis-dirtree-directory-widget-p widget)
        (nomis-dirtree-collapse-node widget)
      (progn
        (message "Not a directory, so can't collapse.")
        (beep)))))

(defun nomis-dirtree-expand-all ()
  "Expand directory under point to show all subdirectories,
sub-subdirectories, etc."
  (interactive)
  (nomis-dirtree-expand 1000000))

(defun collapse-recursively (widget)
  ;; For some reason having this in a `labels` within
  ;; `nomis-dirtree-collapse-all` doesn't work -- undefined function.
  ;; Weird. Was ok in Emacs 24.2 but not in 24.3
  (mapc 'collapse-recursively
        (nomis-dirtree-widget-children widget))
  (nomis-dirtree-collapse-node widget))

(defun nomis-dirtree-collapse-all ()
  "Collapse directory under point and all of its subdirectories,
sub-subdirectories, etc, so that subsequent expansion shows only one level."
  (interactive)
  (let* ((widget (nomis-dirtree-selected-widget)))
    (if (nomis-dirtree-directory-widget-p widget)
        (collapse-recursively widget)
      (progn
        (message "Not a directory, so can't collapse.")
        (beep)))))

(defun nomis/toggle-dirtree-dirs-at-top ()
  (interactive)
  (setq nomis-dirtree/dirs-at-top?
        (not nomis-dirtree/dirs-at-top?))
  (nomis-dirtree/with-return-to-selected-file
   (mapc #'tree-mode-reflesh-tree
         tree-mode-list)))

(defun nomis-dirtree-show-selection-info ()
  "Display some details of the file under point in a message dialog.
Mostly for debugging purposes."
  (interactive)
  (let* ((widget (nomis-dirtree-selected-widget))
         (file (nomis-dirtree-widget-file widget)))
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

(define-key global-map (kbd "H-q d") 'nomis-dirtree)

(cl-labels ((dk (k f)
                (define-key nomis-dirtree-mode-map k f)))

  (define-key widget-keymap (kbd "<RET>") nil)
  (dk (kbd "<RET>")       'nomis-dirtree-display-file)
  (dk (kbd "C-<return>")  'nomis-dirtree-display-file)
  (dk (kbd "M-<return>")  'nomis-dirtree-display-file-in-new-frame)

  (dk (kbd "M-o")         'nomis-dirtree-open-in-default-app)

  (dk (kbd "<down>")      'nomis-dirtree-next-line)
  (dk (kbd "C-<down>")    'nomis-dirtree-next-line-and-display)
  (dk (kbd "<up>")        'nomis-dirtree-previous-line)
  (dk (kbd "C-<up>")      'nomis-dirtree-previous-line-and-display)
  (dk (kbd "<right>")     'nomis-dirtree-next-line-with-expansion)
  (dk (kbd "C-<right>")   'nomis-dirtree-next-line-with-expansion-and-display)
  (dk (kbd "<left>")      'nomis-dirtree-up-directory)
  (dk (kbd "C-<left>")    'nomis-dirtree-up-directory-and-display)
  
  (dk (kbd "M-[")         'nomis-dirtree-history-step-back)
  (dk (kbd "C-M-[")       'nomis-dirtree-history-step-back-and-display)
  (dk (kbd "M-]")         'nomis-dirtree-history-step-forward)
  (dk (kbd "C-M-]")       'nomis-dirtree-history-step-forward-and-display)

  (dk (kbd "M-<right>")   'nomis-dirtree-expand)
  (dk (kbd "M-<left>")    'nomis-dirtree-collapse)
  (dk (kbd "M-S-<right>") 'nomis-dirtree-expand-all)
  (dk (kbd "M-S-<left>")  'nomis-dirtree-collapse-all)

  (dk (kbd "t")           'nomis/toggle-dirtree-dirs-at-top)
  
  (dk (kbd "M-d")         'nomis-dirtree-show-selection-info))

(provide 'nomis-dirtree)
