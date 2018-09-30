;;;; nomis-dirtree.el --- Directory tree views ---  -*- lexical-binding: t -*-

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

;;;; Bug:
;;;; When there are expanded directories within collapsed directories:
;;;; - You have watchers on those expanded directories
;;;    - I guess that's OK, but does it do what is expected?
;;;; - When you refresh, the expanded subdirectories stop being expanded.
;;;;   And now, with auto-refresh, you are updating automatically when there's
;;;;   a change, so this is very bad. (But less bad now that you are ignoring
;;;;   `stopped` events.)
;;;;   - Can you retain the expanded subdirectories?
;;;;     (But the problem is in dirtree, not nomis-dirtree --
;;;;     `tree-mode-reflesh-tree`.)
;;;;     Can you write your own refresh function?
;;;;   Also:
;;;;   You don't update your watchers or knowledge of expansion.
;;;;   - One way to fix this would be to occasionally crawl the trees and reset
;;;;     your watchers and knowledge of expansion.
;;;; PLAN:
;;;; - Understand the details of how tree-mode sets up children and does the
;;;    refresh. And then think about how to roll your own refresh.

;;;; Feature:
;;;; Maybe add feature to make tree selection follow file in current buffer.
;;;; - Use an idle timer -- maybe combine with the auto-refresh timer.

;;;; Faster history:
;;;; - Record buffer positions in the history.
;;;; - When navigating history, before the (expensive) current approach, check
;;;;   whether the recorded point's widget is the one you want.

;;;; - Bug: `xxxx-and-display` commands do the display part when the first part
;;;;        fails. We need to be throwing errors, probably at all places where
;;;;        we currenty beep.

;;;; - Look into the Tree menu.
;;;;   - Is stuff not as it says?
;;;;   - Can you add to it?  Do you want to?

;;;; - Scan all for badness.
;;;; - Tidy.

;;;; ___________________________________________________________________________

(defvar *nomis/dirtree/print-debug-messages?* nil)

(defun nomis/dirtree/debug-message (format-string &rest args)
  (when *nomis/dirtree/print-debug-messages?*
    (apply #'message format-string args )))

;;;; ___________________________________________________________________________
;;;; Initially we have the original dirtree, with much modification.
;;;; I don't have a deep understanding of this, but I've hacked it a fair bit.

(eval-when-compile
  (require 'cl))
(require 'tree-mode)
(require 'windata)
(require 'dired-x)

(require 's)
(require 'dash)

(require 'nomis-core-utils)
(require 'nomis-buffers-windows-frames)
(require 'nomis-files)
(require 'nomis-timers)

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;;; Resume the original dirtree, with much modification.

(defgroup nomis/dirtree/dirtree-group nil
  "Directory tree views"
  :group 'tools)

(defcustom nomis/dirtree/windata '(frame left 0.5 delete)
  "*Arguments to set the window buffer display.
See `windata-display-buffer' for setup the arguments."
  :type 'sexp
  :group 'nomis/dirtree/dirtree-group)

(defcustom nomis/dirtree/buffer "*nomis-dirtree*"
  "*Buffer name for nomis-dirtree"
  :type 'string
  :group 'nomis/dirtree/dirtree-group)

(define-widget 'nomis/dirtree/directory-widget 'tree-widget
  "Directory Tree widget."
  :dynargs        'nomis/dirtree/setup-children
  :has-children   t)

(define-widget 'nomis/dirtree/internal-directory-widget 'push-button
  "File widget."
  :format         "%[%t%]\n"
  :button-face    'default)

(define-widget 'nomis/dirtree/file-widget 'push-button
  "File widget."
  :format         "%[%t%]\n"
  :button-face    'default)

(cl-defun nomis/dirtree/make-directory-widget (file-&-basename
                                               &key root?)
  (let* ((file (car file-&-basename))
         (tag  (cdr file-&-basename))
         (tag (if (nomis/dirtree/directory-to-keep-collapsed?/basename tag)
                  (concat tag "  [no auto-expand]")
                tag)))
    (when root?
      (nomis/dirtree/note-directory-expanded file))
    `(nomis/dirtree/directory-widget
      :tag ,tag
      :file ,file
      :node (nomis/dirtree/internal-directory-widget
             :tag ,tag
             :file ,file)
      :open ,root?
      :nomis/root ,root?)))

(defun nomis/dirtree/make-file-widget (file-&-basename)
  `(nomis/dirtree/file-widget
    :file ,(car file-&-basename)
    :tag ,(cdr file-&-basename)))

(cl-defun nomis/dirtree/make-widget (file-&-basename
                                     &key root?)
  ;; FIXME Maybe the basename here, and simplify other stuff.
  ;;       (But sorting is on the basename, so maybe not.)
  (if (file-directory-p (car file-&-basename))
      (nomis/dirtree/make-directory-widget file-&-basename
                                           :root? root?)
    (nomis/dirtree/make-file-widget file-&-basename)))

(defun nomis/dirtree/make-root-widget (directory)
  "create the root directory"
  (nomis/dirtree/make-widget (cons directory
                                   (nomis/directory-no-slash directory))
                             :root? t))

(defun nomis/dirtree/directory-widget? (widget)
  (eql (car widget) 'nomis/dirtree/directory-widget))

(defun nomis/dirtree/internal-directory-widget? (widget)
  (eql (car widget) 'nomis/dirtree/internal-directory-widget))

(defun nomis/dirtree/file-widget? (widget)
  (eql (car widget) 'nomis/dirtree/file-widget))

(defun nomis/dirtree/widget? (widget)
  (or (nomis/dirtree/directory-widget? widget)
      (nomis/dirtree/file-widget? widget)))

(defun nomis/dirtree/show ()
  "Show `nomis/dirtree/buffer'. Create tree when no parent directory find."
  (interactive)
  (let ((buffer (get-buffer-create nomis/dirtree/buffer))
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
          (nomis/dirtree/make-dirtree (widget-get tree :file) t)
          (setq button (tree-mode-find-node tree path))
          (if button
              (goto-char (widget-get (car button) :from))))
      (call-interactively 'nomis/dirtree/make-dirtree))))

(defun nomis/dirtree/make-dirtree/do-it (root select)
  (let ((buffer (get-buffer-create nomis/dirtree/buffer))
        tree win)
    (with-current-buffer buffer
      (unless (eq major-mode 'nomis/dirtree/mode)
        (nomis/dirtree/mode))
      (dolist (atree tree-mode-list)
        (if (string= (widget-get atree :file) root)
            (setq tree atree)))
      (or tree
          (setq tree (tree-mode-insert
                      (nomis/dirtree/make-root-widget root)))))
    ;; (setq win (get-buffer-window nomis/dirtree/buffer))
    (unless win
      ;;(setq win (get-buffer-window nomis/dirtree/buffer))
      (setq win (apply 'windata-display-buffer nomis/dirtree/buffer nomis/dirtree/windata))
      (select-window win))
    (with-selected-window win
      (unless (widget-get tree :open)
        (widget-apply-action tree))
      (goto-char (widget-get tree :from))
      (recenter 1)
      (nomis/dirtree/note-selection))
    (if select
        (select-window win))))

(defun nomis/dirtree/make-dirtree (new-root select)
  "Create tree of `new-root' directory.
With prefix argument select `nomis/dirtree/buffer'"
  (interactive "DDirectory: \nP")
  (cl-flet ((do-it ()
                   (nomis/dirtree/make-dirtree/do-it new-root select)))
    (let* ((existing-roots (if (get-buffer nomis/dirtree/buffer)
                               (-map #'nomis/dirtree/widget-file
                                     (with-current-buffer nomis/dirtree/buffer
                                       (nomis/dirtree/all-trees)))
                             '())))
      (if (-any? (lambda (existing-root)
                   (s-starts-with? existing-root new-root))
                 existing-roots)
          ;; We already have `new-root` in dirtree.
          ;; TODO Make H-\ come here, I think.
          ;;      (Hmmmm... but `H-q d` does directory and `H-/` does
          ;;      the file.)
          (let* ((single-window-in-frame? (= 1 (length (window-list)))))
            ;; TODO Maybe remove duplication with `nomis/dirtree/goto-file*`.
            (switch-to-buffer-other-window nomis/dirtree/buffer)
            (when (and single-window-in-frame?
                       (fboundp 'flop-frame))
              ;; If we now have side-by-side windows, arrange them so that
              ;; dirtree buffer is on the left.
              (flop-frame))
            (nomis/dirtree/goto-file/internal new-root))
        ;; Remove any existing roots that are children of `new-root`, then
        ;; show a new tree for `new-root`.
        (let* ((existing-roots-to-remove
                (-filter (lambda (existing-root)
                           (s-starts-with? new-root existing-root))
                         existing-roots)))
          (mapc (lambda (existing-root)
                  (nomis/dirtree/goto-file/internal existing-root)
                  (nomis/dirtree/delete-tree/do-it))
                existing-roots-to-remove)
          (do-it))))))

(define-derived-mode nomis/dirtree/mode tree-mode "Dir-Tree"
  "A mode to display tree of directory"
  (tree-widget-set-theme "folder")
  (hl-line-mode)
  (face-remap-add-relative 'hl-line
                           (list (list :background "grey90"
                                       :box nil))))

(defconst nomis/dirtree/approach-to-children :new)
(defconst nomis/dirtree/dirs-at-top? nil)

(defun nomis/dirtree/setup-children (tree)
  "expand directory"
  (case nomis/dirtree/approach-to-children
    (:old
     (or (widget-get tree :args)
         (let ((directory (widget-get tree :file))
               (re (dired-omit-regexp))
               dirs files basename)
           (dolist (file (directory-files directory t))
             (setq basename (file-name-nondirectory file))
             (unless (string-match re basename)
               (if (file-directory-p file)
                   (push (cons (concat file "/") basename) dirs)
                 (push (cons file basename) files))))
           (setq dirs (sort dirs (lambda (a b) (string< (cdr a) (cdr b)))))
           (setq files (sort files (lambda (a b) (string< (cdr a) (cdr b)))))
           (-map #'nomis/dirtree/make-widget
                 (append dirs
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
                                  (cons (if (file-directory-p f)
                                            (concat f "/")
                                          f)
                                        basename)))))
                      (-remove #'null))))
           (cl-labels ((directory?
                        (file-&-basename)
                        (file-directory-p (car file-&-basename)))
                       (sort-files-or-dirs
                        (file-&-basename-pairs)
                        (sort file-&-basename-pairs
                              (lambda (a b) (string< (cdr a) (cdr b))))))
             (let* ((files-and-dirs
                     (if nomis/dirtree/dirs-at-top?
                         (cl-multiple-value-bind (dirs files)
                             (-separate #'directory?
                                        file-&-basename-pairs)
                           (append (sort-files-or-dirs dirs)
                                   (sort-files-or-dirs files)))
                       (->> file-&-basename-pairs
                            sort-files-or-dirs))))
               (-map #'nomis/dirtree/make-widget
                     files-and-dirs))))))))

;;;; ___________________________________________________________________________
;;;; My stuff.

;;;; ---------------------------------------------------------------------------
;;;; Get rid of annoying messages.

;;;; `widget-move` has a call to `widget-echo-help`, which causes annoying
;;;; messages in the *Messages* buffer.

(defvar *nomis/dirtree/in-widget-move?* nil)

(let* ((advice-name '-nomis/dirtree/no-widget-echo-help))
  (advice-add 'widget-move
              :around
              (lambda (orig-fun &rest args)
                (let* ((*nomis/dirtree/in-widget-move?* t))
                  (apply orig-fun args)))
              `((name . ,advice-name)))
  (advice-add 'widget-echo-help
              :around
              (lambda (orig-fun &rest args)
                (unless *nomis/dirtree/in-widget-move?*
                  (apply orig-fun args)))
              `((name . ,advice-name))))

;;;; ---------------------------------------------------------------------------
;;;; Wrappers for tree mode stuff

(defun nomis/dirtree/all-trees ()
  tree-mode-list)

(defun nomis/dirtree/goto-root/impl ()
  (tree-mode-goto-root))

(defun nomis/dirtree/refresh-tree/impl/with-arg (tree)
  (-> tree
      tree-mode-reflesh-tree))

(defun nomis/dirtree/refresh-tree/impl/no-arg ()
  (-> (nomis/dirtree/root-widget-no-arg)
      nomis/dirtree/refresh-tree/impl/with-arg))

(defun nomis/dirtree/next-line/impl (n)
  (tree-mode-next-node n))

(defun nomis/dirtree/previous-line/impl (n)
  (tree-mode-previous-node n))

(defun nomis/dirtree/up-directory/impl (n)
  (tree-mode-goto-parent n))

(defun nomis/dirtree/next-sib/impl (n)
  (tree-mode-next-sib n))

(defun nomis/dirtree/previous-sib/impl (n)
  (tree-mode-previous-sib n))

;;;; ---------------------------------------------------------------------------
;;;; nomis/dirtree/with-make-dirtree-window-active

;;;; Without this:
;;;; - If you changing the selection in the dirtree buffer, it doesn't work.
;;;; - If you auto-delete (from the file watcher code), the selection gets
;;;;   screwed up.

(defun nomis/dirtree/with-make-dirtree-window-active-fun (fun)
  (cl-flet ((do-it () (funcall fun)))
    (let* ((dirtree-window (nomis/find-window-in-any-frame nomis/dirtree/buffer))
           (dirtree-buffer (when dirtree-window
                             (window-buffer dirtree-window))))
      (if (null dirtree-window)
          (progn
            (message "dirtree-window is null. This can happen when nomis/dirtree/buffer exists but is not being displayed in any frame.")
            (do-it))
        (let* ((original-window (selected-window)))
          (unwind-protect
              (progn
                (select-window dirtree-window)
                (do-it))
            (select-window original-window)))))))

(defmacro nomis/dirtree/with-make-dirtree-window-active (&rest body)
  (declare (indent 0))
  `(nomis/dirtree/with-make-dirtree-window-active-fun
    (lambda () ,@body)))

;;;; ---------------------------------------------------------------------------
;;;; with-dirtree-buffer-if-it-exists

(defun with-dirtree-buffer-if-it-exists-fun (fun)
  (cl-flet ((do-it () (funcall fun)))
    (if (get-buffer nomis/dirtree/buffer)
        (with-current-buffer nomis/dirtree/buffer
          (nomis/dirtree/with-make-dirtree-window-active
            (do-it)))
      (do-it))))

(defmacro with-dirtree-buffer-if-it-exists (&rest body)
  (declare (indent 0))
  `(with-dirtree-buffer-if-it-exists-fun (lambda () ,@body)))

;;;; ---------------------------------------------------------------------------
;;;; nomis/dirtree/auto-refresh?
;;;; nomis/dirtree/expanded-directories
;;;; nomis/dirtree/directory-watchers

(require 'filenotify)

(defvar nomis/dirtree/auto-refresh? nil)
(defvar nomis/dirtree/expanded-directories '())
(defvar nomis/dirtree/directory-watchers '())

(defun nomis/dirtree/status ()
  (list :auto-refresh? nomis/dirtree/auto-refresh?
        :expanded-directories nomis/dirtree/expanded-directories
        :watched-directories (-map #'car nomis/dirtree/directory-watchers)))

(defun nomis/dirtree/add-expanded-directory (directory)
  (setf nomis/dirtree/expanded-directories
        (cons directory
              nomis/dirtree/expanded-directories)))

(defun nomis/dirtree/remove-expanded-directory (directory)
  (setq nomis/dirtree/expanded-directories
        ;; `-remove-first` is not only for efficiency -- there can be multiple
        ;; trees, so the same dir can be here twice.
        (-remove-first (-partial #'equal directory)
                       nomis/dirtree/expanded-directories)))

;;;; ---------------------------------------------------------------------------
;;;; Stuff to do on scheduled refresh

(defun nomis/dirtree/remove-watchers-of-deleted-dirs ()
  (setq nomis/dirtree/directory-watchers
        (->> nomis/dirtree/directory-watchers
             (-filter (lambda (entry)
                        (file-notify-valid-p (cdr entry)))))))

(defun nomis/dirtree/remove-roots-whose-dirs-are-deleted ()
  (with-dirtree-buffer-if-it-exists
    (loop for tree in (copy-list (nomis/dirtree/all-trees))
          unless (file-exists-p (nomis/dirtree/widget-file tree))
          do (progn
               (nomis/dirtree/goto-widget tree)
               (nomis/dirtree/delete-tree/do-it)))))

(defun nomis/dirtree/refresh-after-finding-buffer ()
  (condition-case err
      (with-dirtree-buffer-if-it-exists
        (nomis/dirtree/refresh/internal))
    (nomis/dirtree/file-not-found
     ;; We get here if the selected file is deleted -- not a problem.
     )))

;;;; ---------------------------------------------------------------------------
;;;; Refreshing and scheduling refreshes

(defvar nomis/dirtree/refresh-scheduled? nil)
(defvar nomis/dirtree/refresh-interval 2)

(defun nomis/dirtree/refresh ()
  (interactive)
  (when (nomis/find-window-in-any-frame nomis/dirtree/buffer)
    (condition-case err
        (progn
          (nomis/dirtree/remove-watchers-of-deleted-dirs)
          (nomis/dirtree/remove-roots-whose-dirs-are-deleted)
          (nomis/dirtree/refresh-after-finding-buffer)
          (setq nomis/dirtree/refresh-scheduled? nil))
      (error
       (message "Error in nomis/dirtree/refresh %s %s"
                (car err)
                (cdr err))))))

(nomis/def-timer-with-relative-repeats
    nomis/dirtree/refresh-timer
    nomis/dirtree/refresh-interval
  (when nomis/dirtree/refresh-scheduled?
    (nomis/dirtree/refresh))
  `(:repeat ,nomis/dirtree/refresh-interval) ; TODO This is a weird way of specifying the repeat interval
  )

;;;; ---------------------------------------------------------------------------

(defun nomis/dirtree/turn-on-auto-refresh ()
  (when (not nomis/dirtree/auto-refresh?)
    (assert (null nomis/dirtree/directory-watchers))
    (setq nomis/dirtree/auto-refresh? t)
    (nomis/dirtree/refresh)
    (mapc #'nomis/dirtree/add-directory-watcher
          nomis/dirtree/expanded-directories)))

(defun nomis/dirtree/turn-off-auto-refresh ()
  (when nomis/dirtree/auto-refresh?
    (setq nomis/dirtree/auto-refresh? nil)
    (mapc #'nomis/dirtree/remove-directory-watcher
          nomis/dirtree/expanded-directories)
    (assert (null nomis/dirtree/directory-watchers))))

(defun nomis/dirtree/toggle-auto-refresh ()
  (interactive)
  (if nomis/dirtree/auto-refresh?
      (nomis/dirtree/turn-off-auto-refresh)
    (nomis/dirtree/turn-on-auto-refresh))
  (message "nomis-dirtree auto refresh turned %s"
           (if nomis/dirtree/auto-refresh? "on" "off")))

;;;; ---------------------------------------------------------------------------
;;;; File watchers

(defun nomis/dirtree/handle-watch-event (event)
  (let* ((action (cadr event)))
    (when (-contains? '(created
                        deleted
                        renamed)
                      action)
      (let* ((filename (caddr event))
             (filename-no-dir (file-name-nondirectory filename)))
        (unless (or (string-match-p "^.#" filename-no-dir)
                    (string-match-p "^#.*#$" filename-no-dir))
          (setq nomis/dirtree/refresh-scheduled? t))))))

(defun nomis/dirtree/add-directory-watcher (directory)
  (let ((watcher (file-notify-add-watch directory
                                        '(change)
                                        'nomis/dirtree/handle-watch-event)))
    (setf nomis/dirtree/directory-watchers
          (cons (cons directory watcher)
                nomis/dirtree/directory-watchers))))

(defun nomis/dirtree/remove-directory-watcher (directory)
  (setq nomis/dirtree/directory-watchers
        ;; `-remove-first` is not only for efficiency -- there can be multiple
        ;; trees, so the same dir can be here twice.
        (-remove-first (lambda (entry)
                         (if (equal (car entry) directory)
                             (progn
                               (file-notify-rm-watch (cdr entry))
                               t)
                           nil))
                       nomis/dirtree/directory-watchers)))

(defun nomis/dirtree/note-directory-expanded (directory)
  (nomis/dirtree/add-expanded-directory directory)
  (when nomis/dirtree/auto-refresh?
    (nomis/dirtree/add-directory-watcher directory)))

(defun nomis/dirtree/note-directory-collapsed (directory)
  (nomis/dirtree/remove-expanded-directory directory)
  (when nomis/dirtree/auto-refresh?
    (nomis/dirtree/remove-directory-watcher directory)))

;;;; ---------------------------------------------------------------------------
;;;; nomis/dirtree/kill-buffer-hook

(cl-defun nomis/dirtree/kill-buffer-hook (&rest args)
  (when (equal (buffer-name (current-buffer))
               nomis/dirtree/buffer)
    ;; An easy way to remove watchers:
    (nomis/dirtree/collapse-recursively-all-trees)))

(add-hook 'kill-buffer-hook
          'nomis/dirtree/kill-buffer-hook)

;;;; ---------------------------------------------------------------------------
;;;; Widget and file stuff.

;;;; FIXME Would be nice to have clearer separation of the widget and
;;;;       file domains.

;;;; FIXME Make with-arg and no-arg versions of everything use consistent
;;;;       naming.

(defun nomis/dirtree/expanded? (widget)
  (widget-get widget :open))

(defun nomis/dirtree/expand-node (widget)
  (when (tree-widget-p widget)
    (unless (nomis/dirtree/expanded? widget)
      (widget-apply-action widget)
      (-> (nomis/dirtree/widget-file widget)
          nomis/dirtree/note-directory-expanded))))

(defun nomis/dirtree/collapse-node (widget)
  (when (tree-widget-p widget)
    (when (nomis/dirtree/expanded? widget)
      (widget-apply-action widget)
      (-> (nomis/dirtree/widget-file widget)
          nomis/dirtree/note-directory-collapsed))))

(defvar *nomis/dirtree/dirs-to-keep-collapsed-unless-forced*
  '("\\.emacs\\.d"
    "\\.git"
    "\\.idea"
    "\\.repl"
    "\\.vagrant"
    "\\.sync"
    "checkouts"
    "node_modules"
    "out"
    "target"
    "zzzz-nomis-dirtree-test-keep-collapsed"))

(defun nomis/dirtree/directory-to-keep-collapsed?/fullname (name)
  (some (lambda (no-expand-name)
          (string-match (concat "/" no-expand-name "/" "$")
                        name))
        *nomis/dirtree/dirs-to-keep-collapsed-unless-forced*))

(defun nomis/dirtree/directory-to-keep-collapsed?/basename (basename)
  (let* ((res (some (lambda (no-expand-name)
                      (string-match (concat "^" no-expand-name "$")
                                    basename))
                    *nomis/dirtree/dirs-to-keep-collapsed-unless-forced*)))
    res))

(defun nomis/dirtree/widget-file (widget)
  (widget-get widget :file))

(defun nomis/dirtree/widget-tag (widget)
  (widget-get widget :tag))

(defun nomis/dirtree/widget-parent (widget)
  (assert (nomis/dirtree/widget? widget))
  (widget-get widget :parent))

(defun nomis/dirtree/widget-children/all (widget)
  (widget-get widget :children))

(defun nomis/dirtree/widget-children (widget)
  (-remove (lambda (w)
             (eql (car w)
                  'nomis/dirtree/internal-directory-widget))
           (nomis/dirtree/widget-children/all widget)))

(defun nomis/dirtree/selected-widget/no-extras ()
  (widget-at (1- (line-end-position))))

(defun nomis/dirtree/selected-widget/with-extras ()
  (when (= (point) (point-max))
    ;; We're at that nasty place at the end of the buffer.
    ;; Doing this allows eg `nomis/dirtree/goto-file` when at that place.
    (backward-char))
  (let* ((widget (nomis/dirtree/selected-widget/no-extras)))
    (if (nomis/dirtree/internal-directory-widget? widget)
        (widget-get widget :parent)
      widget)))

(defun nomis/dirtree/selected-file ()
  (-> (nomis/dirtree/selected-widget/with-extras)
      nomis/dirtree/widget-file))

(defun nomis/dirtree/root-p (widget)
  (assert (nomis/dirtree/widget? widget))
  (plist-get (rest widget)
             :nomis/root))

(defun nomis/dirtree/widget-path (widget)
  (cl-labels ((helper
               (w)
               (if (nomis/dirtree/root-p w)
                   (list w)
                 (cons w
                       (-> (nomis/dirtree/widget-parent w)
                           helper)))))
    (-> (helper widget)
        reverse)))

(defun nomis/dirtree/root-widget (widget)
  (if (nomis/dirtree/root-p widget)
      widget
    (-> widget
        nomis/dirtree/widget-parent
        nomis/dirtree/root-widget)))

(defun nomis/dirtree/file-path ()
  (->> (nomis/dirtree/selected-widget/with-extras)
       nomis/dirtree/widget-path
       (-map #'nomis/dirtree/widget-file)))

(defun nomis/dirtree/root-widget-no-arg ()
  (-> (nomis/dirtree/selected-widget/with-extras)
      nomis/dirtree/root-widget))

(defun nomis/dirtree/root-file ()
  (-> (nomis/dirtree/root-widget-no-arg)
      nomis/dirtree/widget-file))

(defun nomis/dirtree/file-path-filenames-FOR-DEBUG ()
  (->> (nomis/dirtree/file-path)
       (-map (lambda (s)
               (s-replace (nomis/dirtree/root-file)
                          ""
                          s)))))

(defun nomis/dirtree/filename->root-widget/no-error (filename)
  (cl-find-if (lambda (w)
                (s-starts-with? (nomis/dirtree/widget-file w)
                                filename))
              (nomis/dirtree/all-trees)))

(defun nomis/dirtree/filename->root-widget (filename)
  (let* ((widget (nomis/dirtree/filename->root-widget/no-error filename)))
    (assert widget
            nil
            "File is not in dirtree: %s"
            filename)
    widget))

(defun nomis/dirtree/filename->root-filename (filename)
  (-> filename
      nomis/dirtree/filename->root-widget
      nomis/dirtree/widget-file))

(defun nomis/dirtree/filename->path-from-root (filename)
  (let* ((root-file (nomis/dirtree/filename->root-filename
                     filename)))
    (nomis/filename->path-from-a-root filename
                                      root-file)))

(defun nomis/dirtree/collapse-recursively-all-trees ()
  (mapc #'collapse-recursively
        (nomis/dirtree/all-trees)))

;;;; ---------------------------------------------------------------------------
;;;; Navigation

(defun nomis/dirtree/goto-widget (widget)
  (goto-char (widget-get widget :from)))

(defun nomis/dirtree/goto-selected-widget ()
  ;; Maybe useful in cases where selection has got screwed. (But I think you
  ;; have fixed those -- it was to do with refresh screwing things up, and now
  ;; you do a return-to-selected-file when refreshing.)
  (nomis/dirtree/goto-widget (nomis/dirtree/selected-widget/with-extras)))

(define-error 'nomis/dirtree/file-not-found "nomis-dirtree: No such file")

(cl-defun nomis/dirtree/goto-filename (filename
                                       &key refresh-not-allowed?)
  ;; TODO You also have `nomis/dirtree/goto-file` and similar
  ;;      - Need some renaming.
  (let* ((path (nomis/dirtree/filename->path-from-root filename)))
    (nomis/dirtree/debug-message "Going to %s" (first (last path)))
    (cl-labels ((goto-file-that-is-in-expansion
                 (target-file)
                 ;; If `target-file` is in the tree's expansion, make it the
                 ;; selection; otherwise throw an exception.
                 (let* ((start-file (nomis/dirtree/selected-file)))
                   (while (not (equal target-file
                                      (nomis/dirtree/selected-file)))
                     (ignore-errors ; so we cycle around at end of buffer
                       (nomis/dirtree/next-line/impl 1))
                     (when (equal start-file
                                  (nomis/dirtree/selected-file))
                       (signal 'nomis/dirtree/file-not-found
                               target-file)))))
                (search
                 ()
                 (cl-loop for (f . r) on path
                          do (progn
                               (goto-file-that-is-in-expansion f)
                               (when r
                                 (nomis/dirtree/expand nil))))))
      ;; Search. If we fail to find to find `target-file` refresh and try again.
      (condition-case err
          (search)
        (nomis/dirtree/file-not-found
         (if refresh-not-allowed?
             (signal (car err) (cdr err))
           (progn
             (nomis/dirtree/refresh-tree/impl/no-arg)
             (nomis/dirtree/goto-root/impl) ; because refresh sometimes jumps us to mad and/or bad place
             (search))))))))

(defun nomis/dirtree/with-return-to-selected-file-fun (fun)
  (let* ((file-to-return-to (nomis/dirtree/selected-file))
         (res (funcall fun)))
    ;; We use goto-path here rather than goto-file-that-is-in-expansion
    ;; because the selected file will not be in the expansion if the called
    ;; `fun` does a refresh and if the selected file has been deleted.
    (nomis/dirtree/goto-filename file-to-return-to :refresh-not-allowed? t)
    res))

(defmacro nomis/dirtree/with-return-to-selected-file (&rest body)
  `(nomis/dirtree/with-return-to-selected-file-fun (lambda () ,@body)))

(defun nomis/dirtree/refresh-tree (tree)
  (nomis/dirtree/with-return-to-selected-file ; because refresh sometimes jumps us to mad and/or bad place
   (nomis/dirtree/refresh-tree/impl/with-arg tree)))

(defun nomis/dirtree/refresh/internal ()
  (nomis/dirtree/with-return-to-selected-file ; because refresh sometimes jumps us to mad and/or bad place
   (mapc #'nomis/dirtree/refresh-tree/impl/with-arg
         (nomis/dirtree/all-trees))))

;;;; ---------------------------------------------------------------------------
;;;; History

(defvar *nomis/dirtree/inhibit-history?* nil)

(defvar *nomis/dirtree/filenames/current* nil)
(defvar *nomis/dirtree/filenames/history-list* '())
(defvar *nomis/dirtree/filenames/future-list* '())

(defvar *nomis/dirtree/max-history-size* 100)

(defun nomis/dirtree/clear-history ()
  (interactive)
  (setq *nomis/dirtree/filenames/history-list* '())
  (setq *nomis/dirtree/filenames/future-list* '())
  (setq *nomis/dirtree/filenames/current* (nomis/dirtree/selected-file))
  (message "Cleared history.")
  (nomis/grab-user-attention/low))

(defun nomis/dirtree/note-selection ()
  (if (null (nomis/dirtree/selected-widget/with-extras))
      (message "Not on a widget")
    (unless (or *nomis/dirtree/inhibit-history?*
                (equal *nomis/dirtree/filenames/current*
                       (nomis/dirtree/selected-file)))
      (nomis/dirtree/debug-message "Noting selection %s"
                                   (nomis/dirtree/selected-file))
      (when *nomis/dirtree/filenames/current*
        (setq *nomis/dirtree/filenames/history-list*
              (seq-take (cons *nomis/dirtree/filenames/current*
                              *nomis/dirtree/filenames/history-list*)
                        *nomis/dirtree/max-history-size*)))
      (setq *nomis/dirtree/filenames/current* (nomis/dirtree/selected-file))
      (setq *nomis/dirtree/filenames/future-list* '()))))

(defun nomis/dirtree/with-note-selection-fun (fun)
  (nomis/dirtree/note-selection)
  (unwind-protect
      (let* ((*nomis/dirtree/inhibit-history?* t))
        (funcall fun))
    (nomis/dirtree/note-selection)))

(defmacro nomis/dirtree/with-note-selection (&rest body)
  `(nomis/dirtree/with-note-selection-fun (lambda () ,@body)))

(defun nomis/dirtree/no-history? ()
  (null *nomis/dirtree/filenames/history-list*))

(defun nomis/dirtree/no-future? ()
  (null *nomis/dirtree/filenames/future-list*))

(defun nomis/dirtree/history-step-back-impl ()
  (assert (not (nomis/dirtree/no-history?)))
  (let* ((filename (pop *nomis/dirtree/filenames/history-list*)))
    (push *nomis/dirtree/filenames/current*
          *nomis/dirtree/filenames/future-list*)
    (setq *nomis/dirtree/filenames/current* filename)
    (nomis/dirtree/goto-filename filename)))

(defun nomis/dirtree/history-step-forward-impl ()
  (assert (not (nomis/dirtree/no-future?)))
  (let* ((filename (pop *nomis/dirtree/filenames/future-list*)))
    (push *nomis/dirtree/filenames/current*
          *nomis/dirtree/filenames/history-list*)
    (setq *nomis/dirtree/filenames/current* filename)
    (nomis/dirtree/goto-filename filename)))

;;;; ---------------------------------------------------------------------------
;;;; User-visible commands.

(defun nomis/dirtree/display-file* ()
  "Display contents of file under point in other window."
  (save-selected-window
    (let* ((file (nomis/dirtree/selected-file)))
      (when file
        (find-file-other-window file)))))

(cl-defmacro nomis/dirtree/define-command/with-and-without-and-display
    ;; FIXME Can you make M-. work for the `name-for-and-display` function?
    ;;       See `find-function-regexp-alist` for pointers.
    ;;       I found a reference to that at https://emacs.stackexchange.com/questions/31042/how-can-i-record-where-a-function-is-defined-if-its-done-indirectly
    (name
     name-for-and-display
     args
     &key
     doc-string
     preamble
     body
     no-record-history?)
  (declare (indent 3))
  (let* ((doc-string (concat doc-string
                             (unless no-record-history?
                               "
Record history -- record the selection before and after moving around."))))
    `(progn
       (defun ,name (,@args)
         ,doc-string
         ,@preamble
         (cl-labels ((do-it
                      ()
                      ,@body))
           ,(if no-record-history?
                `(do-it)
              `(nomis/dirtree/with-note-selection
                (do-it)))))
       (defun ,name-for-and-display (,@args)
         ,(concat doc-string
                  "
Then display contents of file under point in other window.")
         ,@preamble
         (,name ,@args)
         (nomis/dirtree/display-file*)))))

(defun nomis/dirtree/goto-file/internal (filename)
  (with-dirtree-buffer-if-it-exists
    (nomis/dirtree/with-note-selection
     (nomis/dirtree/goto-filename filename))
    (when (bound-and-true-p hl-line-mode)
      ;; Workaround for bug.
      ;; Without this we don't have the highlighting.
      (hl-line-mode 1))))

(defun nomis/dirtree/filename->dir (filename)
  (cond ((file-regular-p filename)
         (file-name-directory filename))
        ((file-directory-p filename)
         filename)
        (t
         (error "No such file: %S" filename))))

(defun nomis/dirtree/has-file? (filename)
  (with-dirtree-buffer-if-it-exists
    (nomis/dirtree/filename->root-widget/no-error filename)))

(defun nomis/dirtree/make-dirtree-if-there-is-not-one (filename)
  (when (not (and (get-buffer nomis/dirtree/buffer)
                  (nomis/dirtree/has-file? filename)))
    ;; Create the nomis/dirtree buffer in a new frame. If you don't do this
    ;; you can't arrange for `nomis/dirtree/goto-file/return-to-window`
    ;; to work.
    (let* ((original-frame (selected-frame))
           (frame (make-frame)))
      (select-frame frame)
      (unwind-protect
          (let* ((*nomis/dirtree/inhibit-history?* t))
            (nomis/dirtree/make-dirtree (nomis/dirtree/filename->dir filename)
                                        nil))
        (delete-frame)
        (select-frame-set-input-focus original-frame)))))

(cl-defun nomis/dirtree/goto-file* (&key return-to-original-window?)
  (let* ((filename (let* ((filename (or buffer-file-name
                                        dired-directory
                                        ;; default-directory
                                        )))
                     (when filename
                       (expand-file-name filename)))))
    (cond
     ((null filename)
      (message "This buffer has no associated file.")
      (nomis/beep))
     (t
      (let* ((single-window-in-frame? (= 1 (length (window-list))))
             (original-window (selected-window)))
        (nomis/dirtree/make-dirtree-if-there-is-not-one filename)
        (switch-to-buffer-other-window nomis/dirtree/buffer)
        (when return-to-original-window?
          (select-window original-window))
        (nomis/dirtree/goto-file/internal filename)
        (when (and single-window-in-frame?
                   (fboundp 'flop-frame))
          ;; If we now have side-by-side windows, arrange them so that
          ;; dirtree buffer is on the left.
          (flop-frame)))))))

(defun nomis/dirtree/goto-file ()
  "Do the following:
   - Make a note of the current buffer's file; call it f.
     (If the current buffer is a dired buffer, f will be a directory.)
   - Let d be:
     - if f is a directory, then f
     - if f is an ordinary file, then f's directory.
   - One of the following:
     - If there's a nomis/dirtree buffer in the current frame, do nothing.
     - If a nomis/dirtree buffer exists but it is not displayed in the
       current frame, display it in another window in the current frame.
     - If no nomis/dirtree buffer exists, create one that shows d and
       display it in another window in the current frame.
   - If d is not already shown in the dirtree buffer, show it.
   - Change the nomis/dirtree selection to be f.
   - Select the window that contains nomis/dirtree buffer."
  (interactive)
  (nomis/dirtree/goto-file*))

(defun nomis/dirtree/goto-file/return-to-window ()
  "Like `nomis/dirtree/goto-file` except keep the current window selected."
  (interactive)
  (nomis/dirtree/goto-file* :return-to-original-window? t))

(defun nomis/dirtree/display-file ()
  "Display contents of file under point in other window."
  (interactive)
  (nomis/dirtree/note-selection)
  (nomis/dirtree/display-file*))

(defun nomis/dirtree/display-file-and-goto-other-window ()
  "Display contents of file under point in other window."
  (interactive)
  (nomis/dirtree/note-selection)
  (let* ((file (nomis/dirtree/selected-file)))
    (when file
      (nomis/dirtree/display-file)
      (find-file-other-window file))))

(defun nomis/dirtree/display-file-in-new-frame ()
  "Display contents of file under point in other window."
  (interactive)
  (nomis/dirtree/note-selection)
  (save-selected-window
    (let* ((file (nomis/dirtree/selected-file)))
      (when file
        (find-file-other-frame file)))))

(defun nomis/dirtree/open-in-default-app ()
  "Open file under point using its default app."
  (interactive)
  (nomis/dirtree/note-selection)
  (let* ((file (nomis/dirtree/selected-file)))
    (when file
      (shell-command (concat "open \"" file "\"")))))

(nomis/dirtree/define-command/with-and-without-and-display
    nomis/dirtree/next-line
    nomis/dirtree/next-line-and-display
    (arg)
  :doc-string "Move down <arg> lines."
  :preamble ((interactive "p"))
  :body ((let* ((on-last-line? (= (1- (point-max))
                                  (save-excursion
                                    (end-of-line)
                                    (point)))))
           (if on-last-line?
               (error "Can't move forward from last line.")
             (nomis/dirtree/next-line/impl arg)))))

(nomis/dirtree/define-command/with-and-without-and-display
    nomis/dirtree/previous-line
    nomis/dirtree/previous-line-and-display
    (arg)
  :doc-string "Move up <arg> lines."
  :preamble ((interactive "p"))
  :body ((let* ((on-first-line? (= 1
                                   (save-excursion
                                     (beginning-of-line)
                                     (point)))))
           (if on-first-line?
               (error "Can't move up from first line.")
             (nomis/dirtree/previous-line/impl arg)))))

(defun nomis/dirtree/expand-widget-y-or-n-p (widget)
  (nomis/y-or-n-p-with-quit->nil
   (concat "Really expand \""
           (nomis/dirtree/widget-tag widget)
           "\"?"
           " It's a directory I'd normally keep collapsed."
           " ")))

(defvar *nomis/dirtree/no-inverse/flash-time*
  0.5
  "For use as an arg to `nomis/grab-user-attention/low`.
Example of use:
The user pressed the right arrow (or whatever), but we won't
be droping into a subdirectory.
The natural \"inverse\" operation (pressing the left arrow (or
whatever)), will not take the user back to where they came from.
This is intended to alert the user to that.

Note:
Making this number too small means that the flash might not be seen.
I noticed this with the \"and-display\" commands. Maybe switching
windows mucks things up.")

(defun nomis/dirtree/next-line-with-expansion* (arg)
  (unless (< arg 1)
    (let* ((widget (nomis/dirtree/selected-widget/with-extras)))
      (let* ((directory? (nomis/dirtree/directory-widget? widget)))
        (if (and directory?
                 (not (nomis/dirtree/directory-to-keep-collapsed?/fullname
                       (nomis/dirtree/widget-file widget)))
                 (progn
                   ;; Expand the widget, and return truthy if there are
                   ;; children.
                   (nomis/dirtree/expand-node widget)
                   (nomis/dirtree/widget-children widget)))
            nil
          (progn
            (nomis/grab-user-attention/low *nomis/dirtree/no-inverse/flash-time*)))
        (nomis/dirtree/next-line 1)
        (nomis/dirtree/next-line-with-expansion* (1- arg))))))

(nomis/dirtree/define-command/with-and-without-and-display
    nomis/dirtree/next-line-with-expansion
    nomis/dirtree/next-line-with-expansion-and-display
    (arg)
  :doc-string
  "Move down <arg> lines, expanding any encountered collapsed directories
and showing previous expansion of subdirectories."
  :preamble ((interactive "p"))
  :body ((nomis/dirtree/goto-selected-widget) ; without this, when we are at the end of a line we end up on the next sibling
         (nomis/dirtree/next-line-with-expansion* arg)))

(nomis/dirtree/define-command/with-and-without-and-display
    nomis/dirtree/up-directory
    nomis/dirtree/up-directory-and-display
    (arg)
  :doc-string "Move to parent directory. Repeat <arg> times if <arg> supplied."
  :preamble ((interactive "p"))
  :body ((if (nomis/dirtree/root-p (nomis/dirtree/selected-widget/with-extras))
             (progn
               (message "Already at root.")
               (beep))
           (nomis/dirtree/up-directory/impl arg))))

(nomis/dirtree/define-command/with-and-without-and-display
    nomis/dirtree/next-sib
    nomis/dirtree/next-sib-and-display
    (arg)
  :doc-string "Move to next sibling node."
  :preamble ((interactive "p"))
  :body ((nomis/dirtree/next-sib/impl arg)))

(nomis/dirtree/define-command/with-and-without-and-display
    nomis/dirtree/previous-sib
    nomis/dirtree/previous-sib-and-display
    (arg)
  :doc-string "Move to previous sibling node."
  :preamble ((interactive "p"))
  :body ((let* ((selected-widget (nomis/dirtree/selected-widget/with-extras)))
           (if (eql (-> selected-widget
                        nomis/dirtree/widget-parent
                        nomis/dirtree/widget-children
                        first)
                    selected-widget)
               (message "No previous siblings")
             (nomis/dirtree/previous-sib/impl arg)))))

(nomis/dirtree/define-command/with-and-without-and-display
    nomis/dirtree/goto-root
    nomis/dirtree/goto-root-and-display
    ()
  :doc-string "Move to root node"
  :preamble ((interactive))
  :body ((nomis/dirtree/goto-root/impl)))

(nomis/dirtree/define-command/with-and-without-and-display
    nomis/dirtree/scroll-up
    nomis/dirtree/scroll-up-and-display
    (arg)
  :doc-string "Scroll up"
  :preamble ((interactive "p"))
  :body ((scroll-up arg)))

(nomis/dirtree/define-command/with-and-without-and-display
    nomis/dirtree/scroll-down
    nomis/dirtree/scroll-down-and-display
    (arg)
  :doc-string "Scroll down"
  :preamble ((interactive "p"))
  :body ((scroll-down arg)))

(nomis/dirtree/define-command/with-and-without-and-display
    nomis/dirtree/history-step-back
    nomis/dirtree/history-step-back-and-display
    ()
  :no-record-history? t
  :doc-string "Go back in history."
  :preamble ((interactive))
  :body ((if (nomis/dirtree/no-history?)
             (progn
               (beep)
               (message "There is no history [...] (Karl Popper)"))
           (nomis/dirtree/history-step-back-impl))))

(nomis/dirtree/define-command/with-and-without-and-display
    nomis/dirtree/history-step-forward
    nomis/dirtree/history-step-forward-and-display
    ()
  :no-record-history? t
  :doc-string "Go forward in history."
  :preamble ((interactive))
  :body ((if (nomis/dirtree/no-future?)
             (progn
               (beep)
               (message "There is no future [...] (Nelson Mandela)"))
           (nomis/dirtree/history-step-forward-impl))))

(defun nomis/dirtree/expand (arg)
  "Expand directory under point, showing previous expansion of subdirectories.
If <arg> is supplied, first collapse all and then expand to <arg> levels."
  (interactive "P")
  (cl-labels ((expand-recursively
               (widget n-times &optional force-expand-p)
               (when (and (nomis/dirtree/directory-widget? widget)
                          (>= n-times 1)
                          (or force-expand-p
                              (not (nomis/dirtree/directory-to-keep-collapsed?/fullname
                                    (nomis/dirtree/widget-file widget)))))
                 (nomis/dirtree/expand-node widget)
                 (mapc (lambda (x) (expand-recursively x (1- n-times)))
                       (nomis/dirtree/widget-children widget)))))
    (let* ((widget (nomis/dirtree/selected-widget/with-extras)))
      (if (nomis/dirtree/directory-widget? widget)
          (progn
            (unless (null arg)
              (nomis/dirtree/collapse-all))
            (expand-recursively widget
                                (or arg 1)
                                t))
        (progn
          (message "Not a directory, so can't expand.")
          (beep))))))

(defun nomis/dirtree/collapse ()
  "Collapse directory under point, retaining previous expansion of subdirectories."
  (interactive)
  (let* ((widget (nomis/dirtree/selected-widget/with-extras)))
    (if (nomis/dirtree/directory-widget? widget)
        (nomis/dirtree/collapse-node widget)
      (progn
        (message "Not a directory, so can't collapse.")
        (beep)))))

(defun nomis/dirtree/expand-all ()
  "Expand directory under point to show all subdirectories,
sub-subdirectories, etc."
  (interactive)
  (nomis/dirtree/expand 1000000))

(defun collapse-recursively (widget)
  ;; For some reason having this in a `labels` within
  ;; `nomis/dirtree/collapse-all` doesn't work -- undefined function.
  ;; Weird. Was ok in Emacs 24.2 but not in 24.3
  (mapc 'collapse-recursively
        (nomis/dirtree/widget-children widget))
  (nomis/dirtree/collapse-node widget))

(defun nomis/dirtree/collapse-all ()
  "Collapse directory under point and all of its subdirectories,
sub-subdirectories, etc, so that subsequent expansion shows only one level."
  (interactive)
  (let* ((widget (nomis/dirtree/selected-widget/with-extras)))
    (if (nomis/dirtree/directory-widget? widget)
        (collapse-recursively widget)
      (progn
        (message "Not a directory, so can't collapse.")
        (beep)))))

(defun nomis/dirtree/show-only-selection (arg)
  (interactive "P")
  (let* ((collapse-all-trees? arg)
         (filename (nomis/dirtree/selected-file)))
    (if collapse-all-trees?
        (nomis/dirtree/collapse-recursively-all-trees)
      (collapse-recursively (nomis/dirtree/root-widget-no-arg)))
    (nomis/dirtree/goto-filename filename)))

(defun nomis/dirtree/show-only-selection/collapse-other-trees ()
  (interactive)
  (nomis/dirtree/show-only-selection t))

(defun nomis/dirtree/isearch-forward ()
  (interactive)
  (isearch-forward)
  (nomis/dirtree/note-selection))

(defun nomis/dirtree/toggle-dirtree-dirs-at-top ()
  (interactive)
  (setq nomis/dirtree/dirs-at-top?
        (not nomis/dirtree/dirs-at-top?))
  (nomis/dirtree/refresh/internal))

(defun nomis/dirtree/show-widget-info (widget widget-with-extras?)
  (cl-labels ((emit-info
               ()
               (let* ((inhibit-message t))
                 (message "
 ======== %s info -- %s ========
 (car widget) = %S
 :nomis/root = %S
 :tag = %S
 :file = %S
 :open = %S
 :node = %S
 (line-end-position) = %S
 :from = %S
 :to = %S
 keys = %S
 children info (all)          = %S
 children info (no internals) = %S
"
                          (if widget-with-extras? "Widget-with-extras" "Widget")
                          (car widget)
                          (car widget)
                          (widget-get widget :nomis/root)
                          (widget-get widget :tag)
                          (nomis/dirtree/widget-file widget)
                          (widget-get widget :open)
                          (widget-get widget :node)
                          (line-end-position)
                          (plist-get (rest widget) :from)
                          (plist-get (rest widget) :to)
                          (loop for k in (rest widget) by 'cddr
                                collect k)
                          (-map #'car
                                (nomis/dirtree/widget-children/all widget))
                          (-map #'car
                                (nomis/dirtree/widget-children widget))))))
    (let* ((original-window (selected-window)))
      (unwind-protect
          (progn
            (switch-to-buffer-other-window "*Messages*")
            (emit-info))
        (select-window original-window)))))

(defun nomis/dirtree/show-selection-no-extras-info ()
  "Display some details of selection-no-extras.
Mostly for debugging purposes."
  (interactive)
  (let* ((widget (nomis/dirtree/selected-widget/no-extras)))
    (nomis/dirtree/show-widget-info widget nil)))

(defun nomis/dirtree/show-selection-with-extras-info ()
  "Display some details of selection-with-extras.
Mostly for debugging purposes."
  (interactive)
  (let* ((widget (nomis/dirtree/selected-widget/with-extras)))
    (nomis/dirtree/show-widget-info widget t)))

(defun nomis/dirtree/delete-tree/do-it ()
  (with-dirtree-buffer-if-it-exists
    (assert (tree-mode-root-linep))
    (nomis/dirtree/collapse-all) ; an easy way to remove watchers.
    (tree-mode-delete (tree-mode-tree-ap))))

(defun nomis/dirtree/delete-tree ()
  "Delete tree containing selection from the dirtree buffer."
  (interactive)
  (if (not (tree-mode-root-linep))
      (progn
        (message "The delete-tree command only works when at the root of a tree.")
        (nomis/beep))
    (when (yes-or-no-p "Delete current tree?")
      (nomis/dirtree/delete-tree/do-it))))

(define-key global-map (kbd "H-q d") 'nomis/dirtree/make-dirtree)
(define-key global-map (kbd "H-/")   'nomis/dirtree/goto-file/return-to-window)
(define-key global-map (kbd "H-M-/") 'nomis/dirtree/goto-file)

(cl-labels ((dk (k f)
                (define-key nomis/dirtree/mode-map k f)))

  (define-key widget-keymap (kbd "<RET>") nil)

  (dk (kbd "g")             'nomis/dirtree/refresh)

  (dk (kbd "a")             'nomis/dirtree/toggle-auto-refresh)

  (dk (kbd "<RET>")         'nomis/dirtree/display-file)
  (dk (kbd "C-<return>")    'nomis/dirtree/display-file)
  (dk (kbd "S-<return>")    'nomis/dirtree/display-file-and-goto-other-window)

  (dk (kbd "o")             'nomis/dirtree/open-in-default-app)

  (dk (kbd "<down>")        'nomis/dirtree/next-line)
  (dk (kbd "n")             'nomis/dirtree/next-line)
  (dk (kbd "C-<down>")      'nomis/dirtree/next-line-and-display)
  (dk (kbd "C-n")           'nomis/dirtree/next-line-and-display)

  (dk (kbd "<up>")          'nomis/dirtree/previous-line)
  (dk (kbd "p")             'nomis/dirtree/previous-line)
  (dk (kbd "C-<up>")        'nomis/dirtree/previous-line-and-display)
  (dk (kbd "C-p")           'nomis/dirtree/previous-line-and-display)

  (dk (kbd "<right>")       'nomis/dirtree/next-line-with-expansion)
  (dk (kbd "C-<right>")     'nomis/dirtree/next-line-with-expansion-and-display)

  (dk (kbd "<left>")        'nomis/dirtree/up-directory)
  (dk (kbd "u")             'nomis/dirtree/up-directory)
  (dk (kbd "C-<left>")      'nomis/dirtree/up-directory-and-display)
  (dk (kbd "C-S-u")         'nomis/dirtree/up-directory-and-display)

  (dk (kbd "j")             'nomis/dirtree/next-sib)
  (dk (kbd "C-j")           'nomis/dirtree/next-sib-and-display)

  (dk (kbd "k")             'nomis/dirtree/previous-sib)
  (dk (kbd "C-k")           'nomis/dirtree/previous-sib-and-display)

  (dk (kbd "r")             'nomis/dirtree/goto-root)
  (dk (kbd "C-S-r")         'nomis/dirtree/goto-root-and-display)

  (dk (kbd ",")             'nomis/dirtree/history-step-back)
  (dk (kbd "C-,")           'nomis/dirtree/history-step-back-and-display)
  (dk (kbd ".")             'nomis/dirtree/history-step-forward)
  (dk (kbd "C-.")           'nomis/dirtree/history-step-forward-and-display)

  (dk (kbd "M-<right>")     'nomis/dirtree/expand)
  (dk (kbd "M-<left>")      'nomis/dirtree/collapse)
  (dk (kbd "M-S-<right>")   'nomis/dirtree/expand-all)
  (dk (kbd "M-S-<left>")    'nomis/dirtree/collapse-all)

  (dk (kbd "<SPC>")         'nomis/dirtree/scroll-up)
  (dk (kbd "C-<SPC>")       'nomis/dirtree/scroll-up-and-display)

  (dk (kbd "<DEL>")         'nomis/dirtree/scroll-down)
  (dk (kbd "<backspace>")   'nomis/dirtree/scroll-down)
  (dk (kbd "C-<DEL>")       'nomis/dirtree/scroll-down-and-display)
  (dk (kbd "C-<backspace>") 'nomis/dirtree/scroll-down-and-display)

  (dk (kbd "H-1")           'nomis/dirtree/show-only-selection)
  (dk (kbd "H-!")           'nomis/dirtree/show-only-selection/collapse-other-trees)

  (dk (kbd "C-s")           'nomis/dirtree/isearch-forward)

  (dk (kbd "t")             'nomis/dirtree/toggle-dirtree-dirs-at-top)

  (dk (kbd "X")             'nomis/dirtree/clear-history)

  (dk (kbd "I")             'nomis/dirtree/show-selection-no-extras-info)
  (dk (kbd "i")             'nomis/dirtree/show-selection-with-extras-info)

  (dk (kbd "D")             'nomis/dirtree/delete-tree))

(provide 'nomis-dirtree)
