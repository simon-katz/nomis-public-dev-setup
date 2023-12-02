;;; nomis-window-config.el --- Frame and window configurations -*- lexical-binding: t -*-

(progn) ; this stops `hs-hide-all` from hiding the next comment

;;;; _______________ Requires __________________________________________________

(require 'nomis-save-and-read-data)
(require 'treepy)
(require 'cl-format)

;;;; _______________ Customizable variables ____________________________________

(defvar nomis/wc/directory
  "~/.emacs-nomis-frame-window-config/")

(defvar nomis/wc/root-dir-for-searches nil)

;;;; _______________ Public functions etc ______________________________________

(defun nomis/wc/save (wc-name)
  (interactive (list (-nomis/wc/interactive-wc-name-stuff :save)))
  (make-directory nomis/wc/directory t)
  (nomis/save-to-file (-nomis/wc/wc-name->filename wc-name)
                      (window-state-get nil t)
                      :pretty? t)
  (message "Saved window config: %s" wc-name))

(defun nomis/wc/restore (wc-name)
  (interactive (list (-nomis/wc/interactive-wc-name-stuff :restore)))
  (let* ((filename (-nomis/wc/wc-name->filename wc-name))
         (window-state (nomis/read-from-file filename))
         (hacked-window-state (-nomis/wc/window-state/replace-unknown-buffers
                               window-state)))
    (window-state-put hacked-window-state
                      (frame-root-window))
    (message "Restored window config: %s"
             wc-name)))

(defun nomis/wc/search-for-file ()
  (interactive)
  (let* ((filename (-nomis/wc/proxy-buffer-name->filename (buffer-name)))
         (root-directory (read-directory-name
                          (format "Search for %s\nRoot of search: "
                                  filename)
                          nomis/wc/root-dir-for-searches
                          nil
                          t)))
    (find-name-dired root-directory
                     filename)))

(prog1 (define-prefix-command 'nomis/wc/keymap)
  (define-key nomis/wc/keymap (kbd "s") 'nomis/wc/save)
  (define-key nomis/wc/keymap (kbd "r") 'nomis/wc/restore)
  (define-key nomis/wc/keymap (kbd "/") 'nomis/wc/search-for-file))

;;;; _______________ Private things ____________________________________________

(defconst -nomis/wc/file-suffix
  ".window-config")

(defun -nomis/wc/wc-name->filename (wc-name)
  (concat nomis/wc/directory wc-name -nomis/wc/file-suffix))

(defun -nomis/wc/interactive-wc-name-stuff (save-or-restore)
  (let* ((wc-names (->> (directory-files nomis/wc/directory)
                        (-remove (lambda (filename)
                                   (member filename
                                           (list "." ".."))))
                        (-filter (lambda (filename)
                                   (s-ends-with? -nomis/wc/file-suffix
                                                 filename)))
                        (-map (lambda (filename)
                                (s-replace -nomis/wc/file-suffix
                                           ""
                                           filename))))))
    (when (and (null wc-names)
               (eq save-or-restore :restore))
      (error "No saved configurations"))
    (completing-read "Name: "
                     wc-names
                     nil
                     (cl-ecase save-or-restore
                       (:save nil)
                       (:restore t))
                     nil
                     'nomis/wc/wc-name-history
                     (cl-ecase save-or-restore
                       (:save "")
                       (:restore (first wc-names))))))

(defconst -nomis/wc/no-such-buffer-prefix "*NO-SUCH-BUFFER--")
(defconst -nomis/wc/no-such-buffer-suffix "*")

(defun -nomis/wc/buffer-name->proxy-buffer-name (buffer-name)
  (concat -nomis/wc/no-such-buffer-prefix
          buffer-name
          -nomis/wc/no-such-buffer-suffix))

(defun -nomis/wc/proxy-buffer-name->filename (proxy-buffer-name)
  (let* ((prefix -nomis/wc/no-such-buffer-prefix)
         (suffix -nomis/wc/no-such-buffer-suffix))
    (if (or (not (s-starts-with? prefix proxy-buffer-name))
            (not (s-ends-with? suffix proxy-buffer-name)))
        (progn
          (error "This buffer is not a NO-SUCH-BUFFER buffer")
          (nomis/msg/beep))
      (->> proxy-buffer-name
           (replace-regexp-in-string (concat "^" (regexp-quote prefix))
                                     "")
           (replace-regexp-in-string (concat (regexp-quote suffix) "$")
                                     "")
           file-name-nondirectory ; because `find-name-dired` needs just a filename
           ))))

(defun -nomis/wc/get-or-create-buffer-for-no-such-buffer (buffer-name)
  (let* ((proxy-buffer-name (-nomis/wc/buffer-name->proxy-buffer-name
                             buffer-name)))
    (or (get-buffer proxy-buffer-name)
        (let* ((buffer (generate-new-buffer proxy-buffer-name)))
          (with-current-buffer buffer
            (insert (format "NO SUCH BUFFER: %s\n"
                            buffer-name))
            (insert (format "To search for the file, use the command `nomis/wc/search-for-file`.\n"))
            (read-only-mode 1))
          buffer))))

(defun -nomis/wc/window-state/replace-unknown-buffers (window-state)
  (->> window-state
       (treepy-prewalk
        (lambda (form)
          (if (not (and (listp form)
                        (eq (first form) 'buffer)
                        (not (get-buffer (second form)))))
              form
            (let* ((buffer-name (second form))
                   (proxy-buffer-name (-nomis/wc/buffer-name->proxy-buffer-name
                                       buffer-name)))
              (-nomis/wc/get-or-create-buffer-for-no-such-buffer buffer-name)
              (-replace-at 1 proxy-buffer-name form)))))))

;;;; ___________________________________________________________________________
;;;; Restore just-closed frame

(defvar nomis/wc/just-closed-frame-info-list '())

(add-hook 'delete-frame-functions 'nomis/wc/note-deleted-frame)

(defun nomis/wc/note-deleted-frame (frame)
  (let ((info   (make-hash-table))
        (left   (frame-parameter frame 'left))
        (top    (frame-parameter frame 'top))
        (width  (frame-parameter frame 'width))
        (height (frame-parameter frame 'height))
        (state  (window-state-get (frame-root-window frame) t)))
    (puthash :left   left   info)
    (puthash :top    top    info)
    (puthash :width  width  info)
    (puthash :height height info)
    (puthash :state  state  info)
    (push info nomis/wc/just-closed-frame-info-list)))

(defun nomis/wc/restore-deleted-frame ()
  (interactive)
  (if (null nomis/wc/just-closed-frame-info-list)
      (user-error "There is no deleted frame to restore")
    (let* ((frame  (make-frame))
           (info (pop nomis/wc/just-closed-frame-info-list))
           (left   (gethash :left   info))
           (top    (gethash :top    info))
           (width  (gethash :width  info))
           (height (gethash :height info))
           (state  (gethash :state  info)))
      (set-frame-parameter frame 'left   left)
      (set-frame-parameter frame 'top    top)
      (set-frame-parameter frame 'width  width)
      (set-frame-parameter frame 'height height)
      (window-state-put (-nomis/wc/window-state/replace-unknown-buffers state)
                        (frame-root-window frame)))))

;;;; ___________________________________________________________________________

(provide 'nomis-window-config)
