
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)

(defun load-file-relative-to-this-file (file-name)
  (let* ((directory (file-name-directory
                     (file-truename
                      (or load-file-name
                          (buffer-file-name))))))
    (load (concat directory file-name))))

(load-file-relative-to-this-file "dot-emacs")
