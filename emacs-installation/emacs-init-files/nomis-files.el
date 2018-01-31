;;;; Init stuff -- Files.

(require 'cl)

;;;; ___________________________________________________________________________

(defun nomis/dir-separator? (c) (member c '(?/ ?\\)))

(defun nomis/directory-no-slash (s)
  (replace-regexp-in-string "[/\\]$"
                            ""
                            s))

(defun nomis/filename->path (filename)
  (let* ((filename-as-list (string-to-list filename))
         (slash-positions (nomis/positions #'nomis/dir-separator?
                                           filename-as-list))
         (directory? (-> filename-as-list
                         last ; O(n)
                         first
                         nomis/dir-separator?))
         (substring-positions (if directory?
                                  slash-positions
                                (append slash-positions ; O(n)
                                        (list (1- (length filename)))))))
    (cl-loop for pos in substring-positions
             collect (substring filename 0 (1+ pos)))))

;;;; ___________________________________________________________________________

(provide 'nomis-files)
