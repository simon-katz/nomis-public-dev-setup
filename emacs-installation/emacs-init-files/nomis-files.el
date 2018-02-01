;;;; Init stuff -- Files ---  -*- lexical-binding: t -*-

(require 'cl)

;;;; ___________________________________________________________________________

(defun nomis/dir-separator? (c) (member c '(?/ ?\\)))

(defun nomis/directory-no-slash (s)
  (replace-regexp-in-string "[/\\]$"
                            ""
                            s))

(defun nomis/filename->path (filename) ; FIXME Terminology -- a path here is something like ("/a/" "/a/b/" "/a/b/c")
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

(defun nomis/filename->path-from-a-root (filename root-filename) ; FIXME Terminology -- a path here is something like ("/a/b/c/" "/a/b/c/d/" "/a/b/c/d/e")
  (let* ((path (nomis/filename->path filename))
         (path (cons root-filename
                     (-drop-while (lambda (s)
                                    (not (s-starts-with? root-filename
                                                         s)))
                                  path))))
    path))

;;;; ___________________________________________________________________________

(provide 'nomis-files)
