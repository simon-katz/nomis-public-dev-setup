;;;; Init stuff -- nomis-clojure-luminus -*- lexical-binding: t -*-

;;;; ___________________________________________________________________________

(require 's)

;;;; ___________________________________________________________________________

(defun nomis/clojure-luminus/vc-root-dir () ; Copy of `nomis/dirtree/vc-root-dir`
  (let* ((filename (ignore-errors (or (vc-root-dir)
                                      (magit-toplevel)))))
    (when filename
      (expand-file-name filename))))

(defun nomis/this-is-a-luminus-project? ()
  (when-let ((root-dir (nomis/clojure-luminus/vc-root-dir)))
    (file-exists-p (s-concat root-dir
                             ".nomis--luminus-test-file-naming-conventions"))))

(defun nomis/cider-test-infer-test-ns/for-annoying-luminus (ns-name)
  (let* ((path (s-split "\\." ns-name))
         (test-path (if (equal (second path) "test")
                        path
                      (list* (first path) "test" (rest path)))))
    (s-join "." test-path)))

(defun nomis/cider-test-default-test-ns (ns-name)
  (let* ((f (if (nomis/this-is-a-luminus-project?)
                #'nomis/cider-test-infer-test-ns/for-annoying-luminus
              #'cider-test-default-test-ns-fn)))
    (funcall f ns-name)))

(setq cider-test-infer-test-ns
      'nomis/cider-test-default-test-ns)

(defun nomis/luminus/toggle-between-implementation-and-test ()
  (let* ((file-name (buffer-file-name))
         (root-dir (let* ((root-dir-v1 (nomis/clojure-luminus/vc-root-dir))
                          (root-dir-v2 (file-truename root-dir-v1)))
                     (cond ((s-starts-with? root-dir-v1 file-name)
                            root-dir-v1)
                           ((s-starts-with? root-dir-v2 file-name)
                            root-dir-v2)
                           (t
                            (error "Could not find project root dir for %s"
                                   file-name)))))
         (file-name-rel-to-root (s-replace root-dir "" file-name))
         (src-file?  (s-starts-with? "src/"  file-name-rel-to-root))
         (test-file? (s-starts-with? "test/" file-name-rel-to-root)))
    (if (not (or src-file? test-file?))
        (error "This file does not appear to be a src file or a test file")
      (let* ((file-name-starting-at-project-name
              (s-replace (if src-file? "src/clj/" "test/clj/")
                         ""
                         file-name-rel-to-root))
             (project-name-as-dir
              (first (s-split-up-to "/"
                                    file-name-starting-at-project-name
                                    1)))
             (src-path-prefix  (s-concat root-dir
                                         "src/clj/"
                                         project-name-as-dir
                                         "/"))
             (test-path-prefix (s-concat root-dir
                                         "test/clj/"
                                         project-name-as-dir
                                         "/test/"))
             (other-file (s-replace (if src-file? src-path-prefix test-path-prefix)
                                    (if src-file? test-path-prefix src-path-prefix)
                                    file-name)))
        (message
         "nomis projectile-toggle-between-implementation-and-test other-file = %s"
         other-file)
        (if (and other-file
                 (or projectile-create-missing-test-files
                     (file-exists-p other-file)))
            (find-file other-file)
          (progn
            (message "No such file: %s" other-file)
            (nomis/msg/beep)))))))

(advice-add
 'projectile-toggle-between-implementation-and-test
 :around
 (lambda (orig-fun &rest args)
   (if (nomis/this-is-a-luminus-project?)
       (nomis/luminus/toggle-between-implementation-and-test)
     (apply orig-fun args)))
 '((name . nomis/hack-for-non-standard-test-file-naming)))

;;;; ___________________________________________________________________________

(provide 'nomis-clojure-luminus)
