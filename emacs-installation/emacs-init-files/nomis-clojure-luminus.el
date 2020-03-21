;;;; Init stuff -- nomis-clojure-luminus -*- lexical-binding: t -*-

;;;; ___________________________________________________________________________

(defvar nomis/project-name-as-dir-for-annnoying-luminus nil)

(defun nomis/project-type ()
  (if nomis/project-name-as-dir-for-annnoying-luminus
      :annnoying-luminus
    :normal))

(defun nomis/cider-test-infer-test-ns/for-annoying-luminus (ns-name)
  (let* ((path (s-split "\\." ns-name))
         (test-path (if (equal (second path) "test")
                        path
                      (list* (first path) "test" (rest path)))))
    (s-join "." test-path)))

(defun nomis/cider-test-default-test-ns (ns-name)
  (let* ((f (ecase (nomis/project-type)
              (:normal
               #'cider-test-default-test-ns-fn)
              (:annnoying-luminus
               #'nomis/cider-test-infer-test-ns/for-annoying-luminus))))
    (funcall f ns-name)))

(setq cider-test-infer-test-ns
      'nomis/cider-test-default-test-ns)

(advice-add
 'projectile-toggle-between-implementation-and-test
 :around
 (lambda (orig-fun &rest args)
   (ecase (nomis/project-type)
     (:normal
      (apply orig-fun args))
     (:annnoying-luminus
      (let* ((project-name-as-dir
              nomis/project-name-as-dir-for-annnoying-luminus)
             (src-path-section  (s-concat "/" project-name-as-dir "/"))
             (test-path-section (s-concat "/" project-name-as-dir "/test/"))
             (file-name (buffer-file-name))
             (src-file? (cl-search "/src/" file-name))
             (other-file (cond
                          (src-file?
                           (destructuring-bind (prefix suffix)
                               (s-split-up-to "/src/"
                                              file-name
                                              1)
                             (s-concat prefix
                                       "/test/"
                                       (->> suffix
                                            (s-replace src-path-section
                                                       test-path-section)))))
                          ((s-contains? "/test/" file-name)
                           (destructuring-bind (prefix suffix)
                               (s-split-up-to "/test/"
                                              file-name
                                              1)
                             (s-concat prefix
                                       "/src/"
                                       (->> suffix
                                            (s-replace test-path-section
                                                       src-path-section)))))
                          (t
                           nil))))
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
 '((name . nomis/hack-for-non-standard-test-file-naming)))

;;;; ___________________________________________________________________________

(provide 'nomis-clojure-luminus)
