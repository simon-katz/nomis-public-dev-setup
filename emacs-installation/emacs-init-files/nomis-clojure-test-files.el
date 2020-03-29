;;;; Init stuff -- nomis-clojure-test-files -*- lexical-binding: t -*-

;;;; ___________________________________________________________________________

(require 's)

;;;; ___________________________________________________________________________
;;;; Utils

(defun -nomis/ctf/vc-root-dir () ; Copy of `nomis/dirtree/vc-root-dir`
  (let* ((filename (ignore-errors (or (vc-root-dir)
                                      (magit-toplevel)))))
    (when filename
      (expand-file-name filename))))

(defvar -nomis/ctf/luminus-project-special-filename
  ".nomis--luminus-test-file-naming-conventions")

(defun -nomis/this-is-a-luminus-project?/declared ()
  (when-let ((root-dir (-nomis/ctf/vc-root-dir)))
    (file-exists-p (s-concat
                    root-dir
                    -nomis/ctf/luminus-project-special-filename))))

(defun -nomis/ctf/file-exists-for-ns-name?/needs-cider-running (ns-name)
  (let* ((file-name (cider-sync-request:ns-path ns-name)))
    (and file-name
         (not (equal file-name "")))))

;;;; ___________________________________________________________________________
;;;; Stuff for finding the test namespace

;;;; This can assume there's a CIDER connection -- it's for running tests.

(defun -nomis/ctf/standard/test-ns (ns-name)
  (let* ((suffix "-test"))
    (if (string-suffix-p suffix ns-name)
        ns-name
      (let* ((test-ns-name (concat ns-name suffix)))
        (when (-nomis/ctf/file-exists-for-ns-name?/needs-cider-running test-ns-name)
          test-ns-name)))))

(defun -nomis/ctf/luminus/test-ns (ns-name)
  (let* ((path (s-split "\\." ns-name))
         (test-path (if (equal (second path) "test")
                        path
                      (list* (first path) "test" (rest path))))
         (test-ns-name (s-join "." test-path)))
    (when (-nomis/ctf/file-exists-for-ns-name?/needs-cider-running test-ns-name)
      test-ns-name)))

(defun -nomis/ctf/test-ns (ns-name)
  (when ns-name
    (or (-nomis/ctf/standard/test-ns ns-name)
        (-nomis/ctf/luminus/test-ns ns-name))))

(setq cider-test-infer-test-ns
      '-nomis/ctf/test-ns)

;;;; ___________________________________________________________________________
;;;; Stuff to make `projectile-toggle-between-implementation-and-test` work

;;;; This has to work when there's no CIDER connection, so we are in the
;;;; world of files rather than namespaces.

;;;; TODO This needs further work for cljs files.

(defun -nomis/ctf/luminus/other-file-for-toggle-impl-test ()
  (when (-nomis/this-is-a-luminus-project?/declared)
    (let* ((file-name (buffer-file-name))
           (root-dir (let* ((root-dir-v1 (-nomis/ctf/vc-root-dir))
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
      (when (or src-file? test-file?)
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
          other-file)))))

(defun -nomis/ctf/luminus/toggle-impl-test ()
  (let* ((other-file (-nomis/ctf/luminus/other-file-for-toggle-impl-test)))
    (if (and other-file
             (or projectile-create-missing-test-files
                 (file-exists-p other-file)))
        (find-file other-file)
      (error "No such file: %s" other-file))))

(defun -nomis/toggle-between-implementation-and-test (normal-behaviour-fun)
  (if (-nomis/this-is-a-luminus-project?/declared)
      (-nomis/ctf/luminus/toggle-impl-test)
    (funcall normal-behaviour-fun)))

(advice-add
 'projectile-toggle-between-implementation-and-test
 :around
 (lambda (orig-fun &rest args)
   (-nomis/toggle-between-implementation-and-test
    (lambda () (apply orig-fun args))))
 '((name . nomis/hack-for-non-standard-test-file-naming)))

;;;; ___________________________________________________________________________

(provide 'nomis-clojure-test-files)
