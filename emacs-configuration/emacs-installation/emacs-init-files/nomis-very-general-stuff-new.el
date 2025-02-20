;;;; Init stuff -- Very general stuff (new).
;;;; - The plan is to move stuff to here from "nomis-very-general-stuff", but
;;;;   I don't want to do that right now (2017-09-12) in case I break things.
;;;;   And I need what's here earlier in the init setup.

;;;; ___________________________________________________________________________

(setq help-window-select t)

;;;; ___________________________________________________________________________

(cl-defmacro nomis/add-to-list-local (list-var new-item &optional append?)
  "Like `add-to-list`, but:
- does not evaluate the first arg;
- uses `setq-local`.
Useful in .dir-locals.el, where `add-to-list` would be wrong."
  (cl-assert (and (listp list-var)
                  (eql 'quote (first list-var))
                  (symbolp (second list-var))
                  (= 2 (length list-var)))
             t
             "First arg must be a quoted symbol")
  (let ((sym (second list-var)))
    (mmt-once-only (new-item
                    append?)
      `(let ((old-value ,sym))
         (when (not (member ,new-item old-value))
           (setq-local ,sym
                       (if ,append?
                           (append old-value (list ,new-item))
                         (cons ,new-item old-value))))))))

;; Test:

(progn
  (setq nomis/xyz-123 '())
  (nomis/add-to-list-local 'nomis/xyz-123 "y")
  (nomis/add-to-list-local 'nomis/xyz-123 "x")
  (nomis/add-to-list-local 'nomis/xyz-123 "z" t)
  nomis/xyz-123)

;;;; ___________________________________________________________________________

(defun nomis/-add-to-colon-separated-string* (old-value new-item append? preceding-colon?)
  (cl-assert nil nil "nomis/-add-to-colon-separated-string* -- NO LONGER USED")
  (cl-assert (or (null old-value)
                 (stringp old-value)))
  (cl-assert (stringp new-item))
  (let* ((old-value (cond ((null old-value)
                           "")
                          ((s-starts-with? ":" old-value)
                           (s-chop-left 1 old-value))
                          (t
                           old-value))))
    (let* ((old-items (if (equal old-value "") '() (s-split ":" old-value)))
           (new-items (cond ((member new-item old-items)
                             old-items)
                            (append?
                             (append old-items (list new-item)))
                            (t
                             (cons new-item old-items))))
           (string (s-join ":" new-items)))
      (if preceding-colon?
          (s-concat ":" string)
        string))))

(defmacro nomis/add-to-colon-separated-string-local (string-var
                                                     new-item
                                                     &optional
                                                     append?
                                                     preceding-colon?)
  (cl-assert (and (listp string-var)
                  (eql 'quote (first string-var))
                  (symbolp (second string-var))
                  (= 2 (length string-var)))
             t
             "First arg must be a quoted symbol")
  (let* ((sym (second string-var)))
    `(let* ((old-value  (if (boundp ',sym) ,sym "")))
       (setq-local ,sym (nomis/-add-to-colon-separated-string* old-value
                                                               ,new-item
                                                               ,append?
                                                               ,preceding-colon?)))))

;; Tests:

;; (progn
;;   (makunbound 'nomis/xyz-123)
;;   ;; (setq nomis/xyz-123 "")
;;   (nomis/add-to-colon-separated-string-local 'nomis/xyz-123 "y")
;;   (nomis/add-to-colon-separated-string-local 'nomis/xyz-123 "x")
;;   (nomis/add-to-colon-separated-string-local 'nomis/xyz-123 "z" t)
;;   nomis/xyz-123)

;; (progn
;;   (makunbound 'nomis/xyz-123)
;;   ;; (setq nomis/xyz-123 "")
;;   (nomis/add-to-colon-separated-string-local 'nomis/xyz-123 "y" nil t)
;;   (nomis/add-to-colon-separated-string-local 'nomis/xyz-123 "x" nil t)
;;   (nomis/add-to-colon-separated-string-local 'nomis/xyz-123 "z" t   t)
;;   nomis/xyz-123)

;;;; ___________________________________________________________________________

(cl-defmacro nomis/with-temporary-invisible-changes (() &rest forms)
  ;; Copied from https://www.emacswiki.org/emacs/UndoCommands, and changed.
  "Executes FORMS with a temporary buffer-undo-list, undoing on return.
The changes you make within FORMS are undone before returning.
But more importantly, the buffer's buffer-undo-list is not affected.
This allows you to temporarily modify read-only buffers too."
  (declare (indent 1))
  `(let* ((buffer-undo-list)
          (modified (buffer-modified-p))
          (inhibit-read-only t))
     (save-excursion
       (unwind-protect
           (progn ,@forms)
         (primitive-undo (length buffer-undo-list) buffer-undo-list)
         (set-buffer-modified-p modified)))
     nil))

;;;; ___________________________________________________________________________

(defun nomis/with-cleanup-on-non-local-exit/fun (f cleanup-f)
  (let* ((non-local-exit? t))
    (unwind-protect
        (prog1 (funcall f)
          (setq non-local-exit? nil))
      (when non-local-exit?
        (funcall cleanup-f)))))

(defmacro nomis/with-cleanup-on-non-local-exit (bodyform &rest cleanup-forms)
  (declare (indent 1))
  `(nomis/with-cleanup-on-non-local-exit/fun (lambda () ,bodyform)
                                             (lambda () ,@cleanup-forms)))

;;;; ___________________________________________________________________________

(progn
  ;; These are useful when using `visual-line-mode`.
  (define-key global-map (kbd "C-S-a") 'beginning-of-line)
  (define-key global-map (kbd "C-S-e") 'end-of-line))

;;;; ___________________________________________________________________________

(define-key global-map (kbd "H-0") 'digit-argument)
(define-key global-map (kbd "H-1") 'digit-argument)
(define-key global-map (kbd "H-2") 'digit-argument)
(define-key global-map (kbd "H-3") 'digit-argument)
(define-key global-map (kbd "H-4") 'digit-argument)
(define-key global-map (kbd "H-5") 'digit-argument)
(define-key global-map (kbd "H-6") 'digit-argument)
(define-key global-map (kbd "H-7") 'digit-argument)
(define-key global-map (kbd "H-8") 'digit-argument)
(define-key global-map (kbd "H-9") 'digit-argument)

;;;; ___________________________________________________________________________

;;;; I have lots of H-<something> key bindings. This lets me recenter without
;;;; much hand movement when using those commands. (C-l is the default.)
(define-key global-map (kbd "H-l") 'recenter-top-bottom)

;;;; ___________________________________________________________________________

(remove-hook 'xref-after-jump-hook
             'recenter)

;;;; ___________________________________________________________________________

(global-set-key (kbd "C-c C-4") 'toggle-truncate-lines)

;;;; ___________________________________________________________________________

(provide 'nomis-very-general-stuff-new)
