;;;; Init stuff -- Very general stuff (new).
;;;; - The plan is to move stuff to here from "nomis-very-general-stuff", but
;;;;   I don't want to do that right now (2017-09-12) in case I break things.
;;;;   And I need what's here earlier in the init setup.

;;;; ___________________________________________________________________________

(setq help-window-select t)

;;;; ___________________________________________________________________________

(cl-defmacro nomis/add-to-list-local (list-var element &optional append?)
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
    `(let ((vs ,sym)
           (v ,element)
           (append? ,append?))
       (when (not (member v vs))
         (setq-local ,sym
                     (if append?
                         (append vs (list v))
                       (cons v vs)))))))

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

(provide 'nomis-very-general-stuff-new)
