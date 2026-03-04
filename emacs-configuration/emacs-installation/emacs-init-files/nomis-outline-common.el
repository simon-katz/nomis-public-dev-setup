;;; nomis-outline-common.el --- Outline wrappers we can use for outline and org -*- lexical-binding: t; -*-

;;; Code:

;;;; Requires

(require 'cl-lib)
(require 'cl-format)
(require 'dash)
(require 'nomis-popup)
(require 'outline)

;;;; Misc

(defun nomis/outline/c/last-command ()
  (or (bound-and-true-p *nomis/smex/last-command*)
      last-command))

(defun -nomis/outline/c/ordinal (n)
  (cl-format nil "~a~a"
             n
             (let ((x (cl-format nil "~:r" n)))
               (cl-subseq x (- (length x) 2)))))

;;;; Simple outline wrappers

(defun nomis/outline/c/on-heading? ()
  (outline-on-heading-p t))

(defun -nomis/outline/c/on-visible-heading? ()
  (outline-on-heading-p))

(defun nomis/outline/c/back-to-heading? ()
  (outline-back-to-heading t))

(defun -nomis/outline/c/back-to-visible-heading? ()
  (outline-back-to-heading))

(defun nomis/outline/c/up-heading (n)
  (outline-up-heading n t))

(defun -nomis/outline/c/up-visible-heading (n)
  (outline-up-heading n))

(defun -nomis/outline/c/at-beginning-of-heading? ()
  (and (bolp)
       (nomis/outline/c/on-heading?)))

(defun -nomis/outline/c/on-top-level-heading? ()
  "Are we on a top-level heading?"
  ;; `(outline-level)` and `(funcall outline-level)` return weird numbers in
  ;; some modes. This, we hope, is bulletproof.
  (save-excursion
    (when (nomis/outline/c/on-heading?)
      (let* ((olevel (funcall outline-level)))
        (ignore-errors
          ;; `ignore-errors` is needed when before first heading.
          (nomis/outline/c/up-heading 1))
        (or (not (nomis/outline/c/on-heading?)) ; blank lines at top of file?
            (= olevel (funcall outline-level)))))))

(defun -nomis/outline/c/top-level-level ()
  (cl-assert (nomis/outline/c/on-heading?))
  (save-excursion
    (goto-char (point-min))
    (unless (nomis/outline/c/on-heading?) (outline-next-heading))
    (funcall outline-level)))

(defun -nomis/outline/c/ensure-heading-shown ()
  (when (outline-invisible-p)
    ;; Is there a simpler way to show the heading but not the body?
    (outline-show-entry)
    (outline-hide-entry)))

(defun -nomis/outline/c/show-children ()
  ;; The `1` is important; otherwise we get bodies of children.
  (outline-show-children 1))

(defun -nomis/outline/c/prev-or-next (direction)
  (cl-ecase direction
    (:backward (outline-previous-heading))
    (:forward (outline-next-heading))))

;;;; Previous/next helpers

(defun -nomis/outline/c/prev-next-same-level (direction sibling-or-peer)
  (let* ((opoint (point))
         (level (funcall outline-level))
         (npoint  (save-excursion
                    ;; The logic here is a copy-and-edit of
                    ;; `outline-get-last-sibling` and
                    ;; `outline-get-next-sibling`.
                    (-nomis/outline/c/prev-or-next direction)
                    (when (cl-ecase direction
                            (:backward (and (/= (point) opoint)
                                            (outline-on-heading-p t)))
                            (:forward t))
                      (while (and (cl-ecase direction
                                    (:backward t)
                                    (:forward (not (eobp))))
                                  (funcall (cl-ecase sibling-or-peer
                                             (:sibling #'>)
                                             (:peer #'/=))
                                           (funcall outline-level)
                                           level)
                                  (cl-ecase direction
                                    (:backward (not (bobp)))
                                    (:forward t)))
                        (-nomis/outline/c/prev-or-next direction))
                      (if (or (cl-ecase direction
                                (:backward nil)
                                (:forward (eobp)))
                              (< (funcall outline-level) level))
                          nil
                        (cl-assert (= level (funcall outline-level)))
                        (point))))))
    (when npoint
      (goto-char npoint))))

(defun -nomis/outline/c/prev-or-next-heading-pos (start
                                                  direction
                                                  kind)
  (when start
    (save-excursion
      (goto-char start)
      (let* ((boh? (-nomis/outline/c/at-beginning-of-heading?)))
        (if (and (eq direction :backward)
                 (not boh?))
            (progn
              (nomis/outline/c/back-to-heading?)
              (point))
          (when (and (eq direction :forward)
                     (not boh?))
            (nomis/outline/c/back-to-heading?))
          (cl-ecase kind
            (:any-level
             (-nomis/outline/c/prev-or-next direction))
            (:sibling
             (-nomis/outline/c/prev-next-same-level direction :sibling))
            (:peer
             (-nomis/outline/c/prev-next-same-level direction :peer)))
          (when (and (/= (point) start)
                     (nomis/outline/c/on-heading?))
            ;; ^^ Check of `(nomis/outline/c/on-heading?)` needed because
            ;;    `-nomis/outline/c/prev-or-next` goes to BOF or EOF when there's
            ;;    no prev/next heading.
            (point)))))))

(defun nomis/outline/c/prev-or-next-heading (n
                                             direction
                                             kind
                                             &optional
                                             ;; TODO: I don't like this.
                                             ;;       Refactor and make outline
                                             ;;       and org callers consistent.
                                             no-msg?)
  "Go to the N'th-next heading of kind KIND in direction DIRECTION.
If such a heading exists, return `t`.
If no such heading exists, return `nil', leave point unchanged and
display a popup message.
KIND is one of `:sibling`, `:peer` and `:any-level`.
DIRECTION is one or `:forward` and `:backward`."
  (let* ((pos (->> (-iterate (lambda (start)
                               (-nomis/outline/c/prev-or-next-heading-pos
                                start
                                direction
                                kind))
                             (point)
                             (1+ n))
                   cl-rest
                   (-drop (1- n))
                   cl-first)))
    (if pos
        (progn
          (goto-char pos)
          t)
      (unless no-msg?
        (let* ((direction-word (cl-ecase direction
                                 (:backward "previous")
                                 (:forward "next")))
               (kind-word (cl-ecase kind
                            (:any-level "heading")
                            (:sibling "sibling")
                            (:peer "same-level"))))
          (nomis/popup/error-message
           "No %s%s %s"
           (if (= n 1) "" (concat (-nomis/outline/c/ordinal n)
                                  "-"))
           direction-word
           kind-word)))
      nil)))

;;; End

(provide 'nomis-outline-common)
