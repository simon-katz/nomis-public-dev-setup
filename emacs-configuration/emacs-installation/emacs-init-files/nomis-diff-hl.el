;;; nomis-diff-hl.el --- diff-hl setup -*- lexical-binding: t; -*-

;;;; ___________________________________________________________________________
;;;; ---- diff-hl-mode ----

(require 'diff-hl)

(add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

(global-diff-hl-mode)
(diff-hl-flydiff-mode)
(diff-hl-margin-mode) ; (unless (window-system) (diff-hl-margin-mode))
(global-diff-hl-show-hunk-mouse-mode)

(add-hook 'dired-mode-hook 'diff-hl-dired-mode-unless-remote)

(setq diff-hl-show-hunk-inline-popup-hide-hunk t)
(setq diff-hl-show-hunk-inline-popup-smart-lines nil)

(setq diff-hl-show-hunk-function
      (cl-case :default
        (:default 'diff-hl-show-hunk-inline-popup)
        (:other   'diff-hl-show-hunk-posframe)))

(defun nomis/turn-off-diff-hl-for-large-files ()
  (let* ((size (buffer-size)))
    (when (> size (* 1024 1024 0.5))
      (diff-hl-mode -1))))

(add-hook 'find-file-hook 'nomis/turn-off-diff-hl-for-large-files)

(with-eval-after-load 'diff-hl-inline-popup

  (defconst nomis/diff-hl/-scroll-indicator-for-header
    " ▲ ▲ ▲ ▲")

  (defconst nomis/diff-hl/-scroll-indicator-for-footer
    " ▼ ▼ ▼ ▼")

  (defconst nomis/diff-hl/-no-scroll-indicator-for-header
    (make-string (length nomis/diff-hl/-scroll-indicator-for-header)
                 ?\s))

  (defconst nomis/diff-hl/-no-scroll-indicator-for-footer
    (make-string (length nomis/diff-hl/-scroll-indicator-for-footer)
                 ?\s))

  ;; TODO: Define faces for this, different for light and dark themes.
  (defun nomis/diff-hl/-header-footer-colour ()
    (if (nomis/dark-background-mode?)
        "orange"
      "blue"))

  (defun nomis/diff-hl/-header-footer-style/normal ()
    (list :underline  t
          :overline   t
          :foreground (nomis/diff-hl/-header-footer-colour)))

  (defun nomis/diff-hl/-header-style/showing-scroll-arrows ()
    (list :overline   t
          :foreground (nomis/diff-hl/-header-footer-colour)))

  (defun nomis/diff-hl/-footer-style/showing-scroll-arrows ()
    (list :underline t
          :foreground (nomis/diff-hl/-header-footer-colour)))

  (cond
   ((member (pkg-info-package-version 'diff-hl)
            '((20210909 207)
              (20211105 145)
              (20230423 1837)))

    (defun diff-hl-inline-popup--compute-header (width &optional header)
      ;; The original `diff-hl-inline-popup--compute-header` is in
      ;; `diff-hl-inline-popup`.
      "Compute the header of the popup, with some WIDTH, and some optional HEADER text."
      ;; :nomis-hack
      (let* ((show-scroll-arrows?
              (not (eql diff-hl-inline-popup--current-index 0)))
             (scroll-indicator (if show-scroll-arrows?
                                   nomis/diff-hl/-scroll-indicator-for-header
                                 nomis/diff-hl/-no-scroll-indicator-for-header))
             (header (or header ""))
             (new-width (- width (length header) (length scroll-indicator)))
             (header (if (< new-width 0) "" header))
             (new-width (- width (length header) (length scroll-indicator)))
             (line
              (concat
               (propertize
                (concat (diff-hl-inline-popup--separator new-width)
                        header)
                'face (nomis/diff-hl/-header-footer-style/normal))
               (propertize
                scroll-indicator
                'face (if show-scroll-arrows?
                          (nomis/diff-hl/-header-style/showing-scroll-arrows)
                        (nomis/diff-hl/-header-footer-style/normal))))))
        (concat line "\n") ))

    (defun diff-hl-inline-popup--compute-footer (width &optional footer)
      ;; The original `diff-hl-inline-popup--compute-footer` is in
      ;; `diff-hl-inline-popup`.
      "Compute the header of the popup, with some WIDTH, and some optional FOOTER text."
      ;; :nomis-hack
      (let* ((show-scroll-arrows?
              (not (>= diff-hl-inline-popup--current-index
                       (- (length diff-hl-inline-popup--current-lines)
                          diff-hl-inline-popup--height))))
             (scroll-indicator (if show-scroll-arrows?
                                   nomis/diff-hl/-scroll-indicator-for-footer
                                 nomis/diff-hl/-no-scroll-indicator-for-footer))
             (footer (or footer ""))
             (new-width (- width (length footer) (length scroll-indicator)))
             (footer (if (< new-width 0) "" footer))
             (new-width (- width (length footer) (length scroll-indicator)))
             (blank-line (if (display-graphic-p)
                             ""
                           (concat "\n" (propertize (diff-hl-inline-popup--separator width)
                                                    'face '(:underline t)))))
             (line (concat
                    (propertize
                     (concat (diff-hl-inline-popup--separator new-width)
                             footer)
                     'face (nomis/diff-hl/-header-footer-style/normal))
                    (propertize
                     scroll-indicator
                     'face (if show-scroll-arrows?
                               (nomis/diff-hl/-footer-style/showing-scroll-arrows)
                             (nomis/diff-hl/-header-footer-style/normal))))))
        (concat blank-line "\n" line))))

   (t
    (message-box
     "You need to fix your hacks for this version of `diff-hl-inline-popup`."))))

;;;; ___________________________________________________________________________

(provide 'nomis-diff-hl)
