;;;; nomis-electric-clojure.el --- Minor mode for Electric Clojure ---  -*- lexical-binding: t -*-

;;;; Inspired by
;;;; https://gitlab.com/xificurC/hf-electric.el/-/blob/master/hf-electric.el

;;;; ___________________________________________________________________________
;;;; ---- The main functionality ----

(require 'dash)
(require 'nomis-sexp-utils)

(defvar nomis/ec-highlight-initial-whitespace? nil)

(defvar nomis/ec-give-feedback-flash? nil)

(defface nomis/ec-client-face
  `((((background dark)) ,(list :background "DarkGreen"))
    (t ,(list :background "DarkSeaGreen1")))
  "Face for Electric Clojure client code.")

(defface nomis/ec-server-face
  `((((background dark)) ,(list :background "IndianRed4"))
    (t ,(list :background "#ffc5c5")))
  "Face for Electric Clojure server code.")

(defface nomis/ec-either-client-or-server-face
  `((t ,(list :background "what-should-this-be?" ; TODO: What should this be?
              )))
  "Face for Electric Clojure either client or server code.")

(defface nomis/ec-flash-update-region-face-1
  `((t ,(list :background "red3")))
  "Face for Electric Clojure flashing of provided region.")

(defface nomis/ec-flash-update-region-face-2
  `((t ,(list :background "yellow")))
  "Face for Electric Clojure flashing of extended region.")

(defun nomis/ec-not-a-real-paren (p)
  (let* ((parse-state (syntax-ppss p)))
    ;;   string?             comment?            escaped?
    (or (nth 3 parse-state) (nth 4 parse-state) (eq (char-before p)?\\))))

(defun nomis/ec-make-overlay (nesting-level face start end)
  (let* ((ov (make-overlay start end nil t nil)))
    (overlay-put ov 'category 'nomis/ec-overlay)
    (overlay-put ov 'face face)
    (overlay-put ov 'evaporate t)
    (unless nomis/ec-highlight-initial-whitespace?
      ;; We have multiple overlays in the same place, so we need to
      ;; specify their priority.
      (overlay-put ov 'priority (cons nil nesting-level)))
    ov))

(defun nomis/ec-apply-overlays (client-or-server start)
  (let* ((face (case client-or-server
                 (:client 'nomis/ec-client-face)
                 (:server 'nomis/ec-server-face)
                 (:either 'nomis/ec-either-client-or-server-face)))
         (nesting-level (nomis/nesting-level))
         (end (save-excursion (goto-char start) (forward-sexp) (point))))
    (if nomis/ec-highlight-initial-whitespace?
        (nomis/ec-make-overlay nesting-level face start end)
      (save-excursion
        (goto-char start)
        (while (< (point) end)
          (let* ((start-2 (point))
                 (end-2 (min end
                             (progn (end-of-line) (1+ (point))))))
            (nomis/ec-make-overlay nesting-level face start-2 end-2)
            (unless (eobp) (forward-char))
            (when (bolp)
              (back-to-indentation))))))))

(defun nomis/ec-feedback-flash (start end start-2 end-2)
  (when nomis/ec-give-feedback-flash?
    (let* ((flash-overlay-1
            (let* ((ov (make-overlay start end nil t nil)))
              (overlay-put ov 'category 'nomis/ec-overlay)
              (overlay-put ov 'face 'nomis/ec-flash-update-region-face-1)
              (overlay-put ov 'evaporate t)
              (overlay-put ov 'priority 999999)
              ov))
           (flash-overlay-2
            (let* ((ov (make-overlay start-2 end-2 nil t nil)))
              (overlay-put ov 'category 'nomis/ec-overlay)
              (overlay-put ov 'face 'nomis/ec-flash-update-region-face-2)
              (overlay-put ov 'evaporate t)
              (overlay-put ov 'priority 999999)
              ov)))
      (run-at-time 0.2
                   nil
                   (lambda ()
                     (delete-overlay flash-overlay-1)
                     (delete-overlay flash-overlay-2))))))

(defun nomis/ec-overlay-region (start end)
  (save-excursion
    (goto-char start)
    (unless (nomis/at-top-level?) (beginning-of-defun))
    (let* ((start-2 (point))
           (end-2 (save-excursion (goto-char end)
                                  (unless (nomis/at-top-level?) (end-of-defun))
                                  (point))))
      (remove-overlays start-2 end-2 'category 'nomis/ec-overlay)
      (while (and (< (point) end-2)
                  (re-search-forward "(" end-2 'noerror))
        (backward-char)
        (unless (nomis/ec-not-a-real-paren (point))
          (when-let ((client-or-server
                      (cond ((looking-at "(e/client\\_>") :client)
                            ((looking-at "(e/server\\_>") :server)
                            ((looking-at "(e/fn\\_>")     :either))))
            (nomis/ec-apply-overlays client-or-server (point))))
        (forward-char))
      (nomis/ec-feedback-flash start end start-2 end-2)
      `(jit-lock-bounds ,start-2 . ,end-2))))

(define-minor-mode nomis-electric-clojure-mode
  "Highlight Electric Clojure client code regions and server code regions."
  :init-value nil
  (if nomis-electric-clojure-mode
      (jit-lock-register 'nomis/ec-overlay-region t)
    (progn
      (jit-lock-unregister 'nomis/ec-overlay-region)
      (remove-overlays nil nil 'category 'nomis/ec-overlay))))

;;;; ___________________________________________________________________________

(defun nomis/ec-toggle-highlight-initial-whitespace? ()
  (interactive)
  (if (not nomis-electric-clojure-mode)
      (nomis-electric-clojure-mode)
    (progn
      (setq nomis/ec-highlight-initial-whitespace?
            (not nomis/ec-highlight-initial-whitespace?))
      (nomis/ec-overlay-region (point-min) (point-max)))))

;;;; ___________________________________________________________________________
;;;; ---- nomis/ec-report-overlays ----

(defun nomis/ec-report-overlays ()
  (interactive)
  (let* ((ovs (->> (overlays-in (point-min) (point-max))
                   (-filter (lambda (ov)
                              (eq 'nomis/ec-overlay
                                  (overlay-get ov 'category)))))))
    (message "----------------")
    (dolist (ov ovs)
      (message "%s" ov))))

;;;; ___________________________________________________________________________
;;;; ---- Key bindings ----

(define-key clojure-mode-map (kbd "M-E") 'nomis-electric-clojure-mode)

(define-key clojure-mode-map (kbd "C-M-E")
  'nomis/ec-toggle-highlight-initial-whitespace?)

;;;; ___________________________________________________________________________

(provide 'nomis-electric-clojure)
