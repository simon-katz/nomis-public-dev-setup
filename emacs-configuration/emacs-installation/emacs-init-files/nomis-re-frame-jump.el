;;; nomis-re-frame-jump.el --- emacs navigation for re-frame projects  -*- lexical-binding: t; -*-

;;; Code:

(require 'cider-util)
(require 'project)
(require 'xref)

(defun nomis/re-frame-jump-to-reg ()
  "Jump to the re-frame registration of the keyword at point.
Uses ripgrep with one line of before-context to handle registrations
where `reg-xxx` is on a different line from the keyword itself.
Return t if we jumped, nil otherwise."
  ;; Written by Claude. I don't fully understand. Doesn't work if there's
  ;; a comment between a `(rf/reg-xxxx` and the name. Probably other
  ;; problems too.
  ;;
  ;; This looks at file contents rather than buffer contents -- that's
  ;; a limitation of our ripgrep-based approach.
  ;;
  ;; Approach:
  ;;   - Run ripgrep with `-B 1` (one line of before-context) to find all
  ;;     occurrences of the keyword in the project.
  ;;   - Parse rg output into (match-line . context-line) pairs.
  ;;   - Filter to lines that look like registrations:
  ;;       - Match line contains `reg-[a-zA-Z-]+` (direct registration) or
  ;;         `:id` (flow registration via `{:id keyword ...}`).
  ;;       - OR the context (previous) line contains `reg-[a-zA-Z-]+`,
  ;;         handling multi-line registrations where `reg-xxx` and the
  ;;         keyword are on separate lines.
  ;;       - Exclude lines containing `subscribe`, `rf/sub`, or `;;`.
  ;;   - If one candidate, jump directly; otherwise offer completing-read.
  ;;   - Jump to line in file, then search forward to land on the keyword.
  (interactive)
  (let* ((kw (cider-symbol-at-point 'look-back))
         (root (or (when-let (p (project-current)) (project-root p))
                   default-directory))
         (cmd (format "rg --with-filename --line-number --no-heading -B 1 -F -- %s %s"
                      (shell-quote-argument kw)
                      (shell-quote-argument root)))
         (raw-lines (split-string (shell-command-to-string cmd) "\n" t))
         (match-re "^\\(.*\\):\\([0-9]+\\):")
         ;; Parse rg -B 1 output into (match-line . context-line) pairs.
         ;; Match lines: file:linenum:content  Context lines: file-linenum-content
         (groups nil)
         (ctx nil))
    (dolist (line raw-lines)
      (cond
       ((equal line "--")
        (setq ctx nil))
       ((string-match match-re line)
        (push (cons line ctx) groups)
        (setq ctx nil))
       (t
        (setq ctx line))))
    (let* ((groups     (nreverse groups))
           ;; Keep matches where the match line OR the context line contains
           ;; reg-xxx or :id
           (reg-re      "reg-[a-zA-Z-]+\\|:id\\b")
           (ctx-reg-re  "reg-[a-zA-Z-]+") ; :id only valid on match line, not context
           (exclude-re  "subscribe\\|rf/sub[ \t]\\|;;")
           (reg-groups (seq-filter (lambda (pair)
                                     (let ((match (car pair))
                                           (ctx   (cdr pair)))
                                       (and (not (string-match-p exclude-re
                                                                 match))
                                            (or (string-match-p reg-re match)
                                                (and ctx
                                                     (string-match-p ctx-reg-re
                                                                     ctx))))))
                                   groups))
           (candidates (mapcar #'car reg-groups)))
      (when candidates
        (let* ((parsed (delq nil
                             (mapcar (lambda (line)
                                       (when (string-match match-re line)
                                         (list line
                                               (match-string 1 line)
                                               (string-to-number (match-string 2 line)))))
                                     candidates)))
               (entry (if (= 1 (length parsed))
                          (car parsed)
                        (let ((choice (completing-read "re-frame def: "
                                                       (mapcar #'car parsed)
                                                       nil t)))
                          (assoc choice parsed)))))
          (when entry
            (xref-push-marker-stack)
            (find-file (nth 1 entry))
            (goto-char (point-min))
            (forward-line (1- (nth 2 entry)))
            (when (search-forward kw nil t)
              (backward-char (length kw)))
            t))))))

;;; End

(provide 'nomis-re-frame-jump)
