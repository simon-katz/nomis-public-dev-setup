;;;; Init stuff -- Fixes for `highlight`.

(defvar *nomis/hlt/no-leave-gaps?* nil)

(cond
 ((member emacs-version
          '("26.1"))
  (defun hlt-+/--highlight-regexp-region (unhighlightp start end regexp face msgp mousep nth &optional buffers)
    "Hacked version of `hlt-+/--highlight-regexp-region` that does not leave
gaps between matches.

Helper for `hlt-(un)highlight-regexp-region'.
Non-nil UNHIGHLIGHTP means unhighlight.  Otherwise, highlight.
The other arguments are as for `hlt-highlight-regexp-region'.
If UNHIGHLIGHTP:
 Do not advance to the next face, even if `hlt-auto-faces-flag'.
 If FACE is nil then unhighlight all faces."
    (unless regexp (setq regexp  hlt-last-regexp))
    (unless (stringp regexp) ; Else re-search-forward gets an error
      (error "HLT-%sHIGHLIGHT-REGEXP-REGION: REGEXP arg is not a string: `%S'"
             (if unhighlightp "UN" "")
             regexp))
    (let ((mbufs  buffers))
      (unless buffers (setq buffers  (list (current-buffer))))
      ;; Advance the face if highlighting (but not unhighlighting) with auto faces.
      (when (and hlt-auto-faces-flag  (not unhighlightp)) (hlt-next-face))
      (if face (setq hlt-last-face  face) (unless unhighlightp (setq face  hlt-last-face)))
      (dolist (buf  buffers)
        (with-current-buffer buf
          (unless (and start  end  (not (cadr buffers)))
            (let ((start-end  (hlt-region-or-buffer-limits buf)))
              (setq start  (car start-end)
                    end    (cadr start-end))))
          (when (and msgp  (not unhighlightp))
            (let ((reg-size  (abs (- end start))))
              (when (and (> reg-size hlt-max-region-no-warning)
                         (not (progn (and (fboundp 'flash-ding) ; In `frame-fns.el'
                                          (flash-ding 'no-terminate-macros (selected-frame)))
                                     (y-or-n-p (substitute-command-keys
                                                (format "Lots of highlighting slows things down.  Do you \
really want to highlight up to %d chars?  "
                                                        reg-size))))))
                (error "OK, highlighting cancelled"))))
          (when (eq t msgp)
            (message "%sighlighting occurrences of `%s'%s..."
                     (if unhighlightp "UNh" "H")
                     regexp
                     (if mbufs (format " in `%s'"  buf) "")))
          (let ((hits-p               nil)
                (hlt-auto-faces-flag  nil)) ; Prevent advancing - we already advanced.
            (save-excursion
              (goto-char start)
              (while (and (< start end)  (not (eobp))  (re-search-forward regexp end t)  (setq hits-p  t))
                (condition-case nil
                    (progn
                      (unless *nomis/hlt/no-leave-gaps?* ; JSK 2019-07-09 JSK-HACKED
                        (forward-char 1))
                      (setq start  (1+ (point))))
                  (end-of-buffer (setq start  end)))
                (funcall (if unhighlightp #'hlt-unhighlight-region #'hlt-highlight-region)
                         (match-beginning (or nth  0))
                         (match-end (or nth  0))
                         face
                         nil
                         mousep)))
            (when (eq t msgp)
              (if hits-p
                  (message "%sighlighting occurrences of `%s'%s done  %s"
                           (if unhighlightp "UNh" "H")
                           regexp
                           (if mbufs (format " in `%s'"  buf) "")
                           (if unhighlightp
                               ""
                             (let ((remove-msg  "\\[hlt-unhighlight-regexp-region]' to remove highlighting"))
                               (when mousep (setq remove-msg  (concat "\\[universal-argument] " remove-msg)))
                               (setq remove-msg  (substitute-command-keys (concat "`" remove-msg)))
                               remove-msg)))
                (message "No occurrences of `%s' in `%s'" regexp buf))))
          (setq hlt-last-regexp  regexp))))))
 (t
  (message-box
   "You need to fix `hlt-+/--highlight-regexp-region` for this version of Emacs."))
 )

(provide 'nomis-highlight-hacks)
