;;;; Init stuff -- CIDER REPL --  -*- lexical-binding: t -*-

;;;; ___________________________________________________________________________

(cond
 ((member (pkg-info-package-version 'cider)
          '((20220830 500)))

  ;; From https://github.com/dakra/cider/commit/93c9a22db864abb81a119e4d807268434b3576b2

  ;; See https://clojurians.slack.com/archives/C0617A8PQ/p1672395486556439?thread_ts=1672319263.233099&cid=C0617A8PQ

  (defun cider-repl--interpret-crlf (string) ; original is in cider-repl.el
    "Change STRING in the same way as it would be displayed in a shell.
I.e. \r will jump to the beginning of the line and the characters
after will overwrite what's already written on this line.
\n will always insert a newline at the end of line."
    (with-temp-buffer
      (dolist (char (append string nil))
        (cond ((char-equal char ?\r)
               (move-beginning-of-line 1))
              ((char-equal char ?\n)
               (move-end-of-line 1) (newline))
              (t
               (when (/= (point) (point-max)) ; Overwrite character
                 (delete-char 1))
               (insert char))))
      (buffer-substring (point-min) (point-max))))

  (defun cider-repl--emit-output (buffer string face) ; original is in cider-repl.el
    "Using BUFFER, emit STRING as output font-locked using FACE.
Before inserting, run `cider-repl-preoutput-hook' on STRING."
    (with-current-buffer buffer
      (save-excursion
        (cider-save-marker cider-repl-output-start
          (goto-char cider-repl-output-end)
          (when (char-equal (aref string 0) ?\r)
            (setq string (concat (buffer-substring (line-beginning-position) (line-end-position))
                                 string))
            (delete-region (line-beginning-position) (line-end-position)))
          (setq string (cider-repl--interpret-crlf string))
          (setq string (propertize string
                                   'font-lock-face face
                                   'rear-nonsticky '(font-lock-face)))
          (setq string (cider-run-chained-hook 'cider-repl-preoutput-hook string))
          (insert-before-markers string))
        (when (and (= (point) cider-repl-prompt-start-mark)
                   (not (bolp)))
          (insert-before-markers "\n")
          (set-marker cider-repl-output-end (1- (point))))))
    (when-let* ((window (get-buffer-window buffer t)))
      ;; If the prompt is on the first line of the window, then scroll the window
      ;; down by a single line to make the emitted output visible.
      (when (and (pos-visible-in-window-p cider-repl-prompt-start-mark window)
                 (< 1 cider-repl-prompt-start-mark)
                 (not (pos-visible-in-window-p (1- cider-repl-prompt-start-mark) window)))
        (with-selected-window window
          (scroll-down 1)))))

  (defun nrepl--bdecode-1 (&optional stack) ; original is in nrepl-client
    "Decode one elementary bencode object starting at point.
Bencoded object is either list, dict, integer or string.  See
http://en.wikipedia.org/wiki/Bencode#Encoding_algorithm for the encoding
rules.

STACK is a list of so far decoded components of the current message.  Car
of STACK is the innermost incompletely decoded object.  The algorithm pops
this list when inner object was completely decoded or grows it by one when
new list or dict was encountered.

The returned value is of the form (INFO . STACK) where INFO is
:stub, nil, :end or :eob and STACK is either an incomplete parsing state as
above (INFO is :stub, nil or :eob) or a list of one component representing
the completely decoded message (INFO is :end).  INFO is nil when an
elementary non-root object was successfully decoded.  INFO is :end when this
object is a root list or dict."
    (cond
     ;; list
     ((eq (char-after) ?l)
      (nrepl--bdecode-list (cons () stack)))
     ;; dict
     ((eq (char-after) ?d)
      (nrepl--bdecode-list (cons '(dict) stack)))
     ;; end of a list or a dict
     ((eq (char-after) ?e)
      (forward-char 1)
      (cons (if (cdr stack) :e :end)
            (nrepl--push (nrepl--nreverse (car stack))
                         (cdr stack))))
     ;; string
     ((looking-at "\\([0-9]+\\):")
      (let ((pos0 (point))
            (beg (goto-char (match-end 0)))
            (end (byte-to-position (+ (position-bytes (point))
                                      (string-to-number (match-string 1))))))
        (if (null end)
            (progn (goto-char pos0)
                   (cons :stub stack))
          (goto-char end)
          ;; normalise any platform-specific newlines
          (let* ((original (buffer-substring-no-properties beg end))
                 (result (replace-regexp-in-string "\r\n\\|\n\r" "\n" original)))
            (cons nil (nrepl--push result stack))))))
     ;; integer
     ((looking-at "i\\(-?[0-9]+\\)e")
      (goto-char (match-end 0))
      (cons nil (nrepl--push (string-to-number (match-string 1))
                             stack)))
     ;; should happen in tests only as eobp is checked in nrepl-bdecode.
     ((eobp)
      (cons :eob stack))
     ;; truncation in the middle of an integer or in 123: string prefix
     ((looking-at-p "[0-9i]")
      (cons :stub stack))
     ;; else, throw a quiet error
     (t
      (message "Invalid bencode message detected. See the %s buffer for details."
               nrepl-error-buffer-name)
      (nrepl-log-error
       (format "Decoder error at position %d (`%s'):"
               (point) (buffer-substring (point) (min (+ (point) 10) (point-max)))))
      (nrepl-log-error (buffer-string))
      (ding)
      ;; Ensure loop break and clean queues' states in nrepl-bdecode:
      (goto-char (point-max))
      (cons :end nil)))))

 (t
  (message-box
   "You need to fix `nomis/-delete-duplicate-kaocha-autotest-lines`` for this version of vterm.")))

;; (advice-remove 'cider-repl--emit-output 'nomis/-delete-duplicate-kaocha-autotest-lines)

;;;; ___________________________________________________________________________

(setq cider-print-quota 10000)
(setq cider-repl-buffer-size-limit 1000)

(setq cider-print-fn 'pr)
(setq cider-print-options '(("length" 50) ;("right-margin" 70)
                            ))


(provide 'nomis-cider-repl)
