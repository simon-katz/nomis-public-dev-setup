;;;; Init stuff -- Org Mode

(progn) ; this stops `hs-hide-all` from hiding the next comment

;;;; ___________________________________________________________________________
;;;; ____ * Require things

(progn ; do-stuff-that-must-be-done-before-requiring-org
  (cl-flet
      ((do-stuff-that-must-be-done-before-requiring-org
        ()
        (setq org-replace-disputed-keys t)
        (setq org-disputed-keys
              '(([(shift up)]               . [(meta p)])
                ([(shift down)]             . [(meta n)])
                ;; M-- was being taken away from `negative-argument`, so
                ;; change the following from the default value of
                ;; `org-disputed-keys`.
                ;; I'd really like to have no keys on the RHS, but I think
                ;; that's not possible. So add alt, hyper and control to get
                ;; unlikely-to-be-wanted key chords.
                ([(shift left)]             . [(alt hyper control meta -)])
                ([(shift right)]            . [(alt hyper control meta +)])
                ([(control shift right)]    . [(alt hyper control meta shift +)])
                ([(control shift left)]     . [(alt hyper control meta shift -)])))))
    (do-stuff-that-must-be-done-before-requiring-org)
    (require 'org)))

(require 'norg)
(require 'nomis-popup)
(require 'org-bullets)
(require 'cl)
(require 'dash)

;;;; ___________________________________________________________________________
;;;; ____ * General

(setq org-directory (file-truename "~/org"))

(setq org-log-done nil)

(setq org-return-follows-link t)

(setq org-startup-indented t)

(setq org-startup-folded t)

(setq org-tags-match-list-sublevels nil)

(progn ; Use current window when clicking links.
  (setq org-link-frame-setup
        (acons 'file
               'find-file
               org-link-frame-setup)))

(setq org-indirect-buffer-display 'new-frame)

;;;; ___________________________________________________________________________
;;;; ____ * Misc API-ish things -- perhaps candidates for `norg`

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;;; ____ ** Last command

(defun nomis/org/last-command ()
  (or (bound-and-true-p *nomis/smex/last-command*)
      last-command))

;;;; ___________________________________________________________________________
;;;; ____ * Show point and entry when jumping to grep results

(advice-add 'compilation-next-error-function
            :after
            (lambda (&rest _)
              (when (eql major-mode 'org-mode)
                (norg/w/show-entry)))
            '((name . nomis/org-show-entry-when-going-to-grep-results)))

;;;; ___________________________________________________________________________
;;;; ____ * Visibility span

(defconst -nomis/org-visibility-span/detail-values
  ;;  See `org-show-context-detail`.
  '((minimal   nil "Minimal")
    ;; (minimal   t   "Minimal + body")
    ;; (local     nil "Local - body")
    ;; (local     t   "Local (includes body)")
    (ancestors nil "Ancestors")
    ;; (ancestors t   "Ancestors + body")
    (lineage   nil "Lineage")
    ;; (lineage   t   "Lineage + body")
    (tree      nil "Tree")
    ;; (tree      t   "Tree + body")
    (canonical nil "Canonical")
    (canonical t   "Canonical + body")))

(defconst -nomis/org-visibility-span/min-detail
  (first -nomis/org-visibility-span/detail-values))

(defconst -nomis/org-visibility-span/max-detail
  (-> -nomis/org-visibility-span/detail-values
      last
      first))

(defconst -nomis/org-visibility-span/max-value
  (1- (length -nomis/org-visibility-span/detail-values)))

(defun -nomis/org-visibility-span/initial-incremental-value ()
  (or (position 'ancestors
                -nomis/org-visibility-span/detail-values
                :key #'first)
      (progn
        (message "Didn't find entry in -nomis/org-visibility-span/detail-values")
        (nomis/msg/grab-user-attention/low)
        1)))

(defconst -nomis/org-visibility-span/commands
  '(nomis/org-visibility-span/more
    nomis/org-visibility-span/less
    nomis/org-visibility-span/set-min
    nomis/org-visibility-span/set-max))

(defvar -nomis/org-visibility-span/prev-action-index -1)

(defun -nomis/org-visibility-span/set-level/numeric (n delta?
                                                       &optional no-message?)
  (let* ((prev-command-was-not-visibility-span?
          (not (member (nomis/org/last-command)
                       -nomis/org-visibility-span/commands)))
         (prev-action-index -nomis/org-visibility-span/prev-action-index)
         (action-index (cond
                        ((not delta?)
                         n)
                        (prev-command-was-not-visibility-span?
                         (-nomis/org-visibility-span/initial-incremental-value))
                        (t
                         (+ n prev-action-index))))
         (ok? (if delta?
                  (<= 0
                      action-index
                      -nomis/org-visibility-span/max-value)
                (or prev-command-was-not-visibility-span?
                    (not (= n prev-action-index)))))
         (new-pos-or-nil
          (if ok?
              (progn
                (setq -nomis/org-visibility-span/prev-action-index
                      action-index)
                action-index)
            nil)))
    (if (null new-pos-or-nil)
        (let* ((msg (third
                     (if (if delta? (< n 0) (= n 0))
                         -nomis/org-visibility-span/min-detail
                       -nomis/org-visibility-span/max-detail))))
          (nomis/popup/error-message "%s" msg))
      (cl-multiple-value-bind (detail show? msg)
          (nth new-pos-or-nil
               -nomis/org-visibility-span/detail-values)
        (norg/collapse-all-and-set-visibility-span detail)
        (if show? (norg/w/show-entry) (norg/w/hide-entry))
        (unless no-message?
          (nomis/popup/message "%s" msg))))))

(defun nomis/org-visibility-span/more ()
  (interactive)
  (-nomis/org-visibility-span/set-level/numeric 1 t))

(defun nomis/org-visibility-span/less ()
  (interactive)
  (-nomis/org-visibility-span/set-level/numeric -1 t))

(defun nomis/org-visibility-span/set-min ()
  (interactive)
  (-nomis/org-visibility-span/set-level/numeric 0 nil))

(defun nomis/org-visibility-span/set-max ()
  (interactive)
  (let* ((v -nomis/org-visibility-span/max-value))
    (-nomis/org-visibility-span/set-level/numeric v nil)))

(defun nomis/org-visibility-span/set-tree+body ()
  (interactive)
  (let* ((v (position '(tree nil "Tree")
                      -nomis/org-visibility-span/detail-values
                      :test #'equal)))
    (cl-assert (not (null v)))
    (-nomis/org-visibility-span/set-level/numeric v nil t))
  (norg/w/show-entry))

;;;; ___________________________________________________________________________
;;;; ____ * Priorities

(setq org-highest-priority ?1)
(setq org-lowest-priority  ?9)
(setq org-default-priority ?2)

;;;; ___________________________________________________________________________
;;;; ____ * Org mode hook function

(setq org-startup-shrink-all-tables t)

(defun nomis/org-mode ()
  ;; Layout
  (linum-mode 0) ; see "Linum-mode + org-indent-mode gives strange graphical refresh bugs" at http://orgmode.org/worg/org-issues.html
  ;; (setq org-indent-fix-section-after-idle-time nil)
  ;; (setq org-indent-indentation-per-level 3) ; the default of 2 is too small; 4 screws up auto indentation in large files
  ;; (setq org-indent-max 60)
  ;; (setq org-indent-max-levels 80)

  ;; Copying and pasting
  (setq org-yank-adjusted-subtrees t)

  ;; Exporting
  ;; (setq org-export-headline-levels 3)
  ;; (setq org-export-initial-scope 'subtree) ; you can set this when running `org-export-dispatch`

  ;; Scrolling
  (setq org-cycle-hook
        (remq 'org-optimize-window-after-visibility-change
              org-cycle-hook))

  ;; Display the first row of the table at point in the header line.
  (org-table-header-line-mode)

  ;; Misc
  (visual-line-mode 1) ; Try this for a while and see whether you like it
  )

(add-hook 'org-mode-hook 'nomis/org-mode)

;;;; ___________________________________________________________________________
;;;; ____ * Hacky fix for `org-table-header-set-header`

(cond
 ((string-match-p (regexp-quote "/org-9.4.4/")
                  (org-version nil t))
  ;; From org-mode commit f12ca1a56 2021-04-17
  ;; - "table: Restore temporary-goal-column after displaying header"
  ;; Note that this new version of the function also has changes that are not
  ;; from that commit.
  ;;
  ;; See near `org-table-header-overlay` for old version.
  ;;
  (defun org-table-header-set-header ()
    "Display the header of the table at point."
    (let ((gcol temporary-goal-column))
      (unwind-protect
          (progn
            (when (overlayp org-table-header-overlay)
              (delete-overlay org-table-header-overlay))
            (let* ((ws (window-start))
                   (beg (save-excursion
                          (goto-char (org-table-begin))
                          (while (or (org-at-table-hline-p)
                                     (looking-at-p ".*|\\s-+<[rcl]?\\([0-9]+\\)?>"))
                            (move-beginning-of-line 2))
                          (line-beginning-position)))
                   (end (save-excursion (goto-char beg) (point-at-eol))))
              (if (pos-visible-in-window-p beg)
                  (when (overlayp org-table-header-overlay)
                    (delete-overlay org-table-header-overlay))
                (setq org-table-header-overlay
                      (make-overlay ws (+ ws (- end beg))))
                (org-overlay-display
                 org-table-header-overlay
                 (org-table-row-get-visible-string beg)
                 'org-table-header))))
        (setq temporary-goal-column gcol)))))
 (t
  ;; All OK in Org mode version 9.4.6 (which has the above version of
  ;; `org-table-header-set-header`).
  ))

;;;; ___________________________________________________________________________
;;;; ____ * Dependencies

(setq org-enforce-todo-dependencies t)
;; (setq org-agenda-dim-blocked-tasks 'invisible) ; actually the default dimmimg is nice -- you can see more info

;;;; ___________________________________________________________________________
;;;; ____ * Capture

(setq org-default-notes-file
      (concat org-directory
              "/___captured-notes-from-"
              (substring (symbol-name nomis/system-name)
                         1)
              ".org"))

;;;; ___________________________________________________________________________
;;;; ____ * Refiling

;;;; - I haven't quite got this nice (or maybe I did).
;;;; - BUT:
;;;;   After a refile, undo only undoes what happened in one buffer, even
;;;;   though two buffers have been modified. That's crappy.

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;;; ____ Play #1

;; (progn org-use-fast-todo-selection)

;; (setf org-refile-targets '((nil :maxlevel . 9)
;;                            (org-agenda-files :maxlevel . 9)))
;; (setf org-refile-use-outline-path t)
;; (setf org-outline-path-complete-in-steps t)
;; (setq org-refile-allow-creating-parent-nodes 'confirm)
;; (setq org-completion-use-ido nil)

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;;; ____ Play #2 -- 2017-08-10

;; (defun nomis/org-refile-values ()
;;   (list org-refile-targets
;;         org-refile-use-outline-path
;;         org-outline-path-complete-in-steps
;;         org-refile-allow-creating-parent-nodes
;;         org-completion-use-ido))

;; (defun nomis/org-refile-reset ()
;;   (setq org-refile-targets nil
;;         org-refile-use-outline-path nil
;;         org-outline-path-complete-in-steps t
;;         org-refile-allow-creating-parent-nodes nil
;;         org-completion-use-ido nil))

;; (defun nomis/org-refile-setup ()
;;   (setq org-refile-targets '((nil :maxlevel . 9)
;;                              (org-agenda-files :maxlevel . 9))
;;         org-refile-use-outline-path nil ; 'file
;;         org-outline-path-complete-in-steps nil
;;         org-refile-allow-creating-parent-nodes nil
;;         org-completion-use-ido t))

;;;; ___________________________________________________________________________
;;;; ____ * Agendas

(require 'org-agenda)

(progn
  (defun nomis/org-reset-org-agenda-files ()
    (interactive)
    (setq org-agenda-files
          (progn
            (load-library "find-lisp")
            (find-lisp-find-files org-directory
                                  "\\\.org$"))))
  (defadvice org-todo-list (before reset-agenda-files
                                   activate compile)
    (nomis/org-reset-org-agenda-files)))

(progn
  (defun nomis/org-finalize-agenda-hook ()
    (hl-line-mode)
    ;; From http://orgmode.org/worg/org-faq.html
    ;;   How can I stop the mouse cursor from highlighting lines
    ;;   in the agenda?
    ;;     You can add the following to your .emacs:
    (remove-text-properties
     (point-min) (point-max) '(mouse-face t)))
  (add-hook 'org-agenda-finalize-hook
            'nomis/org-finalize-agenda-hook))

;;;; ___________________________________________________________________________
;;;; ____ * Fontify code in code blocks

(setq org-src-fontify-natively t)

;;;; ___________________________________________________________________________
;;;; ***************************************************************************
;;;; After upgrading to Org Mode 9.4.4, the following orgstruct stuff gives an
;;;; error when I start Emacs:
;;;;
;;;; run-hooks: Autoloading file
;;;; [...]/emacs-configuration/emacs-installation/.emacs.d/elpa/org-9.4.4/org.elc
;;;; failed to define function orgstruct-mode
;;;;
;;;; ***************************************************************************
;;;; ___________________________________________________________________________
;;
;; ;;;; ___________________________________________________________________________
;; ;;;; ____ * orgstruct
;;
;; ;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; ;;;; ____ orgstruct-mode general
;;
;; ;;;; To use, enable orgstruct-mode or orgstruct++-mode
;;
;; (setq orgstruct-heading-prefix-regexp ";+ *\\(?:_+ \\)?")
;;
;; (add-hook 'prog-mode-hook 'orgstruct-mode)
;;
;; ;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; ;;;; ____ orgstruct-mode display
;;
;; (defvar nomis/orgstruct-display-table nil
;;   ;; Approach copied from `org-display-table` stuff.
;;   "The display table for orgstruct-mode when `org-ellipsis' is non-nil.")
;;
;; (defun nomis/orgstruct-set-up-display-table ()
;;   (if (not (member org-version
;;                    '("9.1.9")))
;;       (progn
;;         (nomis/msg/grab-user-attention/low)
;;         (message "•••• You need to check `nomis/orgstruct-set-up-display-table` for this version of Org mode."))
;;     (when (and (stringp org-ellipsis) (not (equal "" org-ellipsis)))
;;       (unless nomis/orgstruct-display-table
;;         (setq nomis/orgstruct-display-table (make-display-table)))
;;       (set-display-table-slot
;;        nomis/orgstruct-display-table 4
;;        (vconcat (mapcar (lambda (c) (make-glyph-code c 'org-ellipsis))
;;                         org-ellipsis)))
;;       (setq buffer-display-table nomis/orgstruct-display-table))))
;;
;; (add-hook 'orgstruct-mode-hook 'nomis/orgstruct-set-up-display-table)

;;;; ___________________________________________________________________________
;;;; ____ * Display -- misc

(setq org-ellipsis " ▶")

(setq org-bullets-bullet-list '("⦿"
                                "■"
                                "●"
                                "✖"
                                "♣"
                                "◑"
                                "◒"
                                "◐"
                                "◓"
                                "◮"
                                "◭"
                                ;; "➀" "➁" "➂" "➃" "➄" "➅" "➆" "➇" "➈" "➉"
                                "➊" "➋" "➌" "➍" "➎" "➏" "➐" "➑" "➒" "➓"
                                ;; "█" "▊" "▌" "▎"
                                ))

(add-hook 'org-mode-hook 'org-bullets-mode)

;;;; ___________________________________________________________________________
;;;; ____ * Display -- links

(defun nomis/org-show-link-destination ()
  ;; Copied with changes from
  ;; https://stackoverflow.com/questions/30312638/is-there-a-package-or-setting-to-show-an-org-mode-link-under-cursor-destinatio
  (when (memq major-mode
              '(org-mode
                org-agenda-mode))
    (ignore-errors ; sometimes this breaks, (?) and stops future ones running (?) (but maybe that was before checking the major-mode)
      (let ((object (org-element-context)))
        (when (eq (car object) 'link)
          (message "%s"
                   (org-element-property :raw-link object)))))))

(add-hook 'post-command-hook 'nomis/org-show-link-destination)

;;;; ___________________________________________________________________________
;;;; ____ * Display -- faces


;;;; ___________________________________________________________________________
;;;; ____ * Display -- blog faces

(defconst nomis/org-blog-faces
  '((org-level-1 . (:inherit outline-1 :weight bold :height 1.3
                             :box (:line-width 2
                                               :color "grey75"
                                               :style released-button)))
    (org-level-2 . (:inherit outline-2 :weight bold :height 1.2))
    (org-level-3 . (:inherit outline-3 :weight bold :height 1.1))
    (org-level-7 . nil)))

(defvar nomis/org-blog-stuff-on-p nil)

(defun nomis/toggle-org-blog-stuff ()
  (interactive)
  (make-local-variable 'nomis/org-blog-stuff-on-p)
  (make-local-variable 'face-remapping-alist)
  (setq face-remapping-alist
        (if nomis/org-blog-stuff-on-p
            (let* ((org-blog-faces-keys (-map 'car nomis/org-blog-faces)))
              (-remove (lambda (x) (memq (car x) org-blog-faces-keys))
                       face-remapping-alist))
          (append nomis/org-blog-faces
                  face-remapping-alist)))
  (setq nomis/org-blog-stuff-on-p
        (not nomis/org-blog-stuff-on-p)))

;;;; ___________________________________________________________________________
;;;; ____ * Display -- Alternative heading faces

;;;; A set of colours that avoids clashes with my `xxxx` highlighting.

(defvar nomis/org-alternative-heading-faces
  '((outline-1 . (:inherit font-lock-function-name-face))
    (outline-2 . (:inherit font-lock-variable-name-face))
    (outline-3 . (:inherit font-lock-keyword-face))
    (outline-4 . (:inherit font-lock-comment-face))
    (outline-5 . (:foreground "Goldenrod1")) ; default is (:inherit font-lock-type-face)
    (outline-6 . (:foreground "Cyan1")) ; default is (:inherit font-lock-constant-face)
    (outline-7 . (:inherit font-lock-builtin-face))
    (outline-8 . (:inherit font-lock-string-face))))

(defvar nomis/org-alternative-heading-stuff-on-p nil)

(defun nomis/toggle-org-alternative-heading-stuff ()
  (interactive)
  (make-local-variable 'nomis/org-alternative-heading-stuff-on-p)
  (make-local-variable 'face-remapping-alist)
  (setq face-remapping-alist
        (if nomis/org-alternative-heading-stuff-on-p
            (let* ((org-alternative-heading-faces-keys (-map 'car nomis/org-alternative-heading-faces)))
              (-remove (lambda (x) (memq (car x) org-alternative-heading-faces-keys))
                       face-remapping-alist))
          (append nomis/org-alternative-heading-faces
                  face-remapping-alist)))
  (setq nomis/org-alternative-heading-stuff-on-p
        (not nomis/org-alternative-heading-stuff-on-p)))

;;;; ___________________________________________________________________________
;;;; ____ * Export1

(defun get-string-from-file (path)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defconst nomis/org-export-apply-hacks-max-read-attempts 20)
(defconst nomis/org-export-apply-hacks-sleep-ms 100)

(cl-defun nomis/org-export-read-hacked-file
    (output-path &optional (n-attempts 1))
  (cl-flet ((sleep-a-bit
             ()
             (sleep-for 0 nomis/org-export-apply-hacks-sleep-ms)))
    (cond ((file-exists-p output-path)
           (sleep-a-bit) ; in case file exists but writing hasn't finished
           (get-string-from-file output-path))
          ((<= n-attempts nomis/org-export-apply-hacks-max-read-attempts)
           (sleep-a-bit)
           (nomis/org-export-read-hacked-file output-path (1+ n-attempts)))
          (t
           (beep)
           (error "FAILED: Tried %s times to read %s"
                  nomis/org-export-apply-hacks-max-read-attempts
                  output-path)))))

(cl-defun nomis/org-export-apply-hacks (&optional s)
  (let ((input-path (make-temp-file "__nomis-org-export--input-"))
        (output-path (make-temp-file "__nomis-org-export--output-")))
    (delete-file output-path) ; later we will wait for it to be created
    (write-region s nil input-path)
    (unwind-protect
        (progn
          (nomis/run-clojure-no-insert
           (format "(do (require '[nomis-blog.layer-2-domain.content.source.org-mode-source.pre-parse-transforms :as ppt])
                         (ppt/org-export-apply-hacks-to-file :latex
                                                              \"%s\"
                                                              \"%s\"))"
                   input-path
                   output-path))
          (nomis/org-export-read-hacked-file output-path))
      (ignore-errors (delete-file input-path))
      (ignore-errors (delete-file output-path)))))

(defun nomis/org-export ()
  (interactive)
  (let* ((s (buffer-string))
         (old-buffer (current-buffer))
         (name (-> old-buffer
                   buffer-file-name
                   file-name-nondirectory))
         (temp-name (concat "__zzzz--temp--nomis-org-export--" name))
         (new-buffer (generate-new-buffer temp-name)))
    (progn ; I did have `unwind-protect`, but that meant I didn't see errors
      (with-current-buffer new-buffer
        (let ((new-s (nomis/org-export-apply-hacks s)))
          (insert new-s))
        (write-file temp-name)
        (org-export-dispatch) ; user must select "latex", then "pdf"
        )
      (kill-buffer new-buffer)
      (delete-file temp-name)
      (let ((temp-name-sans-extension (file-name-sans-extension temp-name))
            (name-sans-extension (file-name-sans-extension name)))
        (rename-file (concat temp-name-sans-extension ".tex")
                     (concat name-sans-extension ".tex")
                     t)
        (rename-file (concat temp-name-sans-extension ".pdf")
                     (concat name-sans-extension ".pdf")
                     t)))))

;;;; Publishing

;;;; This doesn't work particularly well. Do it in a terminal window instead.

;; (defun nomis/ordinary-insertion-filter (proc string)
;;   ;; Copied, with changes, from
;;   ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Filter-Functions.html#Filter-Functions
;;   (when (buffer-live-p (process-buffer proc))
;;     (with-current-buffer (process-buffer proc)
;;       (let ((moving (cl-case 2
;;                       (1 (= (point) (process-mark proc)))
;;                       (2 t))))
;;         (save-excursion
;;           ;; Insert the text, advancing the process marker.
;;           (goto-char (process-mark proc))
;;           (insert (s-replace "" "\n" string))
;;           (set-marker (process-mark proc) (point)))
;;         (if moving (goto-char (process-mark proc)))))))

;; (defun nomis/org-publish-filter-function (proc string)
;;   (nomis/ordinary-insertion-filter proc string))

;; (defun nomis/org-publish ()
;;   (interactive)
;;   (let (;; (password (read-passwd "Enter password: "))
;;         (output-buffer (get-buffer-create "nomis-org-publish")))
;;     (display-buffer output-buffer)
;;     (with-current-buffer output-buffer
;;       (goto-char (point-max))
;;       (insert "\n____________________________________\n"))
;;     (cl-case 2
;;       (1 (call-process "/Users/simonkatz/development-100/repositories/nomis/nomis-blog/_scripts/publish.sh"
;;                        nil
;;                        output-buffer
;;                        t
;;                        ;; (concat "\"" password "\"")
;;                        ))
;;       (2 (make-process :name "nomis-org-publish"
;;                        :buffer output-buffer
;;                        :filter 'nomis/org-publish-filter-function
;;                        :command (list "/Users/simonkatz/development-100/repositories/nomis/nomis-blog/_scripts/publish.sh"
;;                                       ;; (concat "\"" password "\"")
;;                                       )
;;                        ;; :stderr output-buffer
;;                        )))))

;;;; ___________________________________________________________________________
;;;; * nomis/org-search-heading-text

(defun -nomis/org-grab-heading-text ()
  (save-excursion
    ;; Jump to first word of heading
    (norg/w/back-to-heading)
    (forward-word)
    (backward-word)
    ;; Grab text of heading
    (let* ((beg (point))
           (end (progn
                  (org-end-of-line)
                  (point))))
      (if (not (< beg end))
          (error "No heading here")
        (let* ((text (buffer-substring beg end)))
          (set-text-properties 0 (length text) nil text)
          text)))))

(defvar -nomis/org-search-heading-text/text nil)

(defun -nomis/org-search-heading-text/search (again?)
  (cl-assert (not (null -nomis/org-search-heading-text/text)))
  (cl-flet ((search-for-text
             ()
             (search-backward (cl-case 1
                                (1
                                 ;; Simply look for the text.
                                 -nomis/org-search-heading-text/text)
                                (2
                                 ;; Just find refs. But is awkward -- you need
                                 ;; to press M-. to go back. I wanted H-. but
                                 ;; I'm already using that for something else.
                                 (s-concat "[[*"
                                           -nomis/org-search-heading-text/text
                                           "]")))
                              nil
                              t)))
    (when again?
      (norg/w/back-to-heading))
    (or (search-for-text)
        (progn
          (end-of-buffer)
          (search-for-text))))
  (norg/show-point))

(defun nomis/org-search-heading-text ()
  (interactive)
  (setq -nomis/org-search-heading-text/text (-nomis/org-grab-heading-text))
  (unless (and org-mark-ring
               (ignore-errors ; without this, we can get "Marker does not point anywhere" errors
                 (= (first org-mark-ring)
                    (point))))
    ;; Note: Ive tried customising `org-mark-ring-length` using
    ;; `customize-variable`, but I can't get it to work. There's a comment in
    ;; `org-mark-ring-length` saying "Changing this requires a restart of Emacs
    ;; to work correctly", but even restarting doesn't work.
    (org-mark-ring-push))
  (-nomis/org-search-heading-text/search nil))

(defun nomis/org-search-heading-text-again ()
  (interactive)
  (if (null -nomis/org-search-heading-text/text)
      (error "nomis/org-search-heading-text hasn't been called yet")
    (-nomis/org-search-heading-text/search t)))

;;;; ___________________________________________________________________________
;;;; * nomis/org-get-links-to-current-heading

;;;; Inspired by
;;;; https://stackoverflow.com/questions/9844154/list-all-inbound-links-to-a-header-in-org-mode

(defun nomis/org-get-links-to-current-heading (arg)
  "Show links to the current heading.

By default search in all .org buffers. With a prefix argument,
limit the search to the current buffer."
  (interactive "P")
  (let* ((title (nth 4 (org-heading-components)))
         (regexp (concat (nomis/rx/or (nomis/rx/wrap "\\[\\[")
                                      (nomis/rx/wrap "::"))
                         "\\*"
                         (->> (org-link-make-string title)
                              (s-chop-prefix "[[")
                              (s-chop-suffix "]")
                              regexp-quote))))
    (if arg
        (occur regexp)
      (multi-occur-in-matching-buffers ".*\\.org$" regexp))))

;;;; ___________________________________________________________________________
;;;; * nomis/org-global-todo-list

(defun nomis/org-global-todo-list ()
  (interactive)
  ;; (nomis/themes/disable-and-set-custom-themes
  ;;  nomis/themes/standard-light+nomis+altbg1)
  (org-todo-list))

;;;; ___________________________________________________________________________
;;;; * org-content

(cond ((member org-version
               '("9.5.2"))
       ;; Fix broken `org-content` — a nil argument should show all headlines.
       ;; Copied from `org` package and hacked — changed "p" to "P".
       (defun org-content (&optional arg)
         "Show all headlines in the buffer, like a table of contents.
With numerical argument N, show content up to level N."
         (interactive "P")
         (org-show-all '(headings drawers))
         (save-excursion
           (goto-char (point-max))
           (let ((regexp (if (and (wholenump arg) (> arg 0))
                             (format "^\\*\\{1,%d\\} " arg)
                           "^\\*+ "))
                 (last (point)))
             (while (re-search-backward regexp nil t)
               (org-flag-region (line-end-position) last t 'outline)
               (setq last (line-end-position 0)))))))
      ((version<= "9.5.5" org-version)
       ;; It's fixed now.
       )
      (t
       (message-box "You need to fix `org-content` for this version of org mode.")))

;;;; ___________________________________________________________________________
;;;; * Key bindings

(require 'nomis-org-key-bindings)

;;;; ___________________________________________________________________________
;;;; * End

(provide 'nomis-org)
