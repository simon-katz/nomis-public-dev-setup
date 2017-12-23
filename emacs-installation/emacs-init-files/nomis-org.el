;;;; ________ * Init stuff -- Org Mode.

;;;; ________ ** Require things

(progn
  (setq org-replace-disputed-keys t) ; must be done before requiring org
  (require 'org))

(require 'cl)
(require 'dash)

;;;; ________ ** Stuff everyone needs

;;; The following lines are always needed. Choose your own keys.

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-hook 'org-mode-hook 'turn-on-font-lock) ; not needed when global-font-lock-mode is on
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;;;; ________ ** Personal tailoring

;;;; ________ *** General

(setq org-directory "~/org")

(setq org-log-done nil)

(setq org-return-follows-link t)

(setq org-startup-indented t)

(progn
  ;; Use current window when clicking links.
  (setq org-link-frame-setup
        (acons 'file
               'find-file
               org-link-frame-setup)))

;;;; ________ *** Priorities

(setq org-highest-priority ?1)
(setq org-lowest-priority  ?9)
(setq org-default-priority ?2)

;;;; ________ *** Org mode hook function

(defun nomis-org-mode ()
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
              org-cycle-hook)))

(add-hook 'org-mode-hook 'nomis-org-mode)

;;;; ________ *** Dependencies

(setq org-enforce-todo-dependencies t)
;; (setq org-agenda-dim-blocked-tasks 'invisible) ; actually the default dimmimg is nice -- you can see more info

;;;; ________ *** Capture

(setq org-default-notes-file
      (concat org-directory
              "/___captured-notes-from-"
              (substring (symbol-name nomis/system-name)
                         1)
              ".org"))

(define-key global-map "\C-cc" 'org-capture)

;;;; ________ *** Navigation

(defun nomis/org-show-point ()
  (interactive)
  (case 1
    (1
     (nomis/with-temporary-invisible-changes ()
       (org-meta-return) ; this does what it does and also makes point visible
       ))
    (2
     ;; This makes lots of stuff visible, but seems to be the "official" way.
     ;; Leave this here as a point of interest.
     (let ((org-catch-invisible-edits 'show))
       (org-check-before-invisible-edit 'insert)))))

(defun nomis/org-previous-heading ()
  (interactive)
  (outline-previous-heading)
  (nomis/org-show-point))

(defun nomis/org-next-heading ()
  (interactive)
  (outline-next-heading)
  (nomis/org-show-point))

(defun nomis/-org-heading-same-level-with-extras/helper (direction)
  (let ((start-position-fun (case direction
                              (:forward 'org-end-of-line)
                              (:backward 'org-beginning-of-line)))
        (re-search-function (case direction
                              (:forward 're-search-forward)
                              (:backward 're-search-backward)))
        (post-search-adjust-function (case direction
                                       (:forward 'org-beginning-of-line)
                                       (:backward #'(lambda ())))))
    (let* ((text-to-look-for (save-excursion
                               (org-beginning-of-line)
                               (concat (thing-at-point 'symbol)
                                       " "))))
      (funcall start-position-fun)
      (let ((found-p (condition-case nil
                         (progn
                           (funcall re-search-function
                                    (concat "^" (regexp-quote text-to-look-for))
                                    nil
                                    nil ;t
                                    )
                           t)
                       (error nil))))
        (if found-p
            (progn
              (nomis/org-show-point)
              (funcall post-search-adjust-function))
          (progn
            (org-beginning-of-line)
            (message (concat "No more headings at this level"
                             (when (eql direction :forward)
                               " (but there's a bug so maybe there are...)")))
            (beep)))))))

(defun nomis/org-forward-heading-same-level-with-extras ()
  "A replacement for `org-forward-heading-same-level`.
Move forward one subheading at same level as this one.
Works when the target is invisible (and makes it visible).
If this is the first subheading within its parent, move to the first
subheading at this level in the next parent."
  (interactive)
  (nomis/-org-heading-same-level-with-extras/helper :forward))

(define-key org-mode-map [remap org-forward-heading-same-level]
  'nomis/org-forward-heading-same-level-with-extras)

(defun nomis/org-backward-heading-same-level-with-extras ()
  "A replacement for `org-backward-heading-same-level`.
Move backward one subheading at same level as this one.
Works when the target is invisible (and makes it visible).
If this is the first subheading within its parent, move to the last
subheading at this level in the previous parent."
  (interactive)
  (nomis/-org-heading-same-level-with-extras/helper :backward))

(define-key org-mode-map [remap org-backward-heading-same-level]
  'nomis/org-backward-heading-same-level-with-extras)


;;;; ________ *** Refiling

;;;; - I haven't quite got this nice (or maybe I did).
;;;; - BUT:
;;;;   After a refile, undo only undoes what happened in one buffer, even
;;;;   though two buffers have been modified. That's crappy.

;;;; ________ **** Play #1

;; (progn org-use-fast-todo-selection)

;; (setf org-refile-targets '((nil :maxlevel . 9)
;;                            (org-agenda-files :maxlevel . 9)))
;; (setf org-refile-use-outline-path t)
;; (setf org-outline-path-complete-in-steps t)
;; (setq org-refile-allow-creating-parent-nodes 'confirm)
;; (setq org-completion-use-ido nil)

;;;; ________ **** Play #2 -- 2017-08-10

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

;;;; ________ *** Agendas

(require 'org-agenda)

(progn
  (defun nomis-org-reset-org-agenda-files ()
    (interactive)
    (setq org-agenda-files
          (progn
            (load-library "find-lisp")
            (find-lisp-find-files org-directory
                                  "\.org$"))))
  (defadvice org-todo-list (before reset-agenda-files
                                   activate compile)
    (nomis-org-reset-org-agenda-files)))

(progn
  (defun nomis-org-finalize-agenda-hook ()
    (hl-line-mode)
    ;; From http://orgmode.org/worg/org-faq.html
    ;;   How can I stop the mouse cursor from highlighting lines
    ;;   in the agenda?
    ;;     You can add the following to your .emacs:
    (remove-text-properties
     (point-min) (point-max) '(mouse-face t)))
  (add-hook 'org-finalize-agenda-hook
            'nomis-org-finalize-agenda-hook))

(progn
  (defun nomis-setup-org-keys ()
    ;; I don't like RETURN in org agenda giving ORG-AGENDA-SWITCH-TO.
    ;; I prefer this:
    (org-defkey org-agenda-mode-map "\C-m" 'org-agenda-show-and-scroll-up))
  (add-hook 'org-mode-hook 'nomis-setup-org-keys))

(add-hook 'org-mode-hook 'nomis-turn-on-idle-highlight-mode)

;;;; ________ *** Fontify code in code blocks

(setq org-src-fontify-natively t)

;;;; ________ *** orgstruct

;; To use, enable orgstruct-mode or orgstruct++-mode

(setq orgstruct-heading-prefix-regexp ";* *_* ")

;;;; ________ *** Display -- links

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


;;;; ________ *** Display -- blog faces

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


;;;; ________ *** Export

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
;;       (let ((moving (case 2
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
;;     (case 2
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

;;;; ________ ** end

(provide 'nomis-org)
