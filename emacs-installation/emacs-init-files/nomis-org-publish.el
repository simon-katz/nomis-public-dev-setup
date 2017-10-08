;;;; Init stuff -- Org Mode.

;;;; ___________________________________________________________________________

(defvar nomis/repos-directory "~/development-100/repositories/nomis/")

;;;; ___________________________________________________________________________

;; (progn ; Jekyll stuff

;;   (require 'ox-publish)

;;   (setq org-export-with-toc nil)

;;   (defvar nomis/jekyll-blog/base-directory
;;     (concat nomis/repos-directory "blog-play-using-jekyll/"))

;;   (defvar nomis/jekyll-blog/org-directory
;;     (concat nomis/jekyll-blog/base-directory "001-org/"))

;;   (defvar nomis/jekyll-blog/jekyll-input-directory
;;     (concat nomis/jekyll-blog/base-directory "002-jekyll-input/"))

;;   (defvar nomis/jekyll-blog/site-directory
;;     (concat nomis/jekyll-blog/base-directory "003-site/"))

;;   (setq org-publish-project-alist
;;         `(("org-notes"
;;            :base-directory ,nomis/jekyll-blog/org-directory
;;            :base-extension "org"
;;            :publishing-directory ,nomis/jekyll-blog/jekyll-input-directory
;;            :recursive t
;;            :publishing-function org-html-publish-to-html
;;            :headline-levels 4 ; Just the default for this project.
;;            :body-only t)
;;           ("org-static"
;;            :base-directory ,nomis/jekyll-blog/org-directory
;;            :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
;;            :publishing-directory ,nomis/jekyll-blog/jekyll-input-directory
;;            :recursive t
;;            :publishing-function org-publish-attachment)
;;           ("nomis-jekyll-blog" :components ("org-notes" "org-static"))))

;;   (defun nomis/publish-blog/jekyll (force?)
;;     (interactive "P")
;;     (org-publish-project "nomis-jekyll-blog" force?)
;;     (shell-command (s-join " "
;;                            `("jekyll build"
;;                              "--source"
;;                              ,nomis/jekyll-blog/jekyll-input-directory
;;                              "--destination"
;;                              ,nomis/jekyll-blog/site-directory)))))

;;;; ___________________________________________________________________________
;;;; Using Hugo.
;;;; Copied and modified from http://www.modernemacs.com/post/org-mode-blogging/

;; (progn ; Hugo stuff

;;   (require 'cl)
;;   (require 'dash)

;;   (defvar nomis/hugo-blog/base-directory
;;     (concat nomis/repos-directory "blog-play-using-hugo/"))

;;   (defvar nomis/hugo-blog/org-directory
;;     (concat nomis/hugo-blog/base-directory "001-org/"))

;;   (defvar nomis/hugo-blog/site-directory
;;     (concat nomis/hugo-blog/base-directory "002-site/"))

;;   (defvar nomis/hugo-process "Hugo Server")
;;   (defvar nomis/hugo-server-site "http://localhost:1313/")

;;   (defmacro nomis/with-dir (DIR &rest FORMS)
;;     "Execute FORMS in DIR."
;;     (declare (indent 1))
;;     (let ((orig-dir (gensym)))
;;       `(progn (setq ,orig-dir default-directory)
;;               (cd ,DIR) ,@FORMS (cd ,orig-dir))))

;;   (defun nomis/publish-blog/hugo ()
;;     "Run hugo and push changes upstream."
;;     (interactive)
;;     (nomis/with-dir nomis/hugo-blog/site-directory
;;       (shell-command "rm -rf ./*")
;;       ;; (shell-command "git rm -rf ./*")
;;       ;; (shell-command "git clean -fxd")

;;       (nomis/with-dir nomis/hugo-blog/org-directory
;;         (->> nomis/hugo-blog/site-directory
;;              (concat "hugo -d ")
;;              shell-command))

;;       ;; (shell-command "git add .")
;;       ;; (--> (current-time-string)
;;       ;;      (concat "git commit -m \"" it "\"")
;;       ;;      (shell-command it))
;;       ;; (magit-push-current-to-upstream nil)
;;       ))

;;   (defun nomis/start-blog-server ()
;;     "Run hugo server if not already running and open its webpage."
;;     (interactive)
;;     (nomis/with-dir nomis/hugo-blog/org-directory

;;       (unless (get-process nomis/hugo-process)
;;         (start-process nomis/hugo-process nil "hugo" "server"))
;;       (browse-url nomis/hugo-server-site)))

;;   (defun nomis/end-blog-server ()
;;     "End hugo server process if running."
;;     (interactive)
;;     (--when-let (get-process nomis/hugo-process)
;;       (delete-process it)))

;;   ;; (spacemacs/set-leader-keys (kbd "ab") 'nomis/publish-blog/hugo)
;;   ;; (spacemacs/set-leader-keys (kbd "aa") 'nomis/start-blog-server)
;;   ;; (spacemacs/set-leader-keys (kbd "ae") 'nomis/end-blog-server)
;;   )

;;;; ___________________________________________________________________________

(provide 'nomis-org-publish)
