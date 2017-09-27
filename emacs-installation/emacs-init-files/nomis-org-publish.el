;;;; Init stuff -- Org Mode.

;;;; ___________________________________________________________________________

(require 'ox-publish)

(setq org-export-with-toc nil)

(defvar nomis/repos-directory "~/development-100/repositories/nomis/")

(defvar nomis/jekyll-blog/base-directory
  (concat nomis/repos-directory "blog-play-using-jekyll/"))

(defvar nomis/jekyll-blog/org-directory
  (concat nomis/jekyll-blog/base-directory "001-org/"))

(defvar nomis/jekyll-blog/jekyll-input-directory
  (concat nomis/jekyll-blog/base-directory "002-jekyll-input/"))

(setq org-publish-project-alist
      `(("org-notes"
         :base-directory ,nomis/jekyll-blog/org-directory
         :base-extension "org"
         :publishing-directory ,nomis/jekyll-blog/jekyll-input-directory
         :recursive t
         :publishing-function org-html-publish-to-html
         :headline-levels 4 ; Just the default for this project.
         :body-only t)
        ("org-static"
         :base-directory ,nomis/jekyll-blog/org-directory
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
         :publishing-directory ,nomis/jekyll-blog/jekyll-input-directory
         :recursive t
         :publishing-function org-publish-attachment)
        ("nomis-jekyll-blog" :components ("org-notes" "org-static"))))

(defun nomis/publish-blog/jekyll (force?)
  (interactive "P")
  (org-publish-project "nomis-jekyll-blog" force?))

;;;; ___________________________________________________________________________

(provide 'nomis-org-publish)
