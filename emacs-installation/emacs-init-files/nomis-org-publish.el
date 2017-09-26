;;;; ________ * Init stuff -- Org Mode.

(require 'ox-publish)

(defvar nomis/org-jekyll-base-directory
  "~/org/public-persona/blog-using-jekyll")

(defvar nomis/org-publish-base-directory
  (concat nomis/org-jekyll-base-directory "/in"))

(defvar nomis/org-publish-publishing-directory
  (concat nomis/org-jekyll-base-directory "/public_html/"))

(setq org-publish-project-alist
      `(("org-notes"
         :base-directory ,nomis/org-publish-base-directory
         :base-extension "org"
         :publishing-directory ,nomis/org-publish-publishing-directory
         :recursive t
         :publishing-function org-html-publish-to-html
         :headline-levels 4 ; Just the default for this project.
         :auto-preamble t)
        ("org-static"
         :base-directory ,nomis/org-publish-base-directory
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
         :publishing-directory ,nomis/org-publish-publishing-directory
         :recursive t
         :publishing-function org-publish-attachment)
        ("nomis-jekyll-blog" :components ("org-notes" "org-static"))))

(defun nomis/publish-blog/jekyll (force?)
  (interactive "P")
  (org-publish-project "nomis-jekyll-blog" force?))

;;;; ___________________________________________________________________________

(provide 'nomis-org-publish)
