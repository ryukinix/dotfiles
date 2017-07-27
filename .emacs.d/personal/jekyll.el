(require 'org)
(require 'org2jekyll)
(require 'prelude-packages)

(prelude-require-package 'org2jekyll)
(prelude-require-package 'toc-org)
;; setup of org2jekyll
(setq org2jekyll-blog-author "Manoel Vilela"
      org2jekyll-source-directory (expand-file-name "~/Dropbox/Programming/Projects/Website/Org")
      org2jekyll-jekyll-directory (expand-file-name "~/Dropbox/Programming/Projects/Website/ryukinix.github.io")
      org2jekyll-jekyll-drafts-dir ""
      org2jekyll-jekyll-posts-dir "_posts/"
      org-publish-project-alist
      `(("default"
         :base-directory ,(org2jekyll-input-directory)
         :base-extension "org"
         :publishing-directory ,(org2jekyll-output-directory)
         :publishing-function org-html-publish-to-html
         :headline-levels 4
         :section-numbers nil
         :with-toc nil
         :html-head "<link rel=\"stylesheet\" href=\"./css/style.css\" type=\"text/css\"/>"
         :html-preamble t
         :recursive t
         :make-index t
         :html-extension "html"
         :body-only t)
        ("post"
         :base-directory ,(org2jekyll-input-directory)
         :base-extension "org"
         :publishing-directory ,(org2jekyll-output-directory org2jekyll-jekyll-posts-dir)
         :publishing-function org-html-publish-to-html
         :headline-levels 4
         :section-numbers nil
         :with-toc nil
         :html-head "<link rel=\"stylesheet\" href=\"./css/style.css\" type=\"text/css\"/>"
         :html-preamble t
         :recursive t
         :make-index t
         :html-extension "html"
         :body-only t)
        ("images"
         :base-directory ,(org2jekyll-input-directory "img")
         :base-extension "jpg\\|gif\\|png"
         :publishing-directory ,(org2jekyll-output-directory "img")
         :publishing-function org-publish-attachment
         :recursive t)
        ("js"
         :base-directory ,(org2jekyll-input-directory "js")
         :base-extension "js"
         :publishing-directory ,(org2jekyll-output-directory "js")
         :publishing-function org-publish-attachment
         :recursive t)
        ("css"
         :base-directory ,(org2jekyll-input-directory "css")
         :base-extension "css\\|el"
         :publishing-directory ,(org2jekyll-output-directory "css")
         :publishing-function org-publish-attachment
         :recursive t)
        ("web" :components ("images" "js" "css"))))

;; active toc-org when open a buffer with org-mode enabled
(add-hook 'org-mode-hook 'toc-org-enable)
