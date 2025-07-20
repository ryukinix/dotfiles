(require 'org)
(require 'org2jekyll)

(defcustom lerax-blog-basepath
  (expand-file-name "~/Dropbox/Programming/Projects/Website/ryukinix.github.io")
  "My blog base path"
  :group 'lerax
  :type 'string)

;; setup of org2jekyll
(setq org2jekyll-blog-author "Manoel Vilela"
      org2jekyll-source-directory (concat lerax-blog-basepath "/org")
      org2jekyll-jekyll-directory lerax-blog-basepath
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
         :html-preamble t
         :recursive t
         :make-index t
         :html-extension "html"
         :body-only t)
        ("images"
         :base-directory ,(org2jekyll-input-directory "img")
         :base-extension "jpg\\|gif\\|png"
         :publishing-directory ,(org2jekyll-output-directory "assets/img")
         :publishing-function org-publish-attachment
         :recursive t)
        ("js"
         :base-directory ,(org2jekyll-input-directory "js")
         :base-extension "js"
         :publishing-directory ,(org2jekyll-output-directory "assets/js")
         :publishing-function org-publish-attachment
         :recursive t)
        ("css"
         :base-directory ,(org2jekyll-input-directory "css")
         :base-extension "css\\|el"
         :publishing-directory ,(org2jekyll-output-directory "assets/css")
         :publishing-function org-publish-attachment
         :recursive t)
        ("web" :components ("images" "js" "css"))))

(defun org-custom-link-img-follow (path)
  (org-open-file
   (format "../assets/posts/%s" path)))

(defun org-custom-link-img-export (path desc format)
  (cond
   ((eq format 'html)
    (format "<img src=\"/assets/posts/%s\" alt=\"%s\"/>" path desc))))

;; img:lisp-inference/lisp-inference.png
;; img:<post-name/<file>
(org-add-link-type "img" 'org-custom-link-img-follow 'org-custom-link-img-export)

;; active toc-org when open a buffer with org-mode enabled
(add-hook 'org-mode-hook 'toc-org-enable)

(define-key org2jekyll-mode-map (kbd "<f9>") 'org2jekyll-publish)
