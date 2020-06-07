;; DEAD CODE after update Fri 05 Jun 2020 09:46:13 AM -03
;; some shit about
;; Debugger entered--Lisp error: (void-function org-link-set-parameters)
;;   org-link-set-parameters("local" :export org2jekyll-local-link-export)
;;   byte-code("\301\302\300\303\"\210\304\305\306\307#\207" [org2jekyll-mode-off-hook nil add-hook #[nil "\300\301\302\"\207" [remove-hook org-publish-after-publishing-hook org2jekyll-install-yaml-headers] 3] org-link-set-parameters "local" :export org2jekyll-local-link-export] 4)
;;   require(org2jekyll)
;;   eval-buffer(#<buffer  *load*-500929> nil "/home/lerax/.emacs.d/personal/jekyll.el" nil t)  ; Reading at buffer position 146
;;   load-with-code-conversion("/home/lerax/.emacs.d/personal/jekyll.el" "/home/lerax/.emacs.d/personal/jekyll.el" nil nil)
;;   load("/home/lerax/.emacs.d/personal/jekyll.el")
;;   mapc(load ("/home/lerax/.emacs.d/personal/custom.el" "/home/lerax/.emacs.d/personal/emoji.el" "/home/lerax/.emacs.d/personal/exwm.el" "/home/lerax/.emacs.d/personal/hooks.el" "/home/lerax/.emacs.d/personal/jekyll.el" "/home/lerax/.emacs.d/personal/keybindings.el" "/home/lerax/.emacs.d/personal/latex.el" "/home/lerax/.emacs.d/personal/modes.el" "/home/lerax/.emacs.d/personal/scala.el" "/home/lerax/.emacs.d/personal/spellchecker.el"))
;;   (progn (message "Loading personal configuration files in %s..." prelude-personal-dir) (mapc (quote load) (delete prelude-modules-file (directory-files prelude-personal-dir (quote t) "^[^#.].*\\.el$"))))
;;   (if (file-exists-p prelude-personal-dir) (progn (message "Loading personal configuration files in %s..." prelude-personal-dir) (mapc (quote load) (delete prelude-modules-file (directory-files prelude-personal-dir (quote t) "^[^#.].*\\.el$")))))
;;   eval-buffer(#<buffer  *load*> nil "/home/lerax/.emacs.d/init.el" nil t)  ; Reading at buffer position 6260
;;   load-with-code-conversion("/home/lerax/.emacs.d/init.el" "/home/lerax/.emacs.d/init.el" t t)
;;   load("/home/lerax/.emacs.d/init" t t)

;; (require 'prelude-packages nil t)

;; (prelude-require-package 'org2jekyll)
;; (prelude-require-package 'toc-org)

;; (require 'org)
;; (require 'org2jekyll)

;; ;; setup of org2jekyll
;; (setq org2jekyll-blog-author "Manoel Vilela"
;;       org2jekyll-source-directory (expand-file-name "~/Dropbox/Programming/Projects/Website/ryukinix.github.io/org")
;;       org2jekyll-jekyll-directory (expand-file-name "~/Dropbox/Programming/Projects/Website/ryukinix.github.io")
;;       org2jekyll-jekyll-drafts-dir ""
;;       org2jekyll-jekyll-posts-dir "_posts/"
;;       org-publish-project-alist
;;       `(("default"
;;          :base-directory ,(org2jekyll-input-directory)
;;          :base-extension "org"
;;          :publishing-directory ,(org2jekyll-output-directory)
;;          :publishing-function org-html-publish-to-html
;;          :headline-levels 4
;;          :section-numbers nil
;;          :with-toc nil
;;          :html-head "<link rel=\"stylesheet\" href=\"./css/style.css\" type=\"text/css\"/>"
;;          :html-preamble t
;;          :recursive t
;;          :make-index t
;;          :html-extension "html"
;;          :body-only t)
;;         ("post"
;;          :base-directory ,(org2jekyll-input-directory)
;;          :base-extension "org"
;;          :publishing-directory ,(org2jekyll-output-directory org2jekyll-jekyll-posts-dir)
;;          :publishing-function org-html-publish-to-html
;;          :headline-levels 4
;;          :section-numbers nil
;;          :with-toc nil
;;          :html-head "<link rel=\"stylesheet\" href=\"./css/style.css\" type=\"text/css\"/>"
;;          :html-preamble t
;;          :recursive t
;;          :make-index t
;;          :html-extension "html"
;;          :body-only t)
;;         ("images"
;;          :base-directory ,(org2jekyll-input-directory "img")
;;          :base-extension "jpg\\|gif\\|png"
;;          :publishing-directory ,(org2jekyll-output-directory "assets/img")
;;          :publishing-function org-publish-attachment
;;          :recursive t)
;;         ("js"
;;          :base-directory ,(org2jekyll-input-directory "js")
;;          :base-extension "js"
;;          :publishing-directory ,(org2jekyll-output-directory "assets/js")
;;          :publishing-function org-publish-attachment
;;          :recursive t)
;;         ("css"
;;          :base-directory ,(org2jekyll-input-directory "css")
;;          :base-extension "css\\|el"
;;          :publishing-directory ,(org2jekyll-output-directory (concat org2jekyll-jekyll-posts-dir
;;                                                                       "assets/css"))
;;          :publishing-function org-publish-attachment
;;          :recursive t)
;;         ("web" :components ("images" "js" "css"))))

;; ;; active toc-org when open a buffer with org-mode enabled
;; (add-hook 'org-mode-hook 'toc-org-enable)
