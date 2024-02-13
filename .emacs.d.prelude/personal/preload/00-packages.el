(add-to-list 'load-path (expand-file-name "~/.emacs.d/personal/preload"))

(require 'lerax)

(lerax-init-melpa)

;; NOTE: Sat 20 Jun 2020 05:21:37 PM -03
;; installing: yasnippet-snippets destroy all my software ↓ fuckups everything, please don't do that
;; docker-lerax related bug at bootstraping package install
(lerax-require-packages
 '(
   auctex
   company-c-headers
   company-quickhelp
   cov
   darkroom
   doom-themes
   flycheck
   gif-screencast
   git-commit
   hl-todo
   htmlize
   ix
   kaolin-themes
   linum-relative
   load-env-vars
   multiple-cursors
   neotree
   notmuch
   org-present
   org-ref
   org2jekyll
   ox-gfm
   pyvenv
   restclient
   simple-modeline
   slime-company
   ssh-agency
   toc-org
   treemacs
   visual-fill-column
   xclip
   yasnippet
   zeal-at-point
   )
 )
