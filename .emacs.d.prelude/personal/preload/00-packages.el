(add-to-list 'load-path (expand-file-name "~/.emacs.d/personal/preload"))

(require 'lerax)

(lerax-init-melpa)

;; NOTE: Sat 20 Jun 2020 05:21:37 PM -03
;; installing: yasnippet-snippets destroy all my software ↓ fuckups everything, please don't do that
;; docker-lerax related bug at bootstraping package install
(lerax-require-packages
 '(
   company-c-headers
   company-quickhelp
   darkroom
   doom-themes
   flycheck
   htmlize
   ix
   kaolin-themes
   linum-relative
   multiple-cursors
   neotree treemacs
   notmuch
   org2jekyll
   ox-gfm
   pyvenv
   pyvenv
   restclient
   simple-modeline
   slime-company
   ssh-agency
   toc-org
   xclip
   yasnippet
   org-ref
   auctex
   org-present
   visual-fill-column
   gif-screencast
   zeal-at-point
   )
 )
