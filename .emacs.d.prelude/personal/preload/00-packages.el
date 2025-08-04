(add-to-list 'load-path (expand-file-name "~/.emacs.d/personal/preload"))

(require 'lerax)
(require 'use-package)

(lerax-init-melpa)

(defun lerax-require-packages-run ()
  ;; NOTE: Sat 20 Jun 2020 05:21:37 PM -03
  ;; installing: yasnippet-snippets destroy all my software ↓ fuckups everything, please don't do that
  ;; docker-lerax related bug at bootstraping package install
  (lerax-require-packages
   '(
    ;; sly ;; conflict with slime using prelude, see: https://github.com/bbatsov/prelude/issues/1440
    ;; sly-quicklisp
     tok-theme ;; for my own light mode
     gpt
     ag ;; not sure why I use this package
     auctex
     company-c-headers
     company-quickhelp
     cov
     darkroom
     doom-themes
     flycheck
     gif-screencast
     git-modes
     helm-bibtex
     htmlize
     ix
     json-reformat
     kaolin-themes
     linum-relative
     load-env-vars
     multiple-cursors
     neotree
     nlinum
     notmuch
     org-present
     org-ref
     org2jekyll
     ox-gfm
     pkg-info
     pyvenv
     restclient
     simple-modeline
     slime-company
     slime-company
     ssh-agency
     toc-org
     treemacs
     treemacs-projectile
     visual-fill-column
     wakatime-mode
     which-key
     whitespace-cleanup-mode
     xclip
     yasnippet
     zeal-at-point
     )
   ))

(lerax-require-packages-run)
(lerax-load-init-env-if-exists)

(setq use-package-always-ensure t)
