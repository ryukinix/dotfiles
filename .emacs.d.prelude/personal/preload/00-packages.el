;;; -*- lexical-binding: t -*-
;;; 00-packages.el --- Setup Melpa and Ensure packages

(add-to-list 'load-path (expand-file-name "~/.emacs.d/personal/preload"))

(require 'lerax)
(require 'use-package) ; Native in 29.1+



(defun lerax-require-packages-run ()
  (lerax-require-packages
   '(tok-theme
     gpt
     ag
     auctex
     cov
     darkroom
     doom-themes
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
     notmuch
     org-present
     org-ref
     org2jekyll
     ox-gfm
     pkg-info
     pyvenv
     restclient
     simple-modeline
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
     ripgrep
     ;; treesit local plugins
     treesit-auto)))

(lerax-require-packages-run)
(lerax-load-init-env-if-exists)

(setq use-package-always-ensure t)

(setq prelude-welcome-screen nil) ; disable welcome screen, I want my scratch buffer!
