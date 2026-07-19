;;; -*- lexical-binding: t -*-
;;; modes.el --- Package configurations via use-package

(require 'lerax)

;; Core settings
(setq delete-by-moving-to-trash t)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq python-shell-completion-native-enable nil)
(setq initial-scratch-message
      "#  ▓█████  ███▄ ▄███▓ ▄▄▄       ▄████▄    ██████\n# ▓█   ▀ ▓██▒▀█▀ ██▒▒████▄    ▒██▀ ▀█  ▒██    ▒\n# ▒███   ▓██    ▓██░▒██  ▀█▄  ▒▓█    ▄ ░ ▓██▄\n# ▒▓█  ▄ ▒██    ▒██ ░██▄▄▄▄██ ▒▓▓▄ ▄██▒  ▒   ██▒\n# ░▒████▒▒██▒   ░██▒ ▓█   ▓██▒▒ ▓███▀ ░▒██████▒▒\n# ░░ ▒░ ░░ ▒░   ░  ░ ▒▒   ▓▒█░░ ░▒ ▒  ░▒ ▒▓▒ ▒ ░\n# ░ ░  ░░  ░      ░  ▒   ▒▒ ░  ░  ▒   ░ ░▒  ░ ░\n# ░   ░      ░     ░   ▒   ░        ░  ░  ░\n# ░  ░       ░         ░  ░░ ░            ░\n")
(setq initial-major-mode 'org-mode)
(global-prettify-symbols-mode +1)
(setq confirm-nonexistent-file-or-buffer nil)
(setq ido-create-new-buffer 'always)
(setq inhibit-startup-message t inhibit-startup-echo-area-message t)
(setq kill-buffer-query-functions (remq 'process-kill-buffer-query-function kill-buffer-query-functions))
(defalias 'halt-and-catch-fire #'save-buffers-kill-emacs)
(setq-default prelude-flyspell nil)
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(setq make-backup-files nil)
(setq prelude-guru nil)
(setq projectile-project-search-path '("/home/lerax/Desktop/workspace/"))
(when (version<= "26.1" emacs-version) (setq confirm-kill-processes nil))
(global-display-line-numbers-mode -1)
(menu-bar-mode -1)
(simple-modeline-mode +1)
(super-save-mode +1)
(setq cursor-type 'bar)
(which-function-mode -1)
(setq-default notmuch-search-oldest-first nil)
(setq server-client-instructions nil)
(remove-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
(when (eq system-type 'gnu/linux)
  (setq x-select-enable-clipboard-manager nil))
(when (eq system-type 'windows-nt)
  (put 'gdb 'disabled t))

;; Packages config
(use-package org
  :bind (("<f9>" . org-latex-export-to-pdf)
         ("C-M-<return>" . (lambda ()
                             (interactive)
                             (end-of-buffer)
                             (org-insert-heading)
                             (crux-insert-date))))
  :config
  (require 'org-mouse)
  (require 'org-agenda)
  (setq org-src-fontify-natively t)
  (setq org-todo-keywords '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((emacs-lisp . t)
                                 (python . t)
                                 (shell . t))))

(with-eval-after-load 'org
  (require 'ox-gfm nil t)
  (require 'ob-latex)
  (setq org-latex-listings 'minted)
  (setq org-latex-minted-options '(("frame" "lines") ("linenos" "true"))
        org-preview-latex-default-process 'imagemagick)
  (plist-put org-format-latex-options :scale 1.2))

(use-package ox-beamer
  :ensure nil
  :defer t
  :after org
  :bind (:map org-beamer-mode-map
         ("<f9>" . org-beamer-export-to-pdf))
  :config
  (require 'ox-beamer))

(use-package python
  :bind (:map inferior-python-mode-map
         ("C-c C-z" . other-window))
  :config
  (setq python-shell-completion-native-enable nil)
  (when (package-installed-p 'python-black)
    (define-key python-mode-map (kbd "C-c C-f") 'python-black-buffer)))

(use-package wakatime-mode
  :if (package-installed-p 'wakatime-mode)
  :config (global-wakatime-mode))

(use-package xclip
  :if (executable-find "xclip")
  :config (xclip-mode +1))

(use-package flycheck
  :config
  (remove-hook 'python-mode-hook 'pylint-add-menu-items)
  (remove-hook 'python-mode-hook 'pylint-add-key-bindings)
  (setq-default flycheck-disabled-checkers '(python-pylint emacs-lisp-checkdoc))
  (setq-default flycheck-scheme-chicken-executable "chicken-csc"))

(use-package geiser
  :config
  (setq-default geiser-chicken-binary "chicken-csi")
  (setq-default geiser-active-implementations '(chicken racket guile chez mit chibi))
  (add-hook 'geiser-repl-mode-hook #'smartparens-mode))

(use-package magit
  :defer t
  :config
  (setq magit-blame-echo-style 'headings)
  (defun lerax-dotfiles () (interactive) (magit "~/"))
  (defun lerax-magit-process-environment (env)
    (when (equal default-directory (expand-file-name "~/"))
     (let* ((default (file-name-as-directory (expand-file-name default-directory)))
            (git-dir (expand-file-name "~/.dot/"))
            (work-tree (expand-file-name "~/")))
       (push (format "GIT_WORK_TREE=%s" work-tree) env)
       (push (format "GIT_DIR=%s" git-dir) env)))
    env)
  (advice-add 'magit-process-environment :filter-return #'lerax-magit-process-environment))

(use-package git-commit
  :defer t
  :config (global-git-commit-mode +1))
  
(use-package magit-delta
  :if (executable-find "delta")
  :hook (magit-mode . magit-delta-mode))

(use-package multiple-cursors
  :bind (("C-S-<mouse-1>" . mc/add-cursor-on-click)
         ("C-M->" . mc/mark-next-like-this)
         ("C-M-<" . mc/mark-previous-like-this)
         ("C->" . mc/mark-next-like-this-word)
         ("C-<" . mc/mark-previous-like-this-word)
         ("C-S-l" . mc/edit-lines)
         ("C-ç" . mc/skip-to-next-like-this)
         ("C-M-ç" . mc/skip-to-previous-like-this)
         ("C-c C->" . mc/mark-all-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("C-c <" . mc/mark-previous-like-this-word)
         ("C-c >" . mc/mark-next-like-this-word))
  :config
  (setq mc/always-run-for-all '(crux-kill-whole-line
                                forward-sentence
                                sp-backward-delete-char
                                sp-delete-char
                                sp-forward-sexp
                                sp-kill-word))
  (setq mc/cmds-to-run-once '(execute-extended-command)))

(use-package treemacs
  :bind (("C-x t" . treemacs)
         ("C-x T" . (lambda ()
                      (interactive)
                      (let* ((workspace (treemacs-current-workspace))
                             (current-project (string-trim-right (projectile-project-root) "\/"))
                             (projects (treemacs-workspace->projects workspace)))
                        (unless (cl-find current-project (mapcar #'treemacs-project->path projects) :test #'equal)
                          (condition-case nil
                              (progn
                                (treemacs-add-project-to-workspace current-project)
                                (message "treemacs project added: '%s'" current-project))
                            (error nil)))
                        (treemacs--follow)
                        (treemacs)))))
  :config
  (define-key treemacs-mode-map [drag-mouse-1] nil))

(use-package neotree
  :bind (("C-x y" . neotree-toggle)
         ("C-x Y" . (lambda () (interactive) (neotree-refresh) (neotree)))))

(use-package darkroom
  :bind ("<S-f11>" . darkroom-tentative-mode)
  :hook (darkroom-tentative-mode . (lambda () (setq-local truncate-lines t)))
  :config (setq-default darkroom-text-scale-increase 1.2))

(use-package projectile
  :bind (("C-c p" . projectile-command-map)
         ("<f9>" . projectile-compile-project)
         ("M-<f9>" . projectile-test-project)
         :map projectile-mode-map
         ("C-c C-p" . nil))
  :config
  (custom-set-default 'projectile-keymap-prefix (kbd "C-c p")))

(use-package slime)

(use-package slime-repl
  :ensure nil
  :defer t
  :bind (:map slime-repl-mode-map
         ("C-c C-z" . (lambda () (interactive) (select-window (previous-window))))))

(use-package yasnippet
  :defer t
  :config (yas-global-mode +1)
  :bind (:map yas-minor-mode-map
         ("C-<return>" . yas-expand)
         ("M-<return>" . yas-insert-snippet)))

(use-package vterm
  :defer t
  :bind ("C-c T" . vterm))

(use-package zeal-at-point
  :defer t
  :bind ("C-z" . zeal-at-point))

(use-package lisp-interaction-mode
  :ensure nil
  :defer t
  :bind (:map lisp-interaction-mode-map
         ("C-c C-z" . prelude-visit-ielm)))

;; C-x C-d from helm needs replacement for Vertico, map to project-find-dir
(global-set-key (kbd "C-x C-d") 'project-find-dir)

(provide 'modes)
;;; modes.el ends here
