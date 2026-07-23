;;; -*- lexical-binding: t -*-
;;; config.el --- Packages configuration, modes and features

(require 'lerax)

;; --- From modes.el (use-package blocks) ---
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
  :demand t
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
  :demand t
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

(use-package slime
  :demand t
  :config
  (slime-setup '(slime-asdf slime-quicklisp slime-fancy)))

(use-package slime-repl
  :after slime
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

;; --- From hooks.el (use-package rewrites) ---
(use-package gud
  :defer t)
(use-package compile
  :defer t)
(use-package gdb-mi
  :defer t)

(use-package helm-projectile
  :defer t
  :config
  (defvar helm-source-file-not-found
    (helm-build-dummy-source "Create file"
      :action (lambda (cand) (find-file cand))))
  (add-to-list 'helm-projectile-sources-list helm-source-file-not-found t))

(use-package with-editor
  :defer t
  :hook (with-editor-mode . (lambda () (whitespace-toggle-options 'tabs))))

(use-package go-mode
  :defer t
  :bind (:map go-mode-map
         ("M-." . godef-jump)
         ("C-M-." . godef-jump-other-window))
  :hook (go-mode . (lambda () (whitespace-toggle-options 'lines-tail))))

(use-package pyvenv
  :defer t)

(use-package python
  :defer t
  :hook ((python-mode . lerax-python-venv-auto-activate)
         (python-mode . pyvenv-mode)))

(use-package org
  :defer t
  :hook (org-mode . (lambda ()
                      (whitespace-toggle-options 'lines-tail)
                      (auto-fill-mode))))

(use-package markdown-mode
  :defer t
  :hook (markdown-mode . (lambda ()
                           (whitespace-toggle-options 'lines-tail)
                           (auto-fill-mode))))

;; --- From erc.el ---
(with-eval-after-load 'erc
  (require 'erc-join)
  (require 'erc-log)
  (add-to-list 'erc-modules 'log)
  (add-to-list 'erc-modules 'notifications) ;; enable notifications -- only if have dbus
  (erc-update-modules)

  ; (setq erc-fill-function 'erc-fill-static)
  ; (setq erc-fill-static-center 22)
  (setq erc-hide-list '("JOIN" "PART" "QUIT"))
  (setq erc-lurker-hide-list '("JOIN" "PART" "QUIT"))
  (setq erc-lurker-threshold-time 43200)
  (setq erc-prompt-for-nickserv-password nil)
  (setq erc-prompt-for-password nil)
  (setq erc-prompt-for-channel-key nil)
  (setq erc-server-reconnect-attempts 5)
  (setq erc-server-reconnect-timeout 3)
  (setq erc-track-exclude-types
        '("JOIN" "MODE" "NICK" "PART" "QUIT"
          "324" "329" "332" "333" "353" "477"))
  (setq erc-log-write-after-insert t)
  (setq erc-log-write-after-send t)
  (setq erc-log-insert-log-on-open t)
  (setq erc-default-server "irc.libera.chat")
  (setq erc-prompt "λ>")
  (setq erc-nick '("lerax" "ryukinix"))
  (setq erc-save-buffer-on-part nil)
  (setq erc-save-queries-on-quit nil)
  (erc-autojoin-mode +1)
  (setq erc-autojoin-channels-alist
        '(("libera.chat" "#emacs-social" "#lisp" "#linux" "#emacs")
          ("oftc.net" "#bolhadev"))
        erc-autojoin-timing 'ident
        erc-autojoin-delay 10))

(use-package erc-hl-nicks
  :after erc)

(use-package erc-image
  :after erc)

(defun erc-bolhadev ()
  (interactive)
  ;; oftc doesn't support SASL auth plain!
  ;; for some reason auth-source is not working properly for oftc as well
  (erc-tls :server "irc.oftc.net"
           :nick "ryukinix"
           :port 6697))

;; --- From find-header.el ---
;;; find-header.el --- Find header of C/C++ files
;; Author: Tatsuhiko Kubo
;; This elisp can open header file on current line.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.


;;; Commentary:
;; This is useful as fuck, but would be nice bind the M-, to go back
;; on stack pointer cursor.  That way calling nested find-header
;; would not be messy.

(require 'cc-mode)
;;; Code:

(setq-default find-header-file-header-file-prefixes (list "/usr/include/"
                                                          "/usr/local/include/"
                                                          "/usr/local/gcc-15.2.0/include/c++/15.2.0/"))

(defun find-header-file-current-char ()
  "Get the next char after point and return as string."
  (char-to-string (char-after (point))))

(defun find-header-file-current-line-string ()
  "Get current line as string."
  (let ((line-string ""))
    (save-excursion
      (while (not (bolp))
       (backward-char))
      (while (not (eolp))
       (setq line-string (concat line-string (find-header-file-current-char)))
       (forward-char)))
    line-string))

(defun find-header-file-buffer-on-path (prefix-list filename)
  "Get path of the header from PREFIX-LIST paths based on FILENAME string."
  (if (null (car prefix-list))
      nil
    (if (file-exists-p (concat (car prefix-list) filename))
       (find-file-noselect (concat (car prefix-list) filename))
      (find-header-file-buffer-on-path (cdr prefix-list) filename))))

(defun find-header-file ()
  "Open the buffer of the current cursor line header."
  (interactive)
  (let ((current-line-string (find-header-file-current-line-string))
        (header-file-buffer nil))
    (cond ((string-match "^\\s-*#\\s-*include\\s-*<\\s-*\\([^< ]+\\)\\s-*>" current-line-string)
           (let ((header-file-path (match-string 1 current-line-string)))
             (setq header-file-buffer (find-header-file-buffer-on-path find-header-file-header-file-prefixes
                                                                       header-file-path))))
          ((string-match "^\\s-*#\\s-*include\\s-*\"\\([^\"]+\\)\"\\s-*" current-line-string)
           (let* ((header-file-path (match-string 1 current-line-string))
                  (buffer           (if (file-exists-p (concat default-directory header-file-path))
                                        (find-file-noselect (concat default-directory header-file-path))
                                      nil)))
             (setq header-file-buffer buffer)
             (if (null header-file-buffer)
                 (setq header-file-buffer (find-header-file-buffer-on-path find-header-file-header-file-prefixes
                                                                           header-file-path))
               nil)))
          (t nil))
    (if (null header-file-buffer)
        (message "not found header file")
      (prog2 (xref-push-marker-stack)
       (switch-to-buffer header-file-buffer)))))

;; binding keys for C and C++ to C-c C-. on `find-header-file' function
(cl-loop for mode in (list c++-mode-map c-mode-map)
         do (progn
              (define-key mode (kbd "M-s-.") 'find-header-file)
              (define-key mode (kbd "M-s-,") 'xref-go-back)))

;; --- From gpt.el ---
;;; gpt.el --- AI and GPT integrations

(use-package gptel
  :defer t
  :bind (("C-s-t" . gptel-rewrite)
         ("C-M-S-g" . gptel))
  :config
  (setq gptel-default-mode 'org-mode
        gptel-api-key (getenv "OPENAI_KEY")
        gptel-model 'gemini-3-pro-preview
        gptel-backend (gptel-make-gemini "Gemini"
                        :key (getenv "GEMINI_KEY")
                        :stream t)))

(use-package agent-shell
  :ensure t
  :config
  (setq agent-shell-google-authentication
        (agent-shell-google-make-authentication :api-key (lambda () (getenv "GEMINI_KEY")))))

;; --- From jekyll.el ---
;;; jekyll.el --- Org to Jekyll publishing setup

(defcustom lerax-blog-basepath
  (expand-file-name "~/Dropbox/Programming/Projects/Website/ryukinix.github.io")
  "My blog base path"
  :group 'lerax
  :type 'string)

(use-package toc-org
  :hook (org-mode . toc-org-enable))

(use-package org2jekyll
  :defer t
  :bind (:map org2jekyll-mode-map
         ("<f9>" . org2jekyll-publish))
  :config
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
          ("web" :components ("images" "js" "css")))))

(with-eval-after-load 'org
  (defun org-custom-link-img-follow (path)
    (org-open-file (format "../assets/posts/%s" path)))

  (defun org-custom-link-img-export (path desc format)
    (cond
     ((eq format 'html)
      (format "<img src=\"/assets/posts/%s\" alt=\"%s\"/>" path desc))))

  (org-link-set-parameters "img"
                           :follow 'org-custom-link-img-follow
                           :export 'org-custom-link-img-export))

(defun fix-tuareg-background-at-export ()
  (require 'tuareg)
  (copy-face 'font-lock-type-face 'tuareg-font-lock-constructor-face)
  (set-face-attribute 'tuareg-font-lock-constructor-face nil :background nil))

(add-hook 'htmlize-before-hook #'fix-tuareg-background-at-export)

;; --- From latex.el ---
;;; latex.el --- LaTeX and Org-mode PDF exports

(defcustom lerax-latex-listing 'minted
  "If `lerax-latex-listing' is minted enable syntax highlight"
  :group 'lerax
  :type 'symbol)



(use-package org
  :defer t
  :config
  (require 'ox-latex)
  (require 'oc-biblatex)
  (require 'oc-natbib)
  (require 'oc-csl)
  (require 'ob-latex)
  (require 'ox-gfm nil t)
  (setq org-latex-listings lerax-latex-listing
        org-latex-minted-options '(("frame" "lines")
                                   ("linenos" "true"))
        org-preview-latex-default-process 'imagemagick)
  (plist-put org-format-latex-options :scale 1.2))

(defun clean-export-pdf (&rest _)
  (let* ((fname (file-name-base (buffer-name)))
        (pattern (format "%s.!(pdf|org)" fname))
        (cmd (format "bash -c 'shopt -s extglob; rm -rf %s &'" pattern)))
   (call-process-shell-command cmd nil 0)))

(advice-add 'org-latex-export-to-pdf :after #'clean-export-pdf)
(advice-add 'org-beamer-export-to-pdf :after #'clean-export-pdf)

;; --- From presentation.el ---
;;; presentation.el --- Org-present configuration for presentations


(use-package visual-fill-column
  :defer t
  :config
  (setq-default visual-fill-column-center-text t)
  (setq visual-fill-column-width 110
        visual-fill-column-center-text t))

(use-package org-present
  :defer t
  :bind (:map org-mode-map
         ("C-<f11>" . org-present)
         :map org-present-mode-keymap
         ("C-<f11>" . org-present-quit))
  :hook ((org-present-mode . lerax-org-present-start)
         (org-present-mode-quit . lerax-org-present-end)
         (org-present-after-navigate-functions . lerax-org-present-prepare-slide))
  :config
  (setq org-hide-emphasis-markers t)
  (setq-default org-image-actual-width nil)

  (defun lerax-org-present-prepare-slide (buffer-name heading)
    (org-overview)
    (org-show-entry)
    (org-show-children))

  (defun lerax-org-present-start ()
    (setq header-line-format " ")
    (org-display-inline-images)
    (text-scale-increase +3)
    (setq mode-line-format nil)
    (visual-fill-column-mode 1)
    (visual-line-mode 1))

  (defun lerax-org-present-end ()
    (setq header-line-format nil)
    (org-remove-inline-images)
    (text-scale-reset)
    (setq mode-line-format '(:eval simple-modeline--mode-line))
    (visual-fill-column-mode 0)
    (visual-line-mode 0)))

;; --- From scala.el ---
;;; scala.el --- Scala environment configuration


(use-package sbt-mode
  :defer t
  :commands (sbt-start sbt-command)
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
  ;; sbt-supershell kills sbt-mode: https://github.com/hvesalai/emacs-sbt-mode/issues/152
  (setq sbt:program-options '("-Dsbt.supershell=false")))

(use-package scala-mode
  :defer t
  :hook ((scala-mode . eglot-ensure)
         (scala-mode . (lambda ()
                         (whitespace-toggle-options 'lines-tail)
                         (setq-local flycheck-check-syntax-automatically
                                     '(save idle-change new-line mode-enabled))))))

(defun scalafmt ()
  (interactive)
  (let ((command "scalafmt")
        (current-file (buffer-file-name (current-buffer))))
    (shell-command (format "%s %s" command current-file))))

;; --- From todo.el ---
(defun projectile-todo ()
  (interactive)
  (projectile-ripgrep "\\b(TODO|FIXME)\\b" t))

(defun projectile-todo-all ()
  (interactive)
  (let ((pattern (string-join (map 'list 'car hl-todo-keyword-faces) "|")))
    (projectile-ripgrep (format "\\b(%s)\\b"pattern) t)))
