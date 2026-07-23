;;; -*- lexical-binding: t -*-
;;; config.el --- Packages configuration, modes and features

(require 'lerax)

;; C-x C-d from helm needs replacement for Vertico, map to project-find-dir
(global-set-key (kbd "C-x C-d") 'project-find-dir)

;;; UI & Editing

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

(use-package darkroom
  :bind ("<S-f11>" . darkroom-tentative-mode)
  :hook (darkroom-tentative-mode . (lambda () (setq-local truncate-lines t)))
  :config (setq-default darkroom-text-scale-increase 1.2))

(use-package visual-fill-column
  :defer t
  :config
  (setq-default visual-fill-column-center-text t)
  (setq visual-fill-column-width 110
        visual-fill-column-center-text t))

(use-package smartparens
  :demand t
  :config
  (sp-use-paredit-bindings))

(use-package wakatime-mode
  :demand t
  :if (package-installed-p 'wakatime-mode)
  :config (global-wakatime-mode))

(use-package emacs
  :demand t
  :hook (after-make-frame-functions . lerax-set-emoji-font)
  :config
  (defun lerax-set-emoji-font (frame)
    "Adjust the font settings of FRAME so Emacs can display emoji properly."
    (when (fboundp 'set-fontset-font)
      (if (eq system-type 'darwin)
          (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") frame 'prepend)
        (set-fontset-font t 'symbol (font-spec :family "Symbola") frame 'prepend))))
  (lerax-set-emoji-font nil))

;;; Project & Tools

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

(use-package projectile
  :bind (("C-c p" . projectile-command-map)
         ("<f9>" . projectile-compile-project)
         ("M-<f9>" . projectile-test-project)
         :map projectile-mode-map
         ("C-c C-p" . nil))
  :demand t
  :config
  (custom-set-default 'projectile-keymap-prefix (kbd "C-c p"))

  (defun projectile-todo ()
    (interactive)
    (projectile-ripgrep "\\b(TODO|FIXME)\\b" t))

  (defun projectile-todo-all ()
    (interactive)
    (let ((pattern (string-join (map 'list 'car hl-todo-keyword-faces) "|")))
      (projectile-ripgrep (format "\\b(%s)\\b"pattern) t))))

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

(use-package yasnippet
  :demand t
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

(use-package xclip
  :demand t
  :if (executable-find "xclip")
  :config (xclip-mode +1))

;;; Languages & Development

(use-package python
  :bind (:map inferior-python-mode-map
              ("C-c C-z" . other-window))
  :hook ((python-mode . lerax-python-venv-auto-activate)
         (python-mode . pyvenv-mode))
  :config
  (setq python-shell-completion-native-enable nil)
  (when (package-installed-p 'python-black)
    (define-key python-mode-map (kbd "C-c C-f") 'python-black-buffer)))
(use-package go-mode
  :defer t
  :bind (:map go-mode-map
         ("M-." . godef-jump)
         ("C-M-." . godef-jump-other-window))
  :hook (go-mode . (lambda () (whitespace-toggle-options 'lines-tail))))

(use-package scala-mode
  :defer t
  :hook ((scala-mode . eglot-ensure)
         (scala-mode . (lambda ()
                         (whitespace-toggle-options 'lines-tail)
                         (setq-local flycheck-check-syntax-automatically
                                     '(save idle-change new-line mode-enabled)))))
  :config
  (defun scalafmt ()
    (interactive)
    (let ((command "scalafmt")
          (current-file (buffer-file-name (current-buffer))))
      (shell-command (format "%s %s" command current-file)))))

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

(use-package geiser
  :config
  (setq-default geiser-chicken-binary "chicken-csi")
  (setq-default geiser-active-implementations '(chicken racket guile chez mit chibi))
  (add-hook 'geiser-repl-mode-hook #'smartparens-mode))

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

(use-package lisp-interaction-mode
  :ensure nil
  :defer t
  :bind (:map lisp-interaction-mode-map
         ("C-c C-z" . prelude-visit-ielm)))

;; C-x C-d from helm needs replacement for Vertico, map to project-find-dir
(global-set-key (kbd "C-x C-d") 'project-find-dir)

(use-package gud
  :defer t)

(use-package compile
  :defer t)

(use-package gdb-mi
  :defer t)

(use-package pyvenv
  :defer t)

(use-package flycheck
  :demand t
  :config
  (remove-hook 'python-mode-hook 'pylint-add-menu-items)
  (remove-hook 'python-mode-hook 'pylint-add-key-bindings)
  (setq-default flycheck-disabled-checkers '(python-pylint emacs-lisp-checkdoc))
  (setq-default flycheck-scheme-chicken-executable "chicken-csc"))

(use-package with-editor
  :defer t
  :hook (with-editor-mode . (lambda () (whitespace-toggle-options 'tabs))))

(use-package markdown-mode
  :defer t
  :hook (markdown-mode . (lambda ()
                           (whitespace-toggle-options 'lines-tail)
                           (auto-fill-mode))))

;;; Applications (Org, Mail, Chat, AI)

(use-package org
  :demand t
  :bind (("<f9>" . org-latex-export-to-pdf)
         ("C-M-<return>" . (lambda ()
                             (interactive)
                             (end-of-buffer)
                             (org-insert-heading)
                             (crux-insert-date))))
  :hook (org-mode . (lambda ()
                      (whitespace-toggle-options 'lines-tail)
                      (auto-fill-mode)))
  :config
  ;; Base requires
  (require 'org-mouse)
  (require 'org-agenda)

  ;; Babel configuration
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((emacs-lisp . t)
                                 (python . t)
                                 (shell . t)))

  ;; Base UI/Keywords
  (setq org-src-fontify-natively t)
  (setq org-todo-keywords '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))

  ;; LaTeX and Exporting Overrides
  (require 'ox-latex)
  (require 'oc-biblatex)
  (require 'oc-natbib)
  (require 'oc-csl)
  (require 'ob-latex)
  (require 'ox-gfm nil t)
  (setq org-latex-listings (or (bound-and-true-p lerax-latex-listing) 'minted))
  (setq org-latex-minted-options '(("frame" "lines")
                                   ("linenos" "true"))
        org-preview-latex-default-process 'imagemagick)
  (plist-put org-format-latex-options :scale 1.2)

  ;; Jekyll Custom Links Overrides
  (defun org-custom-link-img-follow (path)
    (org-open-file (format "../assets/posts/%s" path)))
  (defun org-custom-link-img-export (path desc format)
    (cond
     ((eq format 'html)
      (format "<img src=\"/assets/posts/%s\" alt=\"%s\"/>" path desc))))
  (org-link-set-parameters "img"
                           :follow 'org-custom-link-img-follow
                           :export 'org-custom-link-img-export))

(use-package ox-beamer
  :ensure nil
  :defer t
  :after org
  :bind (:map org-beamer-mode-map
         ("<f9>" . org-beamer-export-to-pdf))
  :config
  (require 'ox-beamer))

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

(defun fix-tuareg-background-at-export ()
  (require 'tuareg)
  (copy-face 'font-lock-type-face 'tuareg-font-lock-constructor-face)
  (set-face-attribute 'tuareg-font-lock-constructor-face nil :background nil))

(add-hook 'htmlize-before-hook #'fix-tuareg-background-at-export)

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

(use-package notmuch
  :demand t
  :config
  (setq-default notmuch-search-oldest-first nil)
  (setq mail-user-agent 'message-user-agent
        user-mail-address "manoelnt0@gmail.com"
        user-full-name "Manoel Vilela"
        message-default-mail-headers "Cc: \nBcc: \n"
        message-auto-save-directory "~/mail/draft"
        message-kill-buffer-on-exit t
        message-directory "~/mail/")
  (setq smtpmail-smtp-server "smtp.gmail.com"
        message-send-mail-function 'message-smtpmail-send-it
        smtpmail-debug-info t))

(use-package ispell
  :demand t
  :config
  (defvar spellchecker:extension (if (eq system-type 'windows-nt) ".exe" ""))
  (defvar spellchecker:hunspell-name (format "hunspell%s" spellchecker:extension))
  (defvar spellchecker:aspell-name (format "aspell%s" spellchecker:extension))

  (defvar spellchecker:hunspell-exists
    (file-exists-p (or (executable-find spellchecker:hunspell-name) "/not/found/")))

  (defvar spellchecker:default-spell-program spellchecker:aspell-name)
  (defvar spellchecker:hunspell-dict "pt_BR")
  (defvar spellchecker:aspell-dict "en_US")

  (defun spellchecker:select-spell-program (spell-name)
    (message "%s"
             (print (cond ((equal spell-name spellchecker:hunspell-name)
                           (setq ispell-program-name spellchecker:hunspell-name))
                          ((equal spell-name spellchecker:aspell-name)
                           (setq ispell-program-name spellchecker:aspell-name))))))

  (defun spellchecker:activate ()
    (when (and spellchecker:hunspell-exists
               (equal spellchecker:default-spell-program spellchecker:hunspell-name))
      (setq ispell-dictionary spellchecker:hunspell-dict)
      (spellchecker:select-spell-program spellchecker:hunspell-name)))

  (when (eq system-type 'gnu/linux)
    (spellchecker:activate)))

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

(defcustom lerax-latex-listing 'minted
  "If `lerax-latex-listing' is minted enable syntax highlight"
  :group 'lerax
  :type 'symbol)

(defun clean-export-pdf (&rest _)
  (let* ((fname (file-name-base (buffer-name)))
        (pattern (format "%s.!(pdf|org)" fname))
        (cmd (format "bash -c 'shopt -s extglob; rm -rf %s &'" pattern)))
   (call-process-shell-command cmd nil 0)))

(advice-add 'org-latex-export-to-pdf :after #'clean-export-pdf)
(advice-add 'org-beamer-export-to-pdf :after #'clean-export-pdf)

(defcustom lerax-blog-basepath
  (expand-file-name "~/Dropbox/Programming/Projects/Website/ryukinix.github.io")
  "My blog base path"
  :group 'lerax
  :type 'string)



;;; Miscellaneous
