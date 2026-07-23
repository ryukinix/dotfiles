;;; -*- lexical-binding: t -*-
;;; hooks.el --- Global configurations and hook functions
(require 'lerax)

;; Global auto modes
(add-to-list 'auto-mode-alist '("\\.\\(c\\|f\\)sproj\\'" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.install" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.http" . restclient-mode))
(add-to-list 'magic-mode-alist '("#!/usr/bin/openrc-run" . shell-script-mode))

;; Global frame setup
(add-hook 'after-make-frame-functions 'lerax-setup-terminal-session t)

;; Variables
(defvar gud-gud-gdb-history nil)

;; Use-package rewrites
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

(provide 'hooks)
;;; hooks.el ends here
