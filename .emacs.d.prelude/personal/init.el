;;; -*- lexical-binding: t -*-
;;; init.el --- Core variables, native behaviors and basic UI

(require 'lerax)

;; --- From modes.el (Core settings) ---
;;; modes.el --- Package configurations via use-package


;; Core settings
(setq delete-by-moving-to-trash t)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq python-shell-completion-native-enable nil)
(setq initial-scratch-message
"#  ▓█████  ███▄ ▄███▓ ▄▄▄       ▄████▄    ██████
# ▓█   ▀ ▓██▒▀█▀ ██▒▒████▄    ▒██▀ ▀█  ▒██    ▒
# ▒███   ▓██    ▓██░▒██  ▀█▄  ▒▓█    ▄ ░ ▓██▄
# ▒▓█  ▄ ▒██    ▒██ ░██▄▄▄▄██ ▒▓▓▄ ▄██▒  ▒   ██▒
# ░▒████▒▒██▒   ░██▒ ▓█   ▓██▒▒ ▓███▀ ░▒██████▒▒
# ░░ ▒░ ░░ ▒░   ░  ░ ▒▒   ▓▒█░░ ░▒ ▒  ░▒ ▒▓▒ ▒ ░
# ░ ░  ░░  ░      ░  ▒   ▒▒ ░  ░  ▒   ░ ░▒  ░ ░
# ░   ░      ░     ░   ▒   ░        ░  ░  ░
# ░  ░       ░         ░  ░░ ░            ░
")
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
(when (version<= "26.1" emacs-version) (setq confirm-kill-processes nil))
(global-display-line-numbers-mode -1)
(menu-bar-mode -1)
(simple-modeline-mode +1)
(super-save-mode +1)
(setq cursor-type 'bar)
(which-function-mode -1)

(setq server-client-instructions nil)
(remove-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
(when (eq system-type 'gnu/linux)
  (setq x-select-enable-clipboard-manager nil))
(when (eq system-type 'windows-nt)
  (put 'gdb 'disabled t))

;; --- From hooks.el (Global hooks and variables) ---
;;; hooks.el --- Global configurations and hook functions

;; Global auto modes
(add-to-list 'auto-mode-alist '("\\.\\(c\\|f\\)sproj\\'" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.install" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.http" . restclient-mode))
(add-to-list 'magic-mode-alist '("#!/usr/bin/openrc-run" . shell-script-mode))

;; Global frame setup
(add-hook 'after-make-frame-functions 'lerax-setup-terminal-session t)

;; Variables
(defvar gud-gud-gdb-history nil)
