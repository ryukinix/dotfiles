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

;; --- From email.el ---
(require 'notmuch)
(require 'smtpmail)
;; setup the mail address and use name
(setq-default notmuch-search-oldest-first nil)
(setq mail-user-agent 'message-user-agent)
(setq user-mail-address "manoelnt0@gmail.com"
      user-full-name "Manoel Vilela")
;; smtp config
(setq smtpmail-smtp-server "smtp.gmail.com"
      message-send-mail-function 'message-smtpmail-send-it)

;; report problems with the smtp server
(setq smtpmail-debug-info t)
;; add Cc and Bcc headers to the message buffer
(setq message-default-mail-headers "Cc: \nBcc: \n")
;; postponed message is put in the following draft directory
(setq message-auto-save-directory "~/mail/draft")
(setq message-kill-buffer-on-exit t)
;; change the directory to store the sent mail
(setq message-directory "~/mail/")

;; --- From emoji.el ---
;;; emoji.el --- Emoji font config

(defun lerax-set-emoji-font (frame)
  "Adjust the font settings of FRAME so Emacs can display emoji properly."
  (when (fboundp 'set-fontset-font)
   (if (eq system-type 'darwin)
       (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") frame 'prepend)
     (set-fontset-font t 'symbol (font-spec :family "Symbola") frame 'prepend))))

(lerax-set-emoji-font nil)
(add-hook 'after-make-frame-functions 'lerax-set-emoji-font)

;; --- From spellchecker.el ---
(require 'ispell)

(defvar spellchecker:extension "")
(when (eq system-type 'windows-nt)
  (setq spellchecker:extension ".exe"))

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
  ;; only provide this shortcuts and changes if hunspell is available
  (when (and spellchecker:hunspell-exists
             (equal spellchecker:default-spell-program spellchecker:hunspell-name))

    (setq ispell-dictionary spellchecker:hunspell-dict)
    (spellchecker:select-spell-program spellchecker:hunspell-name)))


(when (eq system-type 'gnu/linux)
  (spellchecker:activate))
