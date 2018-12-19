;; fetching personal defined packages
(require 'prelude-packages)
(prelude-require-packages '(slime-company
                            ssh-agency
                            xclip
                            pyvenv
                            ox-gfm))


(require 'erc)
(require 'org)
(require 'org-mouse) ;; enable mouse features on org-mode
(require 'python)
(require 'company)
(require 'company-quickhelp)
(require 'slime)
(require 'ssh-agency)
(require 'dbus)

;; My modes
(message "Modos do Manoel")

(eval-after-load "org"
  ;; require ob-latex for exporting org-latex blocks correctly
  '(progn
    (require 'ob-latex)
    (require 'ox-gfm nil t)))

;; only activate global-wakatime-mode if the user
;; specifically installed this
(when (package-installed-p 'wakatime-mode)
  ;; wakatime on all files
  (global-wakatime-mode))

;; windows mode section
(when (eq system-type 'windows-nt)
  ;; disable gdb because is buggy using from windows
  ;; use gud-gdb instead
  (put 'gdb 'disabled t))

;; this avoid crash on daemon-mode at linux when
;; a frame is closed
;; more info on:
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Clipboard.html
(when (eq system-type 'gnu/linux)
  (setq x-select-enable-clipboard-manager nil))

(when (executable-find "xclip")
  (require 'xclip)
  (xclip-mode +1))

;; move to trash on deleting
(setq delete-by-moving-to-trash t)

;; always active highlight on source from org-files
(setq org-src-fontify-natively t)

;; no tabs, never.
(setq-default indent-tabs-mode nil)
;; disable shell completion native
;; to avoid warning on running python shell at emacs
(setq python-shell-completion-native-enable nil)

;; add more tags for TODO list on org-mode
(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))

;; add company tooltip align annotations
(setq company-tooltip-align-annotations t)

;; let's go autocomplete FAST
(setq company-quickhelp-delay 0.1)
(setq company-idle-delay 0.3)
(setq company-tooltip-idle-delay 0.1)

;; add slime-company completion tool to slime-contribs
(push 'slime-company slime-contribs)
(push 'slime-asdf slime-contribs)

;; setup irc autologin channels
;;(setq erc-autojoin-channels-alist '(("freenode.net" "#artix")))
(setq erc-prompt "λ>")

;; enable notifications -- only if have dbus
;; (add-to-list 'erc-modules 'notifications)
(add-to-list 'erc-modules 'log) ;; enable

;; == ERC LOG SETUP
(setq erc-save-buffer-on-part nil)
(setq erc-save-queries-on-quit nil)
(setq erc-log-write-after-insert t)
(setq erc-log-write-after-send t)
(setq erc-log-insert-log-on-open t)
(setq initial-scratch-message
";; ▓█████  ███▄ ▄███▓ ▄▄▄       ▄████▄    ██████
;; ▓█   ▀ ▓██▒▀█▀ ██▒▒████▄    ▒██▀ ▀█  ▒██    ▒
;; ▒███   ▓██    ▓██░▒██  ▀█▄  ▒▓█    ▄ ░ ▓██▄
;; ▒▓█  ▄ ▒██    ▒██ ░██▄▄▄▄██ ▒▓▓▄ ▄██▒  ▒   ██▒
;; ░▒████▒▒██▒   ░██▒ ▓█   ▓██▒▒ ▓███▀ ░▒██████▒▒
;; ░░ ▒░ ░░ ▒░   ░  ░ ▒▒   ▓▒█░░ ░▒ ▒  ░▒ ▒▓▒ ▒ ░
;; ░ ░  ░░  ░      ░  ▒   ▒▒ ░  ░  ▒   ░ ░▒  ░ ░
;; ░   ░      ░     ░   ▒   ░        ░  ░  ░
;; ░  ░       ░         ░  ░░ ░            ░
")


;; set tab size to 4 (I don't like it 8, very big for me)
(setq-default tab-width 4)

;; this works to use git on command line with core.editor=emacs
(global-git-commit-mode +1)
;; add company quickhelp mode
(company-quickhelp-mode +1)

;; prettify lambda as λ
(global-prettify-symbols-mode +1)

;; disable prompting stuff
(setq confirm-nonexistent-file-or-buffer nil)
(setq ido-create-new-buffer 'always)
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))


(defalias 'halt-and-catch-fire #'save-buffers-kill-emacs)
