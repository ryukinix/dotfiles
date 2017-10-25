;; fetching personal defined packages
(require 'prelude-packages)
(prelude-require-packages '(slime-company ssh-agency xclip spacemacs-theme))


(require 'erc)
(require 'org)
(require 'org-mouse) ;; enable mouse features on org-mode
(require 'python)
(require 'company)
(require 'company-quickhelp)
(require 'slime)
(require 'ssh-agency)


;; My modes
(message "Modos do Manoel")

;; only activate global-wakatime-mode if the user
;; specifically installed this
(when (package-installed-p 'wakatime-mode)
  ;; wakatime on all files
  (global-wakatime-mode))

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

;; add company tooltip align annotations
(setq company-tooltip-align-annotations t)

;; add company quickhelp mode
(company-quickhelp-mode +1)

;; let's go autocomplete FAST
(setq company-quickhelp-delay 0.1)
(setq company-idle-delay 0.3)
(setq company-tooltip-idle-delay 0.1)

;; add slime-company completion tool to slime-contribs
(push 'slime-company slime-contribs)

;; this works to use git on command line with core.editor=emacs
(global-git-commit-mode +1)

;; setup irc autologin channels
(setq erc-autojoin-channels-alist '(("freenode.net" "#haskell" "#python" "#lisp")))

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
;; ensure that all the ssh keys was loaded
(when (executable-find ssh-agency-agent-executable)
  ;; this will not work if the SSH_AUTH_SOCK was not opened yet
  ;; On my setup I rely on gnome-keyring to open this shit on XFCE startup
  ;; so then on a running without X this will not work
  (let ((hardcoded-auth (getenv "SSH_AUTH_SOCK")))
    (ssh-agency-ensure)
    (when hardcoded-auth
        (setenv "SSH_AUTH_SOCK_BACK" (getenv "SSH_AUTH_SOCK"))
        (setenv "SSH_AUTH_SOCK" hardcoded-auth))))

(defun ssh-agency-auth-sock-restore ()
  "Restore SSH_AUTH_SOCK_BACK and SSH_AUTH_SOCK"
  (interactive)
  (let* ((ssh "SSH_AUTH_SOCK")
         (ssh-back "SSH_AUTH_SOCK_BACK")
         (current (getenv ssh))
         (back (getenv ssh-back)))
    (when back
      (setenv ssh-back current)
      (setenv ssh back))
    (message (getenv ssh))))

;; use spacemacs theme for now... tired from zenburn and yellow colors.
(load-theme 'spacemacs-dark)

;; set tab size to 4 (I don't like it 8, very big for me)
(setq-default tab-width 4)

(when (executable-find "xclip")
  (require 'xclip)
  (xclip-mode +1))
