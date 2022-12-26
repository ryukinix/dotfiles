;; fetching personal defined packages


(require 'org)
(require 'org-mouse) ;; enable mouse features on org-mode
(require 'org-agenda) ;; required to make right-click works
(require 'python)
(require 'company)
(require 'company-quickhelp)
(require 'slime)
(require 'ssh-agency)
(require 'dbus)
(require 'git-commit)
(require 'cov)

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
(push 'slime-quicklisp slime-contribs)


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
(add-to-list 'company-backends 'company-c-headers)

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

;; org-mode babel setup
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)))


(setq-default prelude-flyspell nil) ;; disable flyspell as default


(with-eval-after-load 'flycheck
  ;; wtf, who adds this shit to the python-mode-hook?
  (remove-hook 'python-mode-hook 'pylint-add-menu-items)
  (remove-hook 'python-mode-hook 'pylint-add-key-bindings)
  ;;(setq-default flycheck-check-syntax-automatically '(mode-enabled save))
  (setq-default flycheck-disabled-checkers '(python-pylint emacs-lisp-checkdoc))
  (setq-default flycheck-scheme-chicken-executable "chicken-csc"))

;; not available in emacs-nox package
(when (fboundp 'scroll-bar-mode)
 (scroll-bar-mode -1)) ;; disable that ugly scroll bar, i don't need that

(with-eval-after-load 'geiser
  ;; chicken-install -s srfi-18 apropos chicken-doc
  (setq-default geiser-chicken-binary "chicken-csi")
  (setq-default geiser-active-implementations
                '(chicken racket guile chez mit chibi))
  (add-hook 'geiser-repl-mode-hook (lambda ()
                                    (smartparens-mode +1))))

;; I hate these asdf.txt~ files
(setq make-backup-files nil)

;; Sat 18 Jul 2020 08:45:56 AM -03
;; i'm done with guru mode!
;; j. krishnamurti says that gurus are no good
;; i need to think by myself, goodbye guru-mode
(setq prelude-guru nil)

(setq projectile-project-search-path "/home/lerax/Desktop/workspace/")

(when (version<= "26.1" emacs-version)
  (setq confirm-kill-processes nil))

;; (advice-add 'helm-M-x :before
;;             (lambda (_)
;;               (setq-local extended-command-history nil)))

;; (display-battery-mode +1)

;; disabled forge due to emacsql binary stuff and setup of auth
;; maybe enable later
;; (with-eval-after-load 'magit
;;   (require 'forge))
;; prelude shitstorm on 1.1
(setq cov-coverage-mode t)
(global-nlinum-mode -1)
(global-display-line-numbers-mode -1)
(menu-bar-mode -1)
(simple-modeline-mode +1)
(super-save-mode -1)
(setenv "DOCKER_BUILDKIT" "0")
(setq cursor-type 'bar)
;; i think it's related to the simple modeline]:
(with-eval-after-load which-function-mode
  (which-function-mode -1))
;; which-func-ff-hook error: (wrong-type-argument arrayp nil)
;; remove annoying headerline wavy line
(with-eval-after-load lsp-mode
  (setq-local lsp-headerline-breadcrumb-enable-diagnostics nil))
