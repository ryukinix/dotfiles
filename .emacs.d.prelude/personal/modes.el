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
(use-package slime-company
  :after (slime company)
  :config
  (slime-setup '(slime-asdf slime-quicklisp slime-fancy slime-company)))


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

;; set tab size to 4 (I don't like it 8, very big for me)
(setq-default tab-width 4)

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
(super-save-mode +1) ;; pq eu tinha desativado isso mesmo?
(setq cursor-type 'bar)
;; which-func-ff-hook error: (wrong-type-argument arrayp nil)
;; remove annoying headerline wavy line
(with-eval-after-load lsp-mode
  ;; doesn't work globally:
  ; (lsp-modeline-code-actions-mode nil)   ; Show code actions on modeline
  (setq-local lsp-headerline-breadcrumb-enable-diagnostics nil))

;; i think it's related to the simple modeline:
(which-function-mode -1)

;; M-x customize-variable simple-modeline-segments
;; '(simple-modeline-segments
;;   '((simple-modeline-segment-modified simple-modeline-segment-buffer-name simple-modeline-segment-position)
;;     (simple-modeline-segment-vc
;;      simple-modeline-segment-misc-info
;;      simple-modeline-segment-process
;;      simple-modeline-segment-major-mode)))
;;
(setq-default notmuch-search-oldest-first nil)

;; work in terminal properly
(progn
  (require 'git-commit)
  (require 'magit-process)
  (global-git-commit-mode +1))

(with-eval-after-load 'magit
  (setq magit-blame-echo-style 'headings)

  (defun lerax-magit-process-environment (env)
    "Detect and set git -bare repo env vars when in tracked dotfile directories."
    (when (equal default-directory (expand-file-name "~/"))
     (let* ((default (file-name-as-directory (expand-file-name default-directory)))
            (git-dir (expand-file-name "~/.dot/"))
            (work-tree (expand-file-name "~/")))
       (push (format "GIT_WORK_TREE=%s" work-tree) env)
       (push (format "GIT_DIR=%s" git-dir) env)))
    env)

  (advice-add 'magit-process-environment
              :filter-return #'lerax-magit-process-environment)
  )

(defun lerax-dotfiles ()
  (interactive)
  (magit "~/"))

(setq server-client-instructions nil)

;; move line of M-x and helm search without need of using C-o
(with-eval-after-load 'helm
  (setq-default history-delete-duplicates t)
  (setq-default helm-apropos-show-short-doc t
                helm-completion-style 'helm
                completions-detailed t
                helm-commands-using-frame (remove 'helm-apropos helm-commands-using-frame))
  (setq-default helm-move-to-line-cycle-in-source nil))

;; remove autocomplete of numbers in company-mode
;; ref: https://emacs.stackexchange.com/a/61125
(progn
  (push (apply-partially #'cl-remove-if
                         (lambda (c) (string-match-p "\\`[0-9]+\\'" c)))
        company-transformers)
  )

(org-babel-do-load-languages 'org-babel-load-languages
                             '(
                               (shell . t)
                               (python . t)
                               ))

(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))

;; ref: https://github.com/mvdan/gofumpt
(setq lsp-go-use-gofumpt t)

(remove-hook 'after-save-hook
             'executable-make-buffer-file-executable-if-script-p)

;; using gtags may generate duplicated entries for c/c++ autocomplete
;; THIS DOESN'T WORKS
;; Used: M-x customize-variable company-backends
;; Removed: GNU Global + Etags, save for future session reference
;; (with-eval-after-load 'company
;;   (push 'company-etags company--disabled-backends)
;;   (push 'company-gtags company--disabled-backends))

(when (executable-find "delta")
  (use-package magit-delta
    :hook (magit-mode . magit-delta-mode)))


;; (setq simple-modeline-segments
;;       '((simple-modeline-segment-modified
;;          simple-modeline-segment-buffer-name
;;          simple-modeline-segment-position)
;;         (simple-modeline-segment-vc simple-modeline-segment-misc-info
;;                                     simple-modeline-segment-process
;;                                     simple-modeline-segment-major-mode)))
