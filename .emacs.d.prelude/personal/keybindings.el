;;; -*- lexical-binding: t -*-
;; My Personal Keybindings
(require 'lerax)


(require 'company)
(require 'prelude-custom "~/.emacs.d/core/prelude-custom.el") ;; definition of prelude-user-init-file
(require 'yasnippet) ;; template system
(require 'multiple-cursors)
(require 'treemacs)
(require 'darkroom)
(require 'neotree)
(require 'ox-beamer)
(require 'projectile)
(require 'slime)
(require 'cl-lib)
(require 'slime-repl)
(require 'python)

(message "Personal keybindings loading...")

;; i don't know if this is will wrap another key-bind
;; (global-set-key (kbd "C-;") 'comment-or-uncomment-region)

;; clojure repl improvements
(with-eval-after-load 'cider
  (define-key cider-repl-mode-map (kbd "C-c C-k") 'cider-repl-clear-buffer))


(with-eval-after-load 'org
  (define-key org-mode-map (kbd "<f9>") 'org-latex-export-to-pdf))

(with-eval-after-load 'ox-beamer
  (define-key org-beamer-mode-map (kbd "<f9>") 'org-beamer-export-to-pdf))

;; new version of projectile rebind "C-c p" as projectile prefix
;; to C-c C-p which conflicts with Python run-python keybinding
;; so rollback this change
(with-eval-after-load 'projectile
  (custom-set-default 'projectile-keymap-prefix (kbd "C-c p"))
  (define-key projectile-mode-map (kbd "C-c C-p") nil)
  (define-key projectile-mode-map (kbd "C-c p") projectile-command-map))

(with-eval-after-load 'python
  (define-key inferior-python-mode-map (kbd "C-c C-z") 'other-window)
  (when (package-installed-p 'python-black)
    (define-key python-mode-map (kbd "C-c C-f") 'python-black-buffer)))

;; set darkroom for non-distract mode keybinding
(with-eval-after-load 'darkroom
  (setq-default darkroom-text-scale-increase 1.2)
  (global-set-key (kbd "<S-f11>") 'darkroom-tentative-mode)
  (add-hook 'darkroom-tentative-mode-hook
            (lambda ()
              (setq-local truncate-lines t)
              )))

(with-eval-after-load 'slime
  (define-key slime-mode-map (kbd "TAB") 'company-indent-or-complete-common)
  ;; NOTE: something weird happened here
  ;; usually C-c C-z open a slime inferior lisp if not opened
  ;; now open directly a inferior-lisp without slime...
  ;; after slime it's loaded, it's bind C-c C-z to
  ;; slime-switch-to-output-buffer
  ;; slime-switch-to-output-buffer doesn't works if slime is not connected
  (defun slime-repl-open ()
    (interactive)
    (if (slime-connected-p)
        (slime-switch-to-output-buffer)
      (slime)))
  (define-key lisp-mode-map (kbd "C-c C-z") 'slime-repl-open)
  (define-key slime-mode-map (kbd "C-c C-z") 'slime-repl-open)
  (add-to-list 'lisp-mode-hook
               (lambda ()
                 (setq-local browse-url-browser-function 'eww-browse-url))))

(with-eval-after-load 'slime-repl
  (define-key slime-repl-mode-map (kbd "C-c C-z") (lambda ()
                                                    (interactive)
                                                    (select-window (previous-window)))))


;; some functions and useful macros
(defmacro favorite-dir (path)
  `(lambda ()
     (interactive)
     (find-file ,path)))


;; reset scale
(defun text-scale-reset ()
  (interactive)
  (text-scale-set 0))


;; favorite directories
(let ((courses-dir (expand-file-name "~/Dropbox/University/TCC/"))
      (todo-dir (expand-file-name"~/Sync/todo.org"))
      (did-dir (expand-file-name "~/Sync/did.org")))
  ;; favorite directories
  (global-set-key (kbd "<f5>") (favorite-dir todo-dir))
  (global-set-key (kbd "<f6>") (favorite-dir did-dir))
  (global-set-key (kbd "<f7>") (favorite-dir courses-dir)))

(let ((init (if (eq system-type 'windows-nt)
                (expand-file-name "~/.dotfiles/.emacs.d/personal/")
              prelude-user-init-file)))
  (global-set-key [f8] (favorite-dir init)))

(global-set-key (kbd "M-O") 'lerax-switch-to-minibuffer-window)

;; spacemacs habits...
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd  "C-M-S-b") 'ibuffer)
(global-set-key (kbd "C-x y") 'neotree-toggle)
(global-set-key (kbd "C-x Y")
                (lambda ()
                  (interactive)
                  (neotree-refresh)
                  (neotree)))
(global-set-key (kbd "C-x t") 'treemacs)

;; FIXME: that's not working as properly, it should only add when necessary, once
(global-set-key (kbd "C-x T")
                (lambda ()
                  (interactive)
                  (let* ((workspace (treemacs-current-workspace))
                         (current-project (string-trim-right (projectile-project-root) "\/"))
                         (projects (treemacs-workspace->projects workspace)))
                    (unless (find current-project (mapcar #'treemacs-project->path projects) :test #'equal)
                      (condition-case nil
                          (progn
                            (treemacs-add-project-to-workspace current-project)
                            (message "treemacs project added: '%s'" current-project))
                        (error nil))))
                    (treemacs--follow)
                    (treemacs)))


;; universal compile command
;;(global-set-key (kbd "<f9>") 'compile)
(global-set-key (kbd "<f9>") 'projectile-compile-project)
(global-set-key (kbd "M-<f9>") 'projectile-test-project)
(global-set-key (kbd "C-M-S-x") 'edebug-eval-top-level-form)
(global-set-key (kbd "<C-f9>") 'flyspell-buffer)

;; killing emacs: daemon, frame and just closing
(global-set-key (kbd "<C-M-f4>") 'save-buffers-kill-emacs)

(global-set-key (kbd "<C-f4>") 'lerax-kill-this-buffer-and-window)
(global-set-key (kbd "C-M-S-k") 'lerax-kill-this-buffer-and-window)
(global-set-key (kbd "<M-f4>") 'lerax-intelligent-close)

(global-set-key (kbd "M-N") (lerax-get-optimal-linum-mode))
(global-unset-key (kbd "M-R"))
(global-set-key (kbd "M-R") 'lerax-optimal-linum-relative-mode)


;; ispell changing dictionaries when need
(global-set-key [C-f8] (lambda ()
                         (interactive)
                         (ispell-change-dictionary "pt_BR")))

(global-set-key [C-f7] (lambda ()
                         (interactive)
                         (ispell-change-dictionary "en_US")))

;; company-mode additional keybindings
(define-key company-mode-map [C-tab] 'company-complete)
(define-key company-active-map [C-tab] 'company-complete-common-or-cycle)
(define-key company-active-map [tab] 'company-complete-selection)
(define-key company-active-map (kbd "\C-n") 'company-select-next)
(define-key company-active-map (kbd "\C-p") 'company-select-previous)
(define-key company-active-map (kbd "\C-d") 'company-show-doc-buffer)
(define-key company-active-map (kbd "M-.") 'company-show-location)
(define-key yas-minor-mode-map [C-return] 'yas-expand)
(define-key yas-minor-mode-map [M-return] 'yas-insert-snippet)

;; open a terminal full-featured on emacs
(global-set-key (kbd "C-c T") 'vterm)
;; BUGGY: (define-key term-mode-map (kbd "C-c T") 'vterm)


;; multiple-cursors
(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)
(global-set-key (kbd "C-M->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-M-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C->") 'mc/mark-next-like-this-word)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this-word)
(global-set-key (kbd "C-S-l") 'mc/edit-lines) ;; mortal-mode from sublime
(global-set-key (kbd "C-ç") 'mc/skip-to-next-like-this)
(global-set-key (kbd "C-M-ç") 'mc/skip-to-previous-like-this)
(global-set-key (kbd "C-c C->") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(global-set-key (kbd "C-c <") 'mc/mark-previous-like-this-word)
(global-set-key (kbd "C-c >") 'mc/mark-next-like-this-word)

;; setup for commands which always run for all cursors
(setq mc/always-run-for-all '(crux-kill-whole-line
                              forward-sentence
                              sp-backward-delete-char
                              sp-delete-char
                              sp-forward-sexp
                              sp-kill-word))

;; this commands below should run only once
(setq mc/cmds-to-run-once '(helm-M-x))

;; HACK: yes, windows and linux has different keybinding names!!!
;; gnu/linux -> windows-nt
;; mouse-4 -> wheel-up
;; mouse-5 -> wheel-down
;; mouse-8 -> mouse-4
;; mouse-9 -> mouse-5
(cl-labels ((meta-kbd (meta bind &optional (key "@key"))
                   (kbd (replace-regexp-in-string key bind meta))))
  (let* ((windows-p (eq (system-name) 'windows-nt))
         (wheel-up (if windows-p "wheel-up" "mouse-4"))
         (wheel-down (if windows-p "wheel-down" "mouse-5"))
         (mouse-forward (if windows-p "mouse-5" "mouse-9"))
         (mouse-backforward (if windows-p "mouse-4" "mouse-8")))
    ;; mouse text scale keybindings
    (global-set-key (meta-kbd "<C-M-@key>" wheel-down) 'text-scale-decrease)
    (global-set-key (meta-kbd "<C-M-@key>" wheel-up) 'text-scale-increase)
    (global-set-key (meta-kbd "<@key>" mouse-forward) 'text-scale-increase)
    (global-set-key (meta-kbd "<@key>" mouse-backforward) 'text-scale-decrease)
    (global-set-key (kbd "C-*") 'text-scale-reset)
    (global-set-key (kbd "<C-Scroll_Lock>") 'text-scale-reset)
    (global-set-key (kbd "<C-pause>") 'text-scale-decrease)
    (global-set-key (meta-kbd "<C-M-@key>" mouse-forward) 'text-scale-reset)
    (global-set-key (meta-kbd "<C-M-@key>" mouse-backforward) 'text-scale-reset)))


;; lisp interaction keybinds
(define-key lisp-interaction-mode-map (kbd "C-c C-z") 'prelude-visit-ielm)


;; disable mouse-start-secondary (selection)
;; this avoid weird highlights when I try just copy and paste with a mouse
;; selection
(global-unset-key (kbd "<M-mouse-1>"))

;; split keybindings
(global-set-key (kbd "C-x /") 'lerax-toggle-window-split)
(global-set-key (kbd "C-x |") 'split-window-horizontally)
(global-set-key (kbd "C-x _") 'split-window-vertically)

;; alternative keybinding because C-x C-; doesnt't works on terminal
(global-set-key (kbd "C-x M-;") 'comment-line)

;; install packages easy
(global-set-key (kbd "M-p") 'package-install)

;; enable yasnippet globally
(yas-global-mode +1)

;; ;; disable this random shit to paste with mouse, i do not use this shit!
;; (global-unset-key (kbd "<mouse-2>"))

;; I don't like this keybinding, it minimizes the emacs
(global-unset-key (kbd "C-z"))


;; horizontal scroll in emacs is weird
(global-unset-key (kbd "<C-prior>"))
(global-unset-key (kbd "<C-next>"))

(when (eq system-type 'gnu/linux)
  (global-unset-key (kbd "<f11>"))
  (global-set-key (kbd "<f11>") 'toggle-frame-fullscreen))

(global-set-key (kbd "C-S-k") 'sp-kill-whole-line)
(global-set-key (kbd "M-<f6>") 'whitespace-mode)
(global-set-key (kbd "C-x C-d") 'helm-browse-project)

(with-eval-after-load 'windmove
  (windmove-default-keybindings '(control shift))) ;; avoid keybinding collision

;; comment region with partial line selection
;; and if there is no region-active only comment the line
(global-set-key (kbd "C-M-;") 'lerax-comment-or-uncomment-region-or-line)

;; select whole line
(global-set-key (kbd "C-M-=") (kbd "C-a C-S-n"))
(global-set-key (kbd "C-,") #'xref-find-references)
(define-key lsp-mode-map (kbd "M-.") #'lsp-find-definition)

(define-key org-mode-map (kbd "C-M-<return>")
  (lambda ()
    (interactive)
    (org-insert-heading)
    (crux-insert-date)))

;; disable annoying move drag-drop of treemacs (i do a lot of mistakes with this)
(with-eval-after-load 'treemacs
  (define-key treemacs-mode-map [drag-mouse-1] nil))

;; FIXME: xxx
(require 'zeal-at-point)
(with-eval-after-load 'zeal-at-point
  (global-set-key (kbd "C-z") 'zeal-at-point))

(global-set-key (kbd "M-<f12>") 'lerax-theme-light-dark-toggle)
