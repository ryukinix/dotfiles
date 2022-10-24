;;; -*- lexical-binding: t -*-
;; My Personal Keybindings
(require 'manoel "~/.emacs.d/personal/preload/mano.el")

;; NOTE: Sat 20 Jun 2020 05:21:37 PM -03
;; installing: yasnippet-snippets destroy all my software ↓ fuckups everything, please don't do that
;; docker-lerax related bug at bootstraping package install
(lerax-require-packages '(multiple-cursors linum-relative
                          neotree treemacs
                          darkroom yasnippet))

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


;;This method, when bound to C-x C-c, allows you to close an emacs frame the
;;same way, whether it's the sole window you have open, or whether it's
;;a "child" frame of a "parent" frame.  If you're like me, and use emacs in
;;a windowing environment, you probably have lots of frames open at any given
;;time.  Well, it's a pain to remember to do Ctrl-x 5 0 to dispose of a child
;;frame, and to remember to do C-x C-x to close the main frame (and if you're
;;not careful, doing so will take all the child frames away with it).  This
;;is my solution to that: an intelligent close-frame operation that works in
;;all cases (even in an emacs -nw session).
(defun lerax-intelligent-close ()
  "quit a frame the same way no matter what kind of frame you are on"
  (interactive)
  (if (eq (car (visible-frame-list)) (selected-frame))
      ;;for parent/master frame...
      (if (> (length (visible-frame-list)) 1)
          ;;close a parent with children present
          (delete-frame (selected-frame))
        ;;close a parent with no children present
        (save-buffers-kill-emacs))
    ;;close a child frame
    (delete-frame (selected-frame))))

;; some functions and useful macros
(defmacro favorite-dir (path)
  `(lambda ()
     (interactive)
     (find-file ,path)))

(defun lerax-toggle-window-split ()
  "Toggle window split layout. Vertical to horizontal and vice-versa.
   Function stole without shame from https://www.emacswiki.org/emacs/ToggleWindowSplit."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
         (next-win-buffer (window-buffer (next-window)))
         (this-win-edges (window-edges (selected-window)))
         (next-win-edges (window-edges (next-window)))
         (this-win-2nd (not (and (<= (car this-win-edges)
                     (car next-win-edges))
                     (<= (cadr this-win-edges)
                     (cadr next-win-edges)))))
         (splitter
          (if (= (car this-win-edges)
             (car (window-edges (next-window))))
          'split-window-horizontally
        'split-window-vertically)))
    (delete-other-windows)
    (let ((first-win (selected-window)))
      (funcall splitter)
      (if this-win-2nd (other-window 1))
      (set-window-buffer (selected-window) this-win-buffer)
      (set-window-buffer (next-window) next-win-buffer)
      (select-window first-win)
      (if this-win-2nd (other-window 1))))))

(defun kill-this-buffer-and-window ()
  (interactive)
  (if (> (length (window-list)) 1)
      (kill-buffer-and-window)
    (kill-buffer (current-buffer))))

;; reset scale
(defun text-scale-reset ()
  (interactive)
  (text-scale-set 0))


(defun switch-to-minibuffer-window ()
  "Switch to minibuffer window (if active)"
  (interactive)
  (when (active-minibuffer-window)
    (select-window (active-minibuffer-window))))

(global-set-key (kbd "M-O") 'switch-to-minibuffer-window)

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


;; favorite directories
(let ((courses-dir (expand-file-name "~/Dropbox/University/TCC/"))
      (todo-dir (expand-file-name"~/Dropbox/todo.txt"))
      (did-dir (expand-file-name "~/Dropbox/did.txt")))
  ;; favorite directories
  (global-set-key (kbd "<f5>") (favorite-dir todo-dir))
  (global-set-key (kbd "<f6>") (favorite-dir did-dir))
  (global-set-key (kbd "<f7>") (favorite-dir courses-dir)))

(let ((init (if (eq system-type 'windows-nt)
                (expand-file-name "~/.dotfiles/.emacs.d/personal/")
              prelude-user-init-file)))
  (global-set-key [f8] (favorite-dir init)))

;; spacemacs habits...
(global-set-key (kbd "M-1") 'other-window)
(global-set-key (kbd "M-2") 'other-window)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd  "C-M-S-b") 'ibuffer)
(global-set-key (kbd "C-x y") 'neotree-toggle)
(global-set-key (kbd "C-x Y")
                (lambda ()
                  (interactive)
                  (neotree-refresh)
                  (neotree)))
(global-set-key (kbd "C-x t") 'treemacs)
(global-set-key (kbd "C-x T")
                (lambda ()
                  (interactive)
                  (let ((original-state (treemacs-current-visibility)))
                    (treemacs-add-and-display-current-project-exclusively)
                    (treemacs--follow)
                    (when (eq (treemacs-current-visibility) original-state)
                      (treemacs)))))


;; universal compile command
;;(global-set-key (kbd "<f9>") 'compile)
(global-set-key (kbd "<f9>") 'projectile-compile-project)
(global-set-key (kbd "C-M-S-x") 'edebug-eval-top-level-form)
(global-set-key (kbd "<C-f9>") 'flyspell-buffer)

;; killing emacs: daemon, frame and just closing
(global-set-key (kbd "<C-M-f4>") 'save-buffers-kill-emacs)

(global-set-key (kbd "<C-f4>") 'kill-this-buffer-and-window)
(global-set-key (kbd "C-M-S-k") 'kill-this-buffer-and-window)
(global-set-key (kbd "<M-f4>") 'lerax-intelligent-close)

;; only available after emacs 26.X
(defvar optimal-linum-mode
  (if (fboundp 'display-line-numbers-mode)
      'display-line-numbers-mode
    'linum-mode))

(defun optimal-linum-relative-mode ()
  "Use the best available linum relative mode"
  (interactive)
  (if (boundp 'display-line-numbers)
      (if (not (equal display-line-numbers 'relative))
        (setq display-line-numbers 'relative)
      (display-line-numbers-mode -1))
    (linum-relative-mode)))

(global-set-key (kbd "M-N") optimal-linum-mode)
(global-unset-key (kbd "M-R"))
(global-set-key (kbd "M-R") 'optimal-linum-relative-mode)


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
(global-set-key (kbd "C-x M") 'term)


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

;; windmove
(require 'windmove)
(windmove-mode +1)
(windmove-default-keybindings '(control shift)) ;; avoid keybinding collision
