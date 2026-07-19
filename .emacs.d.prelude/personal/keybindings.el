;;; -*- lexical-binding: t -*-
;;; keybindings.el --- Global personal keybindings

(require 'lerax)

;; some functions and useful macros
(defmacro favorite-dir (path)
  "Retorna um comando interativo para abrir o PATH."
  `(lambda ()
     (interactive)
     (find-file ,path)))

;; reset scale
(defun text-scale-reset ()
  (interactive)
  (text-scale-set 0))

;; favorite directories
(let ((masters-dir (expand-file-name "~/Sync/ita/masters-thesis/"))
      (todo-dir (expand-file-name "~/Sync/todo.org"))
      (did-dir (expand-file-name "~/Sync/did.org")))
  (global-set-key (kbd "<f5>") (favorite-dir todo-dir))
  (global-set-key (kbd "<f6>") (favorite-dir did-dir))
  (global-set-key (kbd "<f7>") (favorite-dir masters-dir)))

(global-set-key (kbd "M-<f1>") (lambda () (interactive) (switch-to-buffer "*scratch*")))

(let ((init (if (eq system-type 'windows-nt)
                (expand-file-name "~/.dotfiles/.emacs.d/personal/")
              prelude-user-init-file)))
  (global-set-key [f8] (favorite-dir init)))

(global-set-key (kbd "M-O") 'lerax-switch-to-minibuffer-window)

;; spacemacs habits...
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-M-S-b") 'ibuffer)

;; universal compile command
(global-set-key (kbd "M-S-<f9>") 'compile)
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

;; mouse text scale keybindings
(cl-labels ((meta-kbd (meta bind &optional (key "@key"))
                   (kbd (replace-regexp-in-string key bind meta))))
  (let* ((windows-p (eq (system-name) 'windows-nt))
         (wheel-up (if windows-p "wheel-up" "mouse-4"))
         (wheel-down (if windows-p "wheel-down" "mouse-5"))
         (mouse-forward (if windows-p "mouse-5" "mouse-9"))
         (mouse-backforward (if windows-p "mouse-4" "mouse-8")))
    (global-set-key (meta-kbd "<C-M-@key>" wheel-down) 'text-scale-decrease)
    (global-set-key (meta-kbd "<C-M-@key>" wheel-up) 'text-scale-increase)
    (global-set-key (meta-kbd "<@key>" mouse-forward) 'text-scale-increase)
    (global-set-key (meta-kbd "<@key>" mouse-backforward) 'text-scale-decrease)
    (global-set-key (kbd "C-*") 'text-scale-reset)

    (global-set-key (kbd "C-+") 'text-scale-increase)
    (global-set-key (kbd "C--") 'text-scale-decrease)

    (global-set-key (kbd "<C-Scroll_Lock>") 'text-scale-reset)
    (global-set-key (kbd "<C-pause>") 'text-scale-decrease)
    (global-set-key (meta-kbd "<C-M-@key>" mouse-forward) 'text-scale-reset)
    (global-set-key (meta-kbd "<C-M-@key>" mouse-backforward) 'text-scale-reset)))


;; disable mouse-start-secondary (selection)
(global-unset-key (kbd "<M-mouse-1>"))

;; split keybindings
(global-set-key (kbd "C-x /") 'lerax-toggle-window-split)
(global-set-key (kbd "C-x |") 'split-window-horizontally)
(global-set-key (kbd "C-x _") 'split-window-vertically)

;; alternative keybinding because C-x C-; doesnt't works on terminal
(global-set-key (kbd "C-x M-;") 'comment-line)

;; install packages easy
(global-set-key (kbd "M-p") 'package-install)

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

(with-eval-after-load 'windmove
  (windmove-default-keybindings '(control shift))) ;; avoid keybinding collision

;; comment region with partial line selection
(global-set-key (kbd "C-M-;") 'lerax-comment-or-uncomment-region-or-line)

;; select whole line
(global-set-key (kbd "C-M-=") (kbd "C-a C-S-n"))
(global-set-key (kbd "C-,") #'xref-find-references)

(global-set-key (kbd "M-<f12>") 'lerax-theme-light-dark-toggle)

(provide 'keybindings)
;;; keybindings.el ends here
