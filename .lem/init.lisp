(in-package :lem-user)

(define-color-theme "leraxy" ("black-metal-immortal")
  (syntax-keyword-attribute :foreground "dodger blue"))

(load-theme "leraxy")
(lem-core:set-font-name "JuliaMono")


(define-command open-init-file () ()
  (find-file
   (merge-pathnames "init.lisp" (lem-home))))

(define-command open-did-org () ()
  (find-file (expand-file-name "~/Sync/did.org")))

(define-command open-todo-org () ()
  (find-file (expand-file-name "~/Sync/todo.org")))


(define-command kill-current-buffer () ()
  (ignore-errors
    (let ((buffer (current-buffer)))
      (when (and (buffer-filename buffer)
                 (buffer-modified-p buffer)
                 (not (buffer-temporary-p buffer))
                 (not (buffer-read-only-p buffer)))
        (save-buffer (current-buffer)))))
  (kill-buffer (current-buffer)))

(define-command duplicate-region-or-line () ()
  (copy-region)
  (yank))

(define-keys *global-keymap*
  ("C-h b" 'describe-bindings)
  ("C-h k" 'describe-key)
  ("C-h a" 'lem-lisp-mode:lisp-apropos)
  ("C-h c" 'apropos-command)
  ("C-h p" 'lem-lisp-mode:lisp-apropos-package)
  ("C-h f" 'lem-lisp-mode:lisp-describe-symbol)
  ("M-C-K" 'kill-current-buffer)
  ("C-c d" 'duplicate-region-or-line)
  ("M-r"   'lem/language-mode:find-references)
  ("C-x t" 'lem/filer::filer)
  ("C-?"   'redo)
  ("C-/"   'undo)
  ("F5"    'open-todo-org)
  ("F6"    'open-did-org)
  ("F8"    'open-init-file)
  ("M-N"   'lem/line-numbers:toggle-line-numbers)
  ("F11"   'toggle-frame-fullscreen))

;; modes
(lem/auto-save:auto-save-mode)

(defun lisp-minor-modes ()
  (lem-paredit-mode:paredit-mode t)
  (lem-lisp-mode/paren-coloring::enable))
(add-hook lem-lisp-mode:*lisp-mode-hook* 'lisp-minor-modes)

(setq lem-shell-mode:*default-shell-command* "/usr/bin/bash")

;; org-mode
#+sdl2 ;; not working properly with webview in my instalation
(ql:quickload :organ-mode)
