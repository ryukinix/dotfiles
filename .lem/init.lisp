(in-package :lem-user)

(define-color-theme "leraxy" ("black-metal-immortal")
  (:foreground "#c1c1c1") 
  (:background "#000000")
  (syntax-keyword-attribute :foreground "dodger blue"))

(load-theme "leraxy")
(lem-core:set-font-name "JuliaMono")
;; TODO: set only on webview, how can I use reader macro here for this?
#-sdl2
(lem-core:set-font-size 14)

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
  (let* ((point (current-point))
         (start (cursor-region-beginning point))
         (end (cursor-region-end point)))
   (copy-region start end))
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
