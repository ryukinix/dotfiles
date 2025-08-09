(in-package :lem-user)

(define-color-theme "leraxy" ("black-metal-immortal")
  (syntax-keyword-attribute :foreground "dodger blue"))

(load-theme "leraxy")

(lem-core:set-font-name "JuliaMono")

(define-command open-init-file () ()
  ;; @sasanidas
  (find-file
   (merge-pathnames "init.lisp" (lem-home))))

(define-command kill-current-buffer () ()
  (ignore-errors
    (let ((buffer (current-buffer)))
      (when (and (buffer-filename buffer)
                 (buffer-modified-p buffer)
                 (not (buffer-temporary-p buffer))
                 (not (buffer-read-only-p buffer)))
        (save-buffer (current-buffer)))))
  (kill-buffer (current-buffer)))

(define-keys *global-keymap*
  ("C-h b" 'describe-bindings)
  ("C-h k" 'describe-key)
  ("C-h a" 'lem-lisp-mode:lisp-apropos)
  ("C-h c" 'apropos-command)
  ("C-h p" 'lem-lisp-mode:lisp-apropos-package)
  ("C-h f" 'lem-lisp-mode:lisp-describe-symbol)
  ("M-C-K" 'kill-current-buffer)
  ("M-r"   'lem/language-mode:find-references)
  ("C-x t" 'lem/filer::filer)
  ("C-?"   'redo)
  ("C-/"   'undo)
  ("F8"    'open-init-file)
  ("M-N"   'lem/line-numbers:toggle-line-numbers)
  ("F11"   'toggle-frame-fullscreen))

;; modes
(lem/auto-save:auto-save-mode)
;; (lem-paredit-mode:paredit-mode)
;; (lem-lisp-mode/paren-coloring:toggle-paren-coloring)

(setq lem-shell-mode:*default-shell-command* "/usr/bin/bash")
