(in-package :lem-user)

(define-color-theme "leraxy" ("black-metal-immortal")
  (syntax-keyword-attribute :foreground "dodger blue"))

(load-theme "leraxy")

;; FIXME(@lerax): 2025/06/11 15h00
;; not working properly yet: i can't see the code formatted after calling
;; M-x format-current-buffer
;; (defun ruff (buf)
;;   "Format a Python buffer with ruff."
;;   (let ((file (buffer-filename buf)))
;;     (uiop:run-program
;;      (format nil "rust format ~a" file)
;;      :ignore-error-status t))
;;   (revert-buffer t))

;; (lem:register-formatter lem-python-mode:python-mode #'ruff)

(define-command open-init-file () ()
  ;; @sasanidas
  (find-file
   (merge-pathnames "init.lisp" (lem-home))))

(define-command kill-current-buffer () ()
  (unless (buffer-temporary-p (current-buffer))
    (save-buffer (current-buffer)))
  (kill-buffer (current-buffer)))

(define-keys *global-keymap*
  ("C-h b" 'describe-bindings)
  ("C-h k" 'describe-key)
  ("C-h a" 'lem-lisp-mode:lisp-apropos)
  ("C-h c" 'apropos-command)
  ("C-h p" 'lem-lisp-mode:lisp-apropos-package)
  ("C-h f" 'lem-lisp-mode:lisp-describe-symbol)
  ("M-C-K" 'kill-current-buffer)
  ("F8"  'open-init-file)
  ("M-N" 'lem/line-numbers:toggle-line-numbers)
  ("F11" 'toggle-frame-fullscreen))

