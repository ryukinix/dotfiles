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

#+sdl2
(defun get-font-by-name-and-kind (name kind)
  (car (loop for font in (lem-if:get-font-list (lem-core:implementation))
             when (and (search name font)
                       (search (format nil "~a.ttf" kind) font))
                   collect font)))

#+sdl2
(defun set-font-by-name (font-name)
  (lem-sdl2/display:change-font 
   (lem-sdl2/display:current-display)
   (lem-sdl2/font:make-font-config
    :latin-normal-file (get-font-by-name-and-kind font-name "Regular")
    :latin-bold-file (get-font-by-name-and-kind font-name "Bold"))))

#+sdl2
(set-font-by-name "JuliaMono")


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
  ("F8"  'open-init-file)
  ("M-N" 'lem/line-numbers:toggle-line-numbers)
  ("F11" 'toggle-frame-fullscreen))

;; modes
(lem/auto-save:auto-save-mode)
;; (lem-paredit-mode:paredit-mode)
;; (lem-lisp-mode/paren-coloring:toggle-paren-coloring)

(setq lem-shell-mode:*default-shell-command* "/usr/bin/bash")

