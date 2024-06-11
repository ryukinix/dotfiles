;; configuration about my personal theme
;; theme


(defvar prelude-minimalistic-ui 't)

(defcustom lerax-theme 'leraxy
  "My theme: only loaded in initialization"
  :group 'lerax
  :type 'symbol)


(defcustom lerax-theme-font "JuliaMono Medium"
  "My default font: only loaded in initialization"
  :group 'lerax
  :type 'string)

(defcustom lerax-theme-font-size 12
  "My default font: only loaded in initialization"
  :group 'lerax
  :type 'integer)


(defvar lerax-theme-window-loaded nil)
(defvar lerax-theme-terminal-loaded nil)

;; HACK(@lerax): Mon 10 Jun 2024 11:07:14 PM -03
;; avoid prelude resetting incorrectly the theme
(defconst prelude-theme nil)


(defun font-exists-p (font)
  "check if font exists"
  (if (and (display-graphic-p)
           (null (x-list-fonts font)))
      nil
    t))

(defun lerax-theme-set-font ()
  (interactive)
  (when (font-exists-p lerax-theme-font)
    (let ((font-string (format "%s-%s" lerax-theme-font lerax-theme-font-size)))
      (set-frame-font font-string t nil))))

(defun lerax-theme-reload ()
  (interactive)
  (load-theme 'doom-meltbus t)
  (load-theme lerax-theme t)
  (enable-theme 'doom-meltbus)
  (enable-theme lerax-theme)
  (lerax-theme-set-font))

(defun lerax-setup-frame-theme (frame)
  (select-frame frame)
  (unless (display-graphic-p frame)
    (winner-mode -1))
  (if (not lerax-theme-window-loaded)
      (progn (lerax-theme-reload)
             (setq lerax-theme-window-loaded t))
    ;; force font reload to fix potential issues
    (lerax-theme-set-font)))

(if (daemonp)
    (add-hook 'after-make-frame-functions
              #'lerax-setup-frame-theme)
  (progn
    (lerax-theme-reload)
    (if (display-graphic-p)
        (setq lerax-theme-window-loaded t)
      (setq lerax-theme-terminal-loaded t)))
  )
