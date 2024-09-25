;; configuration about my personal theme
;; theme

(require 'cl-lib)

(defvar prelude-minimalistic-ui 't)

(defcustom lerax-theme 'leraxy
  "My theme: only loaded in initialization"
  :group 'lerax
  :type 'symbol)

(defcustom lerax-theme-base-dark 'doom-meltbus
  "Base dark theme"
  :group 'lerax
  :type 'symbol)

(defcustom lerax-theme-base-light 'tok
  "Base light theme"
  :group 'lerax
  :type 'symbol)

(defcustom lerax-theme-base lerax-theme-base-dark
  "Base theme"
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

(defun lerax-disable-all-themes ()
  "Disable all base active themes."
  (dolist (i (cl-remove-if-not #'custom-theme-enabled-p
                        (list lerax-theme-base-dark lerax-theme-base-light)))
    (disable-theme i)))

(defun lerax-theme-reload ()
  (interactive)
  (lerax-disable-all-themes)
  (load-theme lerax-theme-base t)
  (load-theme lerax-theme t)
  (enable-theme lerax-theme-base)
  (enable-theme lerax-theme)
  (lerax-theme-set-font))

(defun lerax-get-theme-to-toggle ()
  (let* ((themes (list lerax-theme-base-dark lerax-theme-base-light))
         (theme-to-load (car (cl-remove-if #'custom-theme-enabled-p themes))))
    theme-to-load))

(defun lerax-theme-light-dark-toggle ()
  (interactive)
  (setq lerax-theme-base (lerax-get-theme-to-toggle))
  (lerax-theme-reload))

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
      (setq lerax-theme-terminal-loaded t))))
