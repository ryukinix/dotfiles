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

(defvar lerax-theme-window-loaded nil)
(defvar lerax-theme-terminal-loaded nil)
(defconst prelude-theme lerax-theme)


(defun font-exists-p (font)
  "check if font exists"
  (if (and (display-graphic-p)
           (null (x-list-fonts font)))
      nil
    t))

(defun lerax-theme-set-font ()
  (when (font-exists-p lerax-theme-font)
    (set-face-attribute 'default nil
                        :family lerax-theme-font
                        :height 120
                        :weight 'normal
                        :width 'normal)))

(defun lerax-theme-reload ()
  (interactive)
  (load-theme 'doom-meltbus t)
  (load-theme lerax-theme t)
  (lerax-theme-set-font)
  (enable-theme 'doom-meltbus)
  (enable-theme lerax-theme))


;; this code is not very efficient and not pretty,
;; but solve my problem about handling crazy themes
;; using the daemon way
;; ref: https://stackoverflow.com/oquestions/18904529/after-emacs-deamon-i-can-not-see-new-theme-in-emacsclient-frame-it-works-fr
(lerax-theme-set-font)

(defun lerax-setup-frame-theme (frame)
  (select-frame frame)
  (if (display-graphic-p frame)
      (unless lerax-theme-window-loaded
        (lerax-theme-reload)
        (setq lerax-theme-window-loaded t))
    (when lerax-theme-terminal-loaded
      (winner-mode -1)
      (lerax-theme-reload)
      (setq lerax-theme-terminal-loaded t))))

(if (daemonp)
    (add-hook 'after-make-frame-functions
            #'lerax-setup-frame-theme)
  (progn
    (lerax-theme-reload)
    (if (display-graphic-p)
        (setq lerax-theme-window-loaded t)
      (setq lerax-theme-terminal-loaded t)))
  )
