;; configuration about my personal theme
;; theme


(require 'prelude-packages)
(prelude-require-package 'kaolin-themes)

(defvar lerax-theme 'kaolin-bubblegum)
(defvar lerax-theme-window-loaded nil)
(defvar lerax-theme-terminal-loaded nil)

(setq prelude-theme lerax-theme)

(defun lerax-theme-set-font ()
  (set-face-attribute 'default nil
                      :family "Hack"
                      :height 110
                      :weight 'normal
                      :width 'normal))

(defun lerax-theme-reload ()
  (interactive)
  (load-theme lerax-theme t)
  (lerax-theme-set-font)
  (enable-theme lerax-theme))



;; this code is not very efficient and not pretty,
;; but solve my problem about handling crazy themes
;; using the daemon way
;; ref: https://stackoverflow.com/oquestions/18904529/after-emacs-deamon-i-can-not-see-new-theme-in-emacsclient-frame-it-works-fr
(lerax-theme-set-font)
(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (select-frame frame)
                (if (window-system frame)
                    (unless lerax-theme-window-loaded
                      (lerax-theme-reload)
                      (setq lerax-theme-window-loaded t))
                  (unless lerax-theme-terminal-loaded
                    (lerax-theme-reload)
                    (setq lerax-theme-terminal-loaded t)))))
  (progn
    (load-theme lerax-theme t)
    (if (display-graphic-p)
        (setq lerax-theme-window-loaded t)
      (setq lerax-theme-terminal-loaded t))))
