;; configuration about my personal theme
;; theme


(require 'prelude-packages)
(prelude-require-package 'doom-themes)

(defvar my:theme 'doom-vibrant)
(defvar my:theme-window-loaded nil)
(defvar my:theme-terminal-loaded nil)

(setq prelude-theme my:theme)

;; this code is not very efficient and not pretty,
;; but solve my problem about handling crazy themes
;; using the daemon way
;; ref: https://stackoverflow.com/questions/18904529/after-emacs-deamon-i-can-not-see-new-theme-in-emacsclient-frame-it-works-fr
(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (select-frame frame)
                (if (window-system frame)
                    (unless my:theme-window-loaded
                      (if my:theme-terminal-loaded
                          (enable-theme my:theme)
                        (load-theme my:theme t))
                      (setq my:theme-window-loaded t))
                  (unless my:theme-terminal-loaded
                    (if my:theme-window-loaded
                        (enable-theme my:theme)
                      (load-theme my:theme t))
                    (setq my:theme-terminal-loaded t)))))
  (progn
    (load-theme my:theme t)
    (if (display-graphic-p)
        (setq my:theme-window-loaded t)
      (setq my:theme-terminal-loaded t))))
