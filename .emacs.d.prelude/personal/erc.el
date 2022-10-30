;; (require 'erc)
;; (require 'erc-log)
(require 'subr-x)

;; ;; == ERC LOG SETUP
;; (with-eval-after-load 'erc
;;   ;; enable notifications -- only if have dbus
;;   (add-to-list 'erc-modules 'notifications)
;;   (add-to-list 'erc-modules 'log) ;; enable
;;   (setq erc-default-server "irc.libera.chat")
;;   (setq erc-prompt "λ>")
;;   (setq erc-save-buffer-on-part nil)
;;   (setq erc-save-queries-on-quit nil)
;;   (when (getenv "EXWM")
;;     (erc-notify-mode))
;;   (setq erc-log-write-after-insert t)
;;   (setq erc-log-write-after-send t)
;;   (setq erc-log-insert-log-on-open t))


(defconst lerax-erc-password-file
  (expand-file-name "/keybase/private/lerax/secrets/irc.txt.gpg"))

(defun lerax-read-file-password (file)
  "Read a file password (.gpg) and trim the newlines"
  (string-trim (with-temp-buffer
                 (insert-file-contents file)
                 (buffer-string))))

(defun lerax-erc ()
  (interactive)
  (let ((password (lerax-read-file-password lerax-erc-password-file)))
    (erc :password password)))
