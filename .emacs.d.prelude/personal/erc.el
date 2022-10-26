(require 'erc)
(require 'subr-x)

(setq erc-default-server "irc.libera.chat")

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
