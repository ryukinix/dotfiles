;; Start Daemon on system if is not running
;; Author: Manoel Vilela

;; just on case of running on Windows
(when (eq system-type 'windows-nt)
  (require 'server)
  (load "server")
  (setq server-name "lerax")
  (unless (server-running-p server-name)
    (server-start)))
