;; Start Daemon on system if is not running
;; Author: Manoel Vilela

;; just on case of running on Windows
(require 'server)

(when (eq system-type 'windows-nt)
  (load "server")
  (setq server-name "lerax")
  (unless (server-running-p server-name)
    (server-start)))
