;; Start Daemon on system if is not running
;; Author: Manoel Vilela

(load "server")
(setq server-name "lerax")

(unless (server-running-p server-name)
  (server-start))
