;; Start Daemon on system if is not running
;; Author: Manoel Vilela

(require 'server)

(when (getenv "EXWM")
  (setq server-name "exwm"))

(unless (server-running-p)
  (server-start))
