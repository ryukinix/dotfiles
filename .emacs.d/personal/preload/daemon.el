;; Start Daemon on system if is not running
;; Author: Manoel Vilela

(require 'server)

(unless (server-running-p)
  (server-start))
