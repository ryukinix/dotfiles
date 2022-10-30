(with-eval-after-load 'erc
  ;; enable notifications -- only if have dbus
  (add-to-list 'erc-modules 'notifications)
  (setq erc-default-server "irc.libera.chat")
  (setq erc-prompt "λ>")
  (setq erc-save-buffer-on-part nil)
  (setq erc-save-queries-on-quit nil))

;; == ERC LOG SETUP
(with-eval-after-load 'erc-log
  (add-to-list 'erc-modules 'log) ;; enable
  (setq erc-log-write-after-insert t)
  (setq erc-log-write-after-send t)
  (setq erc-log-insert-log-on-open t))
