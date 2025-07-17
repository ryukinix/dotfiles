(with-eval-after-load 'erc
  (require 'erc-join)
  (require 'erc-log)
  (add-to-list 'erc-modules 'log)
  (add-to-list 'erc-modules 'notifications) ;; enable notifications -- only if have dbus
  (erc-update-modules)
  (setq erc-log-write-after-insert t)
  (setq erc-log-write-after-send t)
  (setq erc-log-insert-log-on-open t)
  (setq erc-default-server "irc.libera.chat")
  (setq erc-prompt "λ>")
  (setq erc-nick "lerax")
  (setq erc-save-buffer-on-part nil)
  (setq erc-save-queries-on-quit nil)
  (erc-autojoin-mode +1)
  (setq erc-autojoin-channels-alist '(("libera.chat" "#emacs" "#lisp" "#linux"))
        erc-autojoin-timing 'ident
        erc-autojoin-delay 10))
