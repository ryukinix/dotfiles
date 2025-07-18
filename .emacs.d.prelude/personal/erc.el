(with-eval-after-load 'erc
  (require 'erc-join)
  (require 'erc-log)
  (add-to-list 'erc-modules 'log)
  (add-to-list 'erc-modules 'notifications) ;; enable notifications -- only if have dbus
  (erc-update-modules)

  ; (setq erc-fill-function 'erc-fill-static)
  ; (setq erc-fill-static-center 22)
  (setq erc-hide-list '("JOIN" "PART" "QUIT"))
  (setq erc-lurker-hide-list '("JOIN" "PART" "QUIT"))
  (setq erc-lurker-threshold-time 43200)
  (setq erc-prompt-for-nickserv-password nil)
  (setq erc-prompt-for-password nil)
  (setq erc-prompt-for-channel-key nil)
  (setq erc-server-reconnect-attempts 5)
  (setq erc-server-reconnect-timeout 3)
  (setq erc-track-exclude-types
        '("JOIN" "MODE" "NICK" "PART" "QUIT"
          "324" "329" "332" "333" "353" "477"))
  (setq erc-log-write-after-insert t)
  (setq erc-log-write-after-send t)
  (setq erc-log-insert-log-on-open t)
  (setq erc-default-server "irc.libera.chat")
  (setq erc-prompt "λ>")
  (setq erc-nick '("lerax" "ryukinix"))
  (setq erc-save-buffer-on-part nil)
  (setq erc-save-queries-on-quit nil)
  (erc-autojoin-mode +1)
  (setq erc-autojoin-channels-alist
        '(("libera.chat" "#emacs-social" "#linux" "#emacs"))
        erc-autojoin-timing 'ident
        erc-autojoin-delay 10))
