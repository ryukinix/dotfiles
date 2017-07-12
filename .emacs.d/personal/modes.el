(require 'erc)
(require 'org)
(require 'python)

;; My modes
(print "Modos do Manoel")

;; wakatime on all files
(global-wakatime-mode)
;; move to trash on deleting
(setq delete-by-moving-to-trash t)

;; always active highlight on source from org-files
(setq org-src-fontify-natively t)

;; no tabs, never.
(setq-default indent-tabs-mode nil)
;; disable shell completion native
;; to avoid warning on running python shell at emacs
(setq python-shell-completion-native-enable nil)

;; add more tags for TODO list on org-mode
(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))

;; windows mode section
(when (eq system-type 'windows-nt)
  ;; disable gdb because is buggy using from windows
  ;; use gud-gdb instead
  (put 'gdb 'disabled t)
  (require 'intero)
  (intero-global-mode))

;; setup irc autologin channels
(setq erc-autojoin-channels-alist '(("freenode.net" "#haskell" "#python" "#lisp")))
