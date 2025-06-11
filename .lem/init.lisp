(in-package :lem-user)
(load-theme "black-metal-immortal")

(in-package :lem)

(define-command open-init-file () ()
  ;; @sasanidas
  (find-file
   (merge-pathnames "init.lisp" (lem-home))))

(define-keys *global-keymap*
  ("F8" 'open-init-file)
  ("F11" 'toggle-frame-fullscreen))