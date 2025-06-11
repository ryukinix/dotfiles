(in-package :lem-user)

(define-color-theme "leraxy" ("black-metal-immortal")
  (:background nil))

(load-theme "lem-dark")
(load-theme "black-metal-immortal")

(in-package :lem)

(define-command open-init-file () ()
  ;; @sasanidas
  (find-file
   (merge-pathnames "init.lisp" (lem-home))))

(define-keys *global-keymap*
  ("F8" 'open-init-file)
  ("F11" 'toggle-frame-fullscreen))