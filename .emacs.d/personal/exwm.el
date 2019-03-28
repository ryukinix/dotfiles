(require 'prelude-packages nil t)

(defun lerax-start-exwm (&optional frame)
  (when (not (null (getenv "EXWM")) )
    (prelude-require-packages '(exwm))
    (require 'exwm)
    (exwm-init frame)))

(add-hook 'after-make-frame-functions
          #'lerax-start-exwm)
