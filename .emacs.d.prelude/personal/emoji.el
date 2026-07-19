;;; -*- lexical-binding: t -*-
;;; emoji.el --- Emoji font config
;;; -*- lexical-binding: t -*-

(defun lerax-set-emoji-font (frame)
  "Adjust the font settings of FRAME so Emacs can display emoji properly."
  (when (fboundp 'set-fontset-font)
   (if (eq system-type 'darwin)
       (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") frame 'prepend)
     (set-fontset-font t 'symbol (font-spec :family "Symbola") frame 'prepend))))

(lerax-set-emoji-font nil)
(add-hook 'after-make-frame-functions 'lerax-set-emoji-font)

(provide 'emoji)
;;; emoji.el ends here
