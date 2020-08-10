;; HACK: sudo apt-get install ttf-ancient-fonts -y
;; Symbola font can be get by this ^ 😂
;; Use company autocompletion to get proper emojis
(defun lerax-set-emoji-font (frame)
  "Adjust the font settings of FRAME so Emacs can display emoji properly."
  (when (fboundp 'set-fontset-font)
   (if (eq system-type 'darwin)
       ;; For NS/Cocoa
       (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") frame 'prepend)
     ;; For Linux
     (set-fontset-font t 'symbol (font-spec :family "Symbola") frame 'prepend))))

;; For when Emacs is started in GUI mode:
;; Hook for when a frame is created with emacsclient
;; see https://www.gnu.org/software/emacs/manual/html_node/elisp/Creating-Frames.html
(progn
 (lerax-set-emoji-font nil)
 (add-hook 'after-make-frame-functions 'lerax-set-emoji-font))
