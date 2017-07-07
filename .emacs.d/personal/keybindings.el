;; My Personal Keybindings

(print "Personal keybindings loading...")

;; i don't know if this is will wrap another key-bind
;; (global-set-key (kbd "C-;") 'comment-or-uncomment-region)


;;This method, when bound to C-x C-c, allows you to close an emacs frame the
;;same way, whether it's the sole window you have open, or whether it's
;;a "child" frame of a "parent" frame.  If you're like me, and use emacs in
;;a windowing environment, you probably have lots of frames open at any given
;;time.  Well, it's a pain to remember to do Ctrl-x 5 0 to dispose of a child
;;frame, and to remember to do C-x C-x to close the main frame (and if you're
;;not careful, doing so will take all the child frames away with it).  This
;;is my solution to that: an intelligent close-frame operation that works in
;;all cases (even in an emacs -nw session).
(defun intelligent-close ()
  "quit a frame the same way no matter what kind of frame you are on"
  (interactive)
  (if (eq (car (visible-frame-list)) (selected-frame))
      ;;for parent/master frame...
      (if (> (length (visible-frame-list)) 1)
          ;;close a parent with children present
          (delete-frame (selected-frame))
        ;;close a parent with no children present
        (save-buffers-kill-emacs))
    ;;close a child frame
    (delete-frame (selected-frame))))

(+ 1 2)
;; some functions and useful macros
(defmacro favorite-dir (path)
  `(lambda ()
     (interactive)
     (find-file ,path)))

(defun replace-last-sexp ()
  (interactive)
  (let ((value (eval (elisp--preceding-sexp))))
    (kill-sexp -1)
    (insert (format "%S" value))))

(defvar personal-dir "C:/Users/lerax/.emacs.d/personal")
(defvar dropbox-dir "D:/Users/Manoel/Dropbox")
(defvar desktop-dir "D:/Users/Manoel/Desktop")
(defvar langs-dir "D:/Users/Manoel/Dropbox/Programming/Langs")

;; spacemacs habits...
(global-set-key (kbd "M-1") 'other-window)
(global-set-key (kbd "M-2") 'other-window)
(global-set-key (kbd "C-x C-b") 'ibuffer)
;; neotree feels
(global-set-key (kbd "C-x t") 'neotree-toggle)

;; favorite directories
(global-set-key (kbd "<f5>") (favorite-dir dropbox-dir))
(global-set-key (kbd "<f6>") (favorite-dir desktop-dir))
(global-set-key (kbd "<f7>") (favorite-dir langs-dir))
(global-set-key (kbd "<f8>") (favorite-dir personal-dir))

;; universal compile command
(global-set-key (kbd "<f9>") 'compile)
(global-set-key (kbd "C-M-S-x") 'replace-last-sexp)

;; killing emacs: daemon, frame and just closing
(global-set-key (kbd "<C-M-f4>") 'save-buffers-kill-emacs)
(global-set-key (kbd "<C-f4>") (lambda () (interactive) (kill-buffer (current-buffer))))
(global-set-key (kbd "<M-f4>") 'intelligent-close)
