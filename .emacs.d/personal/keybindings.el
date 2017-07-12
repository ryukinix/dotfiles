;; My Personal Keybindings

(require 'cl)

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

;; some functions and useful macros
(defmacro favorite-dir (path)
  `(lambda ()
     (interactive)
     (find-file ,path)))

(defun eval-and-replace-last-sexp ()
  (interactive)
  (let ((value (eval (elisp--preceding-sexp))))
    (kill-sexp -1)
    (insert (format "%S" value))))

(when (eq system-type 'windows-nt)
  (lexical-let ((personal-dir "C:/Users/lerax/.emacs.d/personal")
                (dropbox-dir "D:/Users/Manoel/Dropbox")
                (desktop-dir "D:/Users/Manoel/Desktop")
                (langs-dir "D:/Users/Manoel/Dropbox/Programming/Langs"))
    ;; favorite directories
    (global-set-key (kbd "<f5>") (favorite-dir dropbox-dir))
    (global-set-key (kbd "<f6>") (favorite-dir desktop-dir))
    (global-set-key (kbd "<f7>") (favorite-dir langs-dir))
    (global-set-key (kbd "<f8>") (favorite-dir personal-dir))))

;; spacemacs habits...
(global-set-key (kbd "M-1") 'other-window)
(global-set-key (kbd "M-2") 'other-window)
(global-set-key (kbd "C-x C-b") 'ibuffer)
;; neotree feels
(global-set-key (kbd "C-x t") 'neotree-toggle)

;; universal compile command
(global-set-key (kbd "<f9>") 'compile)
(global-set-key (kbd "C-M-S-x") 'eval-and-replace-last-sexp)
(global-set-key (kbd "<C-f9>") 'flyspell-buffer)

;; killing emacs: daemon, frame and just closing
(global-set-key (kbd "<C-M-f4>") 'save-buffers-kill-emacs)
(global-set-key (kbd "<C-f4>") (lambda () (interactive)
                                 (if (> (length (window-list)) 1)
                                     (kill-buffer-and-window)
                                   (kill-buffer (current-buffer)))))

(global-set-key (kbd "<M-f4>") 'intelligent-close)

(global-set-key [M-f1] 'linum-mode)

;; ispell changing dictionaries when need
(global-set-key [C-f8] (lexical-let ((dict (if (eq system-type 'windows-nt)
                                               "brasileiro"
                                             "pt_BR")))
                         (lambda ()
                           (interactive)
                           (ispell-change-dictionary dict))))

(global-set-key [C-f7] (lambda ()
                         (interactive)
                         (ispell-change-dictionary "english")))
