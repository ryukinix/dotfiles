;;; -*- lexical-binding: t -*-
;; My Personal Keybindings

(require 'company)
(require 'prelude-custom)
(require 'multiple-cursors)

(message "Personal keybindings loading...")

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


(let ((dropbox-dir (expand-file-name "~/Dropbox"))
      (desktop-dir (expand-file-name"~/Desktop"))
      (langs-dir (expand-file-name "~/Dropbox/Programming/Langs")))
  ;; favorite directories
  (global-set-key (kbd "<f5>") (favorite-dir dropbox-dir))
  (global-set-key (kbd "<f6>") (favorite-dir desktop-dir))
  (global-set-key (kbd "<f7>") (favorite-dir langs-dir)))


(global-set-key [f8] (favorite-dir prelude-user-init-file))
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

;; company-mode additional keybindings
(define-key company-mode-map [C-tab] 'company-complete)
(define-key company-mode-map [C-return] 'company-complete)
(define-key company-active-map [C-tab] 'company-complete-common-or-cycle)
(define-key company-active-map [tab] 'company-complete-selection)
(define-key company-active-map (kbd "\C-n") 'company-select-next)
(define-key company-active-map (kbd "\C-p") 'company-select-previous)
(define-key company-active-map (kbd "\C-d") 'company-show-doc-buffer)
(define-key company-active-map (kbd "M-.") 'company-show-location)

;; reset scale
(defun text-scale-reset ()
  (interactive)
  (text-scale-set 0))

;; multiple-cursors
(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)

;; HACK: yes, windows and linux has different keybinding names!!!
;; gnu/linux -> windows-nt
;; mouse-4 -> wheel-up
;; mouse-5 -> wheel-down
;; mouse-8 -> mouse-4
;; mouse-9 -> mouse-5
(labels ((meta-kbd (meta bind &optional (key "@key"))
                   (kbd (replace-regexp-in-string key bind meta))))
  (let* ((windows-p (eq system-name 'windows-nt))
         (wheel-up (if windows-p "wheel-up" "mouse-4"))

         (wheel-down (if windows-p "wheel-down" "mouse-5"))
         (mouse-forward (if windows-p "mouse-5" "mouse-9"))
         (mouse-backforward (if windows-p "mouse-4" "mouse-8")))
    ;; mouse text scale keybindings
    (global-set-key (meta-kbd "<C-M-@key>" wheel-down) 'text-scale-decrease)
    (global-set-key (meta-kbd "<C-M-@key>" wheel-up) 'text-scale-increase)
    (global-set-key (meta-kbd "<@key>" mouse-forward) 'text-scale-increase)
    (global-set-key (meta-kbd "<@key>" mouse-backforward) 'text-scale-decrease)
    (global-set-key (kbd "C-*") 'text-scale-reset)
    (global-set-key (kbd "<C-pause>") 'text-scale-decrease)
    (global-set-key (meta-kbd "<C-M-@key>" mouse-forward) 'text-scale-reset)
    (global-set-key (meta-kbd "<C-M-@key>" mouse-backforward) 'text-scale-reset)))
