;;; lerax.el -- useful macros and functions made by me, Mano.el

;; Copyright © 2017-2022 Manoel Vilela
;;
;; Author: Manoel Vilela <manoel_vilela@engineer.com>
;; URL: https://github.com/ryukinix/dotfiles
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; I'm trying organize my utils functions.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

;; sorry
(require 'cl-lib)
(require 'package)

;; accessing a package repo over https on Windows is a no go, so we
;; fallback to http there

(defun lerax-init-melpa ()
  (if (eq system-type 'windows-nt)
      (add-to-list 'package-archives
                   '("melpa" . "http://melpa.org/packages/") t)
    (add-to-list 'package-archives
                 '("melpa" . "https://melpa.org/packages/") t))

  (package-initialize)
  (setq package--initialized nil))

(defgroup lerax nil
  "My variable group collection"
  :prefix "lerax-"
  :group 'convenience)

(defmacro try-quote (symbol)
  "Quote the SYMBOL if is not quoted.
SYMBOL is any datum."
  (or (and (consp symbol)
           (eq (car symbol) 'quote)
           symbol)
      `(quote ,symbol)))

(defmacro when-system (system &rest body)
  "Only eval BODY when SYSTEM is the name/alias of the running system.
SYSTEM can be a unquoted symbol designing a `system-type'
or one of its alias: linux, windows and mac-os.
BODY is the rest of eval forms to be executed when SYSTEM is equal
to `system-type'."
  (let ((system-alist (make-symbol "system-alist"))
        (system-symbol (make-symbol "system-symbol"))
        (quoted-system (make-symbol "quoted-system")))
    `(let* ((,system-alist '((linux gnu/linux)
                             (mac-os darwin)
                             (windows windows-nt)))
            (,quoted-system (try-quote ,system))
            (,system-symbol (or (second (assoc ,quoted-system ,system-alist))
                              ,quoted-system)))
       (when (eq system-type ,system-symbol)
         ,@body))))


(defmacro memoize (func &rest body)
  "Create a lexical cache for the given FUNC.
FUNC must be a pure function of only one parameter.
BODY is the rest of eval forms to be used FUNC memoized."
  `(let ((table (make-hash-table))
         (old-func (symbol-function ',func)))
     (labels ((,func (x)
                     (if (not (null (nth-value 1 (gethash x table))))
                         (gethash x table)
                       (setf (gethash x table)
                             (funcall old-func x)))))
       (setf (symbol-function ',func) #',func)
       (prog1 ,@body
         (setf (symbol-function ',func) old-func)))))

(defun tab-mode ()
  "Toggle `indent-tabs-mode'."
  (interactive)
  (progn (setq-local indent-tabs-mode (not indent-tabs-mode))
         (message "%s" indent-tabs-mode))
  (if indent-tabs-mode
      (setq-local backward-delete-char-untabify-method 'hungry)
    (setq-local backward-delete-char-untabify-method 'untabify)))

;;;###autoload
(defun lerax-c-reformat-region (&optional b e)
  "Format the region selected with clang-format -style=LLVM"
  (interactive "r")
  (when (not (buffer-file-name))
    (error "A buffer must be associated with a file in order to use REFORMAT-REGION."))
  (when (not (executable-find "clang-format"))
    (error "clang-format not found."))
  (shell-command-on-region b e
                           "clang-format -style=LLVM"
                           (current-buffer) t)
  (indent-region b e))

;;; Jump to a random line
; sburke@cpan.org
;;;###autoload
(defun goto-random-line ()
  "Go to a random line in this buffer."
                                        ; good for electrobibliomancy.
  (interactive)
  (forward-line (1+ (random (buffer-line-count)))))

(defun buffer-line-count ()
  "Return the number of lines in this buffer."
  (count-lines (point-min) (point-max)))

;; stoled from prelude, trying to decouple my config from prelude by steps

(defun lerax-packages-installed-p (packages)
  "Check if all packages in `packages' are installed."
  (cl-every #'package-installed-p packages))

(defun package-refresh-contents-once ()
  (defvar lerax-package-refreshed nil)
  (unless lerax-package-refreshed
    (package-refresh-contents)
    (setq lerax-package-refreshed t)))

(defun lerax-require-package (package)
  "Install PACKAGE unless already installed."
  (push package package-selected-packages)
  (unless (package-installed-p package)
    (package-refresh-contents-once)
    (package-install package)))

(defun lerax-require-packages (packages)
  "Ensure PACKAGES are installed.
Missing packages are installed automatically."
  (mapc #'lerax-require-package packages))

(defun lerax-memory-ram ()
  "Return total RAM available in the host system"
  (when (eq system-type 'gnu/linux)
    (string-to-number
     (shell-command-to-string "free --giga  | grep Mem | awk '{print $2}'"))))

(defun lerax-remove-tags (text)
  (replace-regexp-in-string "</?[A-Z]+>" "" text))

(defun lerax-add-tag (tag)
  "Wrap a XML-like TAG into a specific text region"
  (interactive (list
                (completing-read "TAG: " '("JOB" "PERSON" "DATE"))))
  (let* ((begin (region-beginning))
         (end (region-end))
         (text (buffer-substring-no-properties begin end)))
    (delete-region (region-beginning) (region-end))
    (insert
     (format "<%s>" tag)
     (lerax-remove-tags text)
     (format "</%s>" tag))
    (skip-chars-forward "\n")))

(defun lerax-delete-tag ()
  "Delete a XML-like TAG of a specific text region"
  (interactive)
  (let* ((begin (region-beginning))
         (end (region-end))
         (text (buffer-substring-no-properties begin end)))
    (delete-region (region-beginning) (region-end))
    (insert (lerax-remove-tags text))
    (skip-chars-forward "\n")))



;;This method, when bound to C-x C-c, allows you to close an emacs frame the
;;same way, whether it's the sole window you have open, or whether it's
;;a "child" frame of a "parent" frame.  If you're like me, and use emacs in
;;a windowing environment, you probably have lots of frames open at any given
;;time.  Well, it's a pain to remember to do Ctrl-x 5 0 to dispose of a child
;;frame, and to remember to do C-x C-x to close the main frame (and if you're
;;not careful, doing so will take all the child frames away with it).  This
;;is my solution to that: an intelligent close-frame operation that works in
;;all cases (even in an emacs -nw session).
(defun lerax-intelligent-close ()
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



(defun lerax-toggle-window-split ()
  "Toggle window split layout. Vertical to horizontal and vice-versa.
   Function stole without shame from https://www.emacswiki.org/emacs/ToggleWindowSplit."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
         (next-win-buffer (window-buffer (next-window)))
         (this-win-edges (window-edges (selected-window)))
         (next-win-edges (window-edges (next-window)))
         (this-win-2nd (not (and (<= (car this-win-edges)
                     (car next-win-edges))
                     (<= (cadr this-win-edges)
                     (cadr next-win-edges)))))
         (splitter
          (if (= (car this-win-edges)
             (car (window-edges (next-window))))
          'split-window-horizontally
        'split-window-vertically)))
    (delete-other-windows)
    (let ((first-win (selected-window)))
      (funcall splitter)
      (if this-win-2nd (other-window 1))
      (set-window-buffer (selected-window) this-win-buffer)
      (set-window-buffer (next-window) next-win-buffer)
      (select-window first-win)
      (if this-win-2nd (other-window 1))))))

(defun lerax-kill-this-buffer-and-window ()
  (interactive)
  (if (> (length (window-list)) 1)
      (kill-buffer-and-window)
    (kill-buffer (current-buffer))))


;; only available after emacs 26.X
(defun lerax-get-optimal-linum-mode ()
  (if (fboundp 'display-line-numbers-mode)
      'display-line-numbers-mode
    'linum-mode))


(defun lerax-optimal-linum-relative-mode ()
  "Use the best available linum relative mode"
  (interactive)
  (if (boundp 'display-line-numbers)
      (if (not (equal display-line-numbers 'relative))
          (setq display-line-numbers 'relative)
        (display-line-numbers-mode -1))
    (linum-relative-mode)))

(defun lerax-switch-to-minibuffer-window ()
  "Switch to minibuffer window (if active)"
  (interactive)
  (when (active-minibuffer-window)
    (select-window (active-minibuffer-window))))

(defun lerax-setup-c-mode-make ()
  "Generate strings for 'compile and 'gud-gdb commands on C/C++ mode"
  (interactive)
  (define-key (current-local-map) (kbd "\C-c C-c") 'compile)
  (define-key (current-local-map) [M-f9] 'gud-gdb)
  (when buffer-file-name
    (let* ((file (file-name-nondirectory buffer-file-name))
          (file-basename (file-name-sans-extension file))
          (extension (if (eq system-type 'windows-nt) "exe" "bin")))
      (unless (or (file-exists-p "Makefile") (file-exists-p "makefile"))
        (set (make-local-variable 'compile-command)
             ;; emulate make's .c.o implicit pattern rule, but with
             ;; different defaults for the CC, CPPFLAGS, and CFLAGS
             ;; variables:
             ;; $(CC) -c -o $@ $(CPPFLAGS) $(CFLAGS) $<
             (format "%s -o '%s.%s' %s %s '%s'"
                     (or (getenv "CC")
                         (cl-case c-buffer-is-cc-mode
                           (c++-mode "g++")
                           (c-mode "gcc")))
                     file-basename extension
                     (or (getenv "CPPFLAGS") "-DDEBUG=9")
                     (or (getenv "CFLAGS") " -Wall -g")
                     file)))
      (set (make-local-variable 'gud-gud-gdb-history)
           (cons (format "gdb --nx --fullname \"%s.%s\"" file-basename extension)
                 gud-gud-gdb-history))
      (set (make-local-variable 'gud-gdb-history)
           (cons (format "gdb --nx -i=mi \"%s.%s\"" file-basename extension)
                 gud-gdb-history)))))

(defun lerax-python-venv-auto-activate ()
  (interactive)
  (let* ((basepath (or (projectile-project-root) default-directory))
         (venvpath (concat basepath ".venv/")))
    ;; NOTE(@lerax): qua 29 mar 2023 11:22:05
    ;; I should do this here? Well, make check it's the more useful command
    ;; I usually use with compile
    (when (and (not (string-equal venvpath pyvenv-virtual-env))
               (file-exists-p venvpath))
      (pyvenv-activate venvpath))))

(defun lerax-setup-c-project ()
  (interactive)
  (defconst src-path (concat (projectile-project-root) "src/"))
  (when (file-exists-p src-path)
    (message "Setup C/C++ project!")
    (let* ((include-path (list src-path
                               "/usr/include/SDL2/"))
           (clang-argument (list
                            (format "-I%s"
                                    src-path))))
      (local-set-key (kbd "\C-c C-c") 'projectile-compile-project)
      (local-set-key (kbd "<f9>") 'projectile-compile-project)
      (setq-local flycheck-clang-include-path include-path)
      (setq-local flycheck-gcc-include-path include-path)
      (setq-local company-clang-arguments clang-argument)
      (setq-local company-c-headers-path-user
                  (append company-c-headers-path-user
                          include-path)))))

(defun lerax-setup-python-pdb-command ()
  "Set gud-pdb-command-name variable according the file buffer name"
  (define-key (current-local-map) [M-f9] 'pdb)
  (when buffer-file-name
    (set (make-local-variable 'gud-pdb-history)
         (let ((file (file-name-nondirectory buffer-file-name)))
           (cons (format "python -m pdb %s" file)
                 gud-pdb-history)))))

(defun lerax-setup-terminal-session (&optional frame)
  "SETUP-TERMINAL-SESSION fix wrong theme colors in terminal frame."
  (interactive) ;; make callable as command by M-x
  (when (not (display-graphic-p frame))
    (let ((background "#a60022")
          (foreground "white"))
      (when (< (tty-display-color-cells frame) 256)
        (setq background "blue"))
      (set-face-attribute 'helm-selection frame
                          :background background
                          :foreground foreground))))

(defun lerax-comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region.
  If there is region, set region beginning and region end to
  respective beginning and end of the line
"
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (save-excursion (goto-char (region-beginning))
                                  (line-beginning-position))
              end (save-excursion (goto-char (region-end))
                                  (line-end-position)))
      (setq beg (line-beginning-position)
            end (line-end-position)))
    (comment-or-uncomment-region beg end)
    (when (and (comment-only-p beg end)
               (null (region-active-p)))
      (next-logical-line))))

(defun lerax-build-histogram (words)
  "Create a assoc list a histogram of the elements ordered by frequency"
  (let ((hist '()))
    (cl-loop for word in words
             if (assoc word hist)
               do (incf (cdr (assoc word hist)))
             else
               do (push (cons word 1) hist))
    (sort hist '(lambda (x y) (> (cdr x) (cdr y))))))

(defun lerax-insert-uuid ()
  "Insert a uuid in the buffer."
  (interactive)
  (insert (s-trim (shell-command-to-string "uuidgen"))))


(provide 'lerax)
;;; lerax.el ends here
