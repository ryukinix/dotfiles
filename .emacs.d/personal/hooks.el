;; set of hooks made to keep the life more easy

(require 'manoel "~/.emacs.d/personal/preload/mano.el")
(lerax-require-packages '(geiser pyvenv flycheck company-c-headers))

(require 'python)
(require 'cc-mode)
(require 'gud)
(require 'compile)
(require 'gdb-mi)
(require 'helm-projectile)
(require 'company)
(require 'geiser)
(require 'geiser-impl)
(require 'flycheck)
(require 'company-clang)
(require 'company-c-headers)

;; this fucking variable is created on the fly for gud,
;; so I need declare here to avoid warnings
(defvar gud-gud-gdb-history nil)


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
                     (or (getenv "CC") (case c-buffer-is-cc-mode
                                         (c++-mode "g++")
                                         (c-mode "gcc")))
                     file-basename extension
                     (or (getenv "CPPFLAGS") "-DDEBUG=9")
                     (or (getenv "CFLAGS") " -Wall -g")
                     file)))
      (set (make-local-variable 'gud-gud-gdb-history)
           (cons (format "gdb --fullname \"%s.%s\"" file-basename extension)
                 gud-gud-gdb-history))
      (set (make-local-variable 'gud-gdb-history)
           (cons (format "gdb -i=mi \"%s.%s\"" file-basename extension)
                 gud-gdb-history)))))

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
      (setq-local flycheck-clang-include-path include-path)
      (setq-local flycheck-gcc-include-path include-path)
      (setq-local company-clang-arguments clang-argument)
      (setq-local company-c-headers-path-user
                  (append company-c-headers-path-user
                          include-path)))))


(defun setup-python-pdb-command ()
  "Set gud-pdb-command-name variable according the file buffer name"
  (define-key (current-local-map) [M-f9] 'pdb)
  (when buffer-file-name
    (set (make-local-variable 'gud-pdb-history)
         (let ((file (file-name-nondirectory buffer-file-name)))
           (cons (format "python -m pdb %s" file)
                 gud-pdb-history)))))


;; can create files on C-c p h
(with-eval-after-load 'helm-projectile
  (defvar helm-source-file-not-found
    (helm-build-dummy-source
        "Create file"
      :action (lambda (cand) (find-file cand))))


  (add-to-list 'helm-projectile-sources-list helm-source-file-not-found t))

;; make flycheck on racket less agressive
(defun cooldown-flycheck-on-racket (&rest _)
  (message "%s" geiser-impl--implementation)
  (if (eq geiser-impl--implementation 'racket)
      (setq-local flycheck-check-syntax-automatically '(save mode-enabled))
    (setq flycheck-check-syntax-automatically
          (default-value 'flycheck-check-syntax-automatically))))


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


(add-hook 'after-make-frame-functions 'lerax-setup-terminal-session t)


;; disable tabs visualization on with-editor mode used to do commits
;; by command line
(with-eval-after-load 'with-editor
  (add-hook 'with-editor-mode-hook (lambda () (whitespace-toggle-options 'tabs))))
;; to avoid scale problems
(with-eval-after-load 'linum
  (set-face-attribute 'linum nil :height 100))


;; add commands for build and debug to C++ and C
(add-hook 'c-mode-common-hook 'lerax-setup-c-mode-make)
(add-hook 'c-mode-common-hook 'lerax-setup-c-project t)
;; add commands for debug Python code
(add-hook 'python-mode-hook 'setup-python-pdb-command)
(add-hook 'python-mode-hook 'pyvenv-mode)
(add-hook 'org-mode-hook (lambda ()
                           (whitespace-toggle-options 'lines-tail)
                           (auto-fill-mode)))
(add-hook 'markdown-mode-hook (lambda ()
                                (whitespace-toggle-options 'lines-tail)
                                (auto-fill-mode)))


(add-hook 'geiser-mode-hook 'cooldown-flycheck-on-racket)
(add-hook 'geiser-mode-hook (lambda ()
                              (company-quickhelp-mode 0)))
(advice-add 'geiser-set-scheme :after 'cooldown-flycheck-on-racket)


;; force xml-mode to msbuild project files
(add-to-list 'auto-mode-alist '("\\.\\(c\\|f\\)sproj\\'" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.install" . shell-script-mode))
;; openrc services auto mode
(add-to-list 'magic-mode-alist '("#!/usr/bin/openrc-run" . shell-script-mode))

(with-eval-after-load 'go-mode
  (define-key go-mode-map (kbd "M-.") 'godef-jump)
  (define-key go-mode-map (kbd "C-M-.") 'godef-jump-other-window)
  (add-hook 'go-mode-hook (lambda ()
                            (whitespace-toggle-options 'lines-tail)))
  )
