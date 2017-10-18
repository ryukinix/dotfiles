;; set of hooks made to keep the life more easy

(require 'python)
(require 'cc-mode)
(require 'gud)
(require 'compile)
(require 'gdb-mi)
(require 'helm-projectile)
(require 'company)
(require 'prelude-packages)

;; this fucking variable is created on the fly for gud,
;; so I need declare here to avoid warnings
(defvar gud-gud-gdb-history nil)


(defun setup-c-and-cpp-compiler-with-gdb ()
  "Generate strings for 'compile and 'gud-gdb commands on C/C++ mode"
  (interactive)
  (define-key (current-local-map) [C-c C-c] 'compile)
  (define-key (current-local-map) [M-f9] 'gud-gdb)
  (when buffer-file-name
    (let* ((file (file-name-nondirectory buffer-file-name))
          (file-basename (file-name-sans-extension file))
          (extension (if (eq system-type 'windows-nt) "exe" "out")))
      (unless (or (file-exists-p "Makefile") (file-exists-p "makefile"))
        (set (make-local-variable 'compile-command)
             ;; emulate make's .c.o implicit pattern rule, but with
             ;; different defaults for the CC, CPPFLAGS, and CFLAGS
             ;; variables:
             ;; $(CC) -c -o $@ $(CPPFLAGS) $(CFLAGS) $<
             (format "%s -o '%s.%s' %s %s '%s'"
                     (or (getenv "CC") (case c-buffer-is-cc-mode
                                         ('c++-mode "g++")
                                         ('c-mode "gcc")))
                     file-basename extension
                     (or (getenv "CPPFLAGS") "-DDEBUG=9")
                     (or (getenv "CFLAGS") "-pedantic -Wall -g")
                     file)))
      (set (make-local-variable 'gud-gud-gdb-history)
           (cons (format "gdb --fullname \"%s.%s\"" file-basename extension)
                 gud-gud-gdb-history))
      (set (make-local-variable 'gud-gdb-history)
           (cons (format "gdb -i=mi \"%s.%s\"" file-basename extension)
                 gud-gdb-history)))))


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

;; disable tabs visualization on with-editor mode used to do commits
;; by command line
(with-eval-after-load 'with-editor
  (add-hook 'with-editor-mode-hook (lambda () (whitespace-toggle-options 'tabs))))

;; to avoid scale problems
(with-eval-after-load 'linum
  (set-face-attribute 'linum nil :height 100))


;; add commands for build and debug to C++ and C
(add-hook 'c-mode-common-hook 'setup-c-and-cpp-compiler-with-gdb)

;; add commands for debug Python code
(add-hook 'python-mode-hook 'setup-python-pdb-command)


(add-hook 'org-mode-hook (lambda ()
                           (whitespace-toggle-options 'lines-tail)
                           (auto-fill-mode)))

;; force xml-mode to msbuild project files
(add-to-list 'auto-mode-alist '("\\.\\(c\\|f\\)sproj\\'" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.install" . shell-script-mode))

(defun emacsd-push()
  (interactive)
  (shell-command "emacsd-push"))

(defun emacsd-pull()
  (interactive)
  (shell-command "emacsd-pull"))


(defun setup-terminal-session (frame)
  (when (null (window-system frame))
    (set-face-attribute 'helm-selection frame :background "#ff0000")))

;; to enable xclip-mode on terminal mode
(add-hook 'after-make-frame-functions 'setup-terminal-session)
