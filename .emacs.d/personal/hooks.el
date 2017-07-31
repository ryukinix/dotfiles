;; set of hooks made to keep the life more easy

(require 'python)
(require 'cc-mode)
(require 'gud)
(require 'compile)
(require 'gdb-mi)
(require 'helm-projectile)

;; this fucking variable is created on the fly for gud,
;; so I need declare here to avoid warnings
(defvar gud-gud-gdb-history nil)


(defun setup-c-and-cpp-compiler-with-gdb ()
  "Generate strings for 'compile and 'gud-gdb commands on C/C++ mode"
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
                    (or (getenv "CC") (if c-buffer-is-cc-mode
                                          "g++"
                                        "gcc"))
                    file-basename extension
                    (or (getenv "CPPFLAGS") "-DDEBUG=9")
                    (or (getenv "CFLAGS") "-ansi -pedantic -Wall -g")
                    file))
       (push (format "gdb --fullname '%s.%s'" file-basename extension)
             gud-gud-gdb-history)
       (set (make-local-variable 'gud-gdb-command-name)
            (format "gdb -i=mi '%s.%s'" file-basename extension))))))


(defun setup-python-pdb-command ()
  "Set gud-pdb-command-name variable according the file buffer name"
  (define-key (current-local-map) [M-f9] 'pdb)
  (when buffer-file-name
    (set (make-local-variable 'gud-pdb-command-name)
         (let ((file (file-name-nondirectory buffer-file-name)))
           (format "python -m pdb %s" file)))))


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
(add-hook 'prelude-c-mode-common-hook 'setup-c-and-cpp-compiler-with-gdb)

;; add commands for debug Python code
(add-hook 'python-mode-hook 'setup-python-pdb-command)
