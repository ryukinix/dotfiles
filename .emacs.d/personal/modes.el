;; My modes
(print "Modos do Manoel")

;; wakatime on all files
(global-wakatime-mode)
;; move to trash on deleting
(setq delete-by-moving-to-trash t)

;; always active highlight on source from org-files
(setq org-src-fontify-natively t)

;; no tabs, never.
(setq-default indent-tabs-mode nil)
;; disable shell completion native
;; to avoid warning on running python shell at emacs
(setq python-shell-completion-native-enable nil)

;; add more tags for TODO list on org-mode
(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))

;; Smart compile command hook for C++ and C
(require 'compile)
(require 'cc-mode)
(add-hook 'prelude-c-mode-common-hook
          (lambda ()
            (define-key (current-local-map) "\C-c\C-c" 'compile )
            (unless (or (file-exists-p "Makefile") (file-exists-p "makefile"))
              (set (make-local-variable 'compile-command)
                   ;; emulate make's .c.o implicit pattern rule, but with
                   ;; different defaults for the CC, CPPFLAGS, and CFLAGS
                   ;; variables:
                   ;; $(CC) -c -o $@ $(CPPFLAGS) $(CFLAGS) $<
                   (let ((file (file-name-nondirectory buffer-file-name)))
                     (format "%s -c -o %s.o %s %s %s"
                             (or (getenv "CC") (if c-buffer-is-cc-mode
                                                   "g++ -std=c++14"
                                                 "gcc"))
                             (file-name-sans-extension file)
                             (or (getenv "CPPFLAGS") "-DDEBUG=9")
                             (or (getenv "CFLAGS") "-ansi -pedantic -Wall -g")
                             file))))))
