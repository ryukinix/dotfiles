;; set of hooks made to keep the life more easy

(require 'lerax)


(require 'python)
(require 'pyvenv)
(require 'cc-mode)
(require 'gud)
(require 'compile)
(require 'gdb-mi)
(require 'cl-lib)
(require 'helm-projectile)
(require 'company)
(require 'flycheck)
(require 'company-clang)
(require 'company-c-headers)

;; this fucking variable is created on the fly for gud,
;; so I need declare here to avoid warnings
(defvar gud-gud-gdb-history nil)



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

(with-eval-after-load 'go-mode
  (define-key go-mode-map (kbd "M-.") 'godef-jump)
  (define-key go-mode-map (kbd "C-M-.") 'godef-jump-other-window)
  (add-hook 'go-mode-hook (lambda ()
                            (whitespace-toggle-options 'lines-tail)))
  )


(add-hook 'after-make-frame-functions 'lerax-setup-terminal-session t)
(add-hook 'python-mode-hook #'lerax-python-venv-auto-activate)

;; add commands for build and debug to C++ and C
(add-hook 'c-mode-common-hook 'lerax-setup-c-mode-make)
(add-hook 'c-mode-common-hook 'lerax-setup-c-project t)
;; add commands for debug Python code
(add-hook 'python-mode-hook 'lerax-setup-python-pdb-command)
(add-hook 'python-mode-hook 'pyvenv-mode)
(add-hook 'org-mode-hook (lambda ()
                           (whitespace-toggle-options 'lines-tail)
                           (auto-fill-mode)))
(add-hook 'markdown-mode-hook (lambda ()
                                (whitespace-toggle-options 'lines-tail)
                                (auto-fill-mode)))

;; force xml-mode to msbuild project files
(add-to-list 'auto-mode-alist '("\\.\\(c\\|f\\)sproj\\'" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.install" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.http" . restclient-mode))
;; openrc services auto mode
(add-to-list 'magic-mode-alist '("#!/usr/bin/openrc-run" . shell-script-mode))
