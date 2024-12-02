(require 'lerax)

(defun scala-metals ()
  "Enable advanced IDE-like features for scala
   # Get quickly the entire scala dev environment!
   > curl -fL 'https://github.com/coursier/launchers/raw/master/cs-x86_64-pc-linux.gz' | gzip -d > cs
   > chmod +x cs
   ./cs setup \


  Source: https://scalameta.org/metals/docs/editors/emacs.html#installation
  "
  (lerax-require-packages '(lsp-metals
                            lsp-mode
                            lsp-ui
                            lsp-treemacs
                            company-posframe
                            dap-mode))

  (use-package lsp-mode
    ;; Optional - enable lsp-mode automatically in scala files
    ;; You could also swap out lsp for lsp-deffered in order to defer loading
    :hook  (scala-mode . lsp)
    (lsp-mode . lsp-lens-mode)
    :config
    ;; Uncomment following section if you would like to tune lsp-mode performance according to
    ;; https://emacs-lsp.github.io/lsp-mode/page/performance/
    ;; (setq gc-cons-threshold 100000000) ;; 100mb
    ;; (setq read-process-output-max (* 1024 1024)) ;; 1mb
    ;; (setq lsp-idle-delay 0.500)
    ;; (setq lsp-log-io nil)
    ;; (setq lsp-completion-provider :capf)
    (setq lsp-prefer-flymake nil)
    ;; Makes LSP shutdown the metals server when all buffers in the project are closed.
    ;; https://emacs-lsp.github.io/lsp-mode/page/settings/mode/#lsp-keep-workspace-alive
    (setq lsp-keep-workspace-alive nil))

  ;; Add metals backend for lsp-mode
  (use-package lsp-metals)

  ;; Enable nice rendering of documentation on hover
  ;;   Warning: on some systems this package can reduce your emacs responsiveness significally.
  ;;   (See: https://emacs-lsp.github.io/lsp-mode/page/performance/)
  ;;   In that case you have to not only disable this but also remove from the packages since
  ;;   lsp-mode can activate it automatically.
  (use-package lsp-ui)

  ;; Use company-capf as a completion provider.
  ;;
  ;; To Company-lsp users:
  ;;   Company-lsp is no longer maintained and has been removed from MELPA.
  ;;   Please migrate to company-capf.
  (use-package company
    :hook (scala-mode . company-mode)
    :config
    (define-key scala-mode-map (kbd "<tab>") 'company-indent-or-complete-common)
    (define-key scala-mode-map (kbd "TAB") 'company-indent-or-complete-common)
    (setq lsp-completion-provider :capf))

  (progn ;; dap-mode for debugging

    ;; Posframe is a pop-up tool that must be manually installed for dap-mode
    (use-package posframe)

    ;; Use the Debug Adapter Protocol for running tests and debugging
    ;; NOTE(@lerax): dom 01 dez 2024 14:12:48
    ;; https://github.com/emacs-lsp/dap-mode/issues/796
    (use-package dap-mode
      :hook
      (lsp-mode . dap-mode)
      (lsp-mode . dap-ui-mode))

    )
  )

(defun scala-setup ()
  "My scala env for emacs"

  (lerax-require-packages '(use-package
                             sbt-mode
                             scala-mode))

  ;; that is only for NASA COMPUTERS
  ;; be careful, it your EATS RAM REALLY FAST!
  ;; 8gb ram do you have? This is not enough!
  (when (and (not (version< emacs-version "26.1"))
             (>= (lerax-memory-ram) 16))
    (scala-metals))


  ;; Enable sbt mode for executing sbt commands
  (use-package sbt-mode
    :commands sbt-start sbt-command
    :config
    ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
    ;; allows using SPACE when in the minibuffer
    (substitute-key-definition
     'minibuffer-complete-word
     'self-insert-command
     minibuffer-local-completion-map)
    ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
    (setq sbt:program-options '("-Dsbt.supershell=false")))

  (use-package scala-mode
    :hook
    (scala-mode . (lambda () (whitespace-toggle-options 'lines-tail)))
    (scala-mode . (lambda () (setq-local flycheck-check-syntax-automatically
                                    '(save idle-change new-line mode-enabled))))))


(scala-setup)
