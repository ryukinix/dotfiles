(require 'manoel "~/.emacs.d/personal/preload/mano.el")

(defun scala-metals ()
  "You will need emacs-metals in $PATH to work properly.

  Source: https://scalameta.org/metals/docs/editors/emacs.html#installation
  "

  (lerax-require-packages '(use-package
                             lsp-mode
                             company-posframe
                             lsp-ui
                             company-lsp
                             sbt-mode
                             lsp-treemacs
                             dap-mode))


  ;; Enable nice rendering of documentation on hover
  (use-package lsp-ui)
  ;; Add company-lsp backend for metals
  (use-package company-lsp)

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


  (use-package lsp-mode
    ;; Optional - enable lsp-mode automatically in scala files
    :hook  (scala-mode . lsp)
    (lsp-mode . lsp-lens-mode))

  ;; installed company-posframe
  ;; Use the Debug Adapter Protocol for running tests and debugging
  (use-package dap-mode
    :hook
    (lsp-mode . dap-mode)
    (lsp-mode . dap-ui-mode))

  ;; ;; Use the Tree View Protocol for viewing the project structure and triggering compilation
  ;; (use-package lsp-treemacs
  ;;   :config
  ;;   (lsp-metals-treeview-enable t)
  ;;   (setq lsp-metals-treeview-show-when-views-received t))
  )


(when (and (not (version< emacs-version "26.1"))
           (executable-find "metals-emacs"))
  (scala-metals))
