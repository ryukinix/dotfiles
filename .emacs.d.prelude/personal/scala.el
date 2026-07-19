;;; -*- lexical-binding: t -*-
;;; scala.el --- Scala environment configuration
;;; -*- lexical-binding: t -*-

(require 'lerax)

(use-package sbt-mode
  :defer t
  :commands (sbt-start sbt-command)
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
  ;; sbt-supershell kills sbt-mode: https://github.com/hvesalai/emacs-sbt-mode/issues/152
  (setq sbt:program-options '("-Dsbt.supershell=false")))

(use-package scala-mode
  :defer t
  :hook ((scala-mode . eglot-ensure)
         (scala-mode . (lambda ()
                         (whitespace-toggle-options 'lines-tail)
                         (setq-local flycheck-check-syntax-automatically
                                     '(save idle-change new-line mode-enabled))))))

(defun scalafmt ()
  (interactive)
  (let ((command "scalafmt")
        (current-file (buffer-file-name (current-buffer))))
    (shell-command (format "%s %s" command current-file))))

(provide 'scala)
;;; scala.el ends here
