;;; -*- lexical-binding: t -*-
;;; presentation.el --- Org-present configuration for presentations

(require 'lerax)

(use-package visual-fill-column
  :defer t
  :config
  (setq-default visual-fill-column-center-text t)
  (setq visual-fill-column-width 110
        visual-fill-column-center-text t))

(use-package org-present
  :defer t
  :bind (:map org-mode-map
         ("C-<f11>" . org-present)
         :map org-present-mode-keymap
         ("C-<f11>" . org-present-quit))
  :hook ((org-present-mode . lerax-org-present-start)
         (org-present-mode-quit . lerax-org-present-end)
         (org-present-after-navigate-functions . lerax-org-present-prepare-slide))
  :config
  (setq org-hide-emphasis-markers t)
  (setq-default org-image-actual-width nil)

  (defun lerax-org-present-prepare-slide (buffer-name heading)
    (org-overview)
    (org-show-entry)
    (org-show-children))

  (defun lerax-org-present-start ()
    (setq header-line-format " ")
    (org-display-inline-images)
    (text-scale-increase +3)
    (setq mode-line-format nil)
    (visual-fill-column-mode 1)
    (visual-line-mode 1))

  (defun lerax-org-present-end ()
    (setq header-line-format nil)
    (org-remove-inline-images)
    (text-scale-reset)
    (setq mode-line-format '(:eval simple-modeline--mode-line))
    (visual-fill-column-mode 0)
    (visual-line-mode 0)))

(provide 'presentation)
;;; presentation.el ends here
