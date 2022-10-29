;;; Based on: https://systemcrafters.net/emacs-tips/presentations-with-org-present/
;;; Configure Package Archives -----------------------------

;;; Org Mode Appearance ------------------------------------

;; Load org-faces to make sure we can set appropriate faces
(require 'lerax)
(require 'org)
(require 'org-faces)
(require 'org-present)
(require 'visual-fill-column)

;; ensure it column center
(setq-default visual-fill-column-center-text t)
;; Hide emphasis markers on formatted text
(setq org-hide-emphasis-markers t)

;;; Centering Org Documents --------------------------------

;; Configure fill width
(setq visual-fill-column-width 110
      visual-fill-column-center-text t)

;; make org image resize possible
(setq-default org-image-actual-width nil)

;;; Org Present --------------------------------------------

(defun lerax-org-present-prepare-slide (buffer-name heading)
  ;; Show only top-level headlines
  (org-overview)

  ;; Unfold the current entry
  (org-show-entry)

  ;; Show only direct subheadings of the slide but don't expand them
  (org-show-children))

(defun lerax-org-present-start ()

  ;; Set a blank header line string to create blank space at the top
  (setq header-line-format " ")

  ;; Display inline images automatically
  (org-display-inline-images)
  (text-scale-increase +3)
  (setq mode-line-format nil)
  ;; Center the presentation and wrap lines
  (visual-fill-column-mode 1)
  (visual-line-mode 1))

(defun lerax-org-present-end ()
  ;; Clear the header line string so that it isn't displayed
  (setq header-line-format nil)

  ;; Stop displaying inline images
  (org-remove-inline-images)
  (text-scale-reset)
  (setq mode-line-format '(:eval simple-modeline--mode-line))
  ;; Stop centering the document
  (visual-fill-column-mode 0)
  (visual-line-mode 0))


;; Register hooks with org-present
(add-hook 'org-present-mode-hook 'lerax-org-present-start)
(add-hook 'org-present-mode-quit-hook 'lerax-org-present-end)
(add-hook 'org-present-after-navigate-functions 'lerax-org-present-prepare-slide)
(define-key org-mode-map (kbd "C-<f11>") 'org-present)
(define-key org-present-mode-keymap (kbd "C-<f11>") 'org-present-quit)
