(use-package le-gpt
  :bind (("C-s-g" . le-gpt-chat)
         ("C-M-g" . le-gpt-chat)
         ("C-s-n" . le-gpt-complete-at-point)
         ("C-s-t" . le-gpt-transform-region)
         ("C-s-s" . le-gpt-select-project-files)
         ("C-s-d" . le-gpt-deselect-project-files))
  :config
  (setq-default le-gpt-openai-key (getenv "OPENAI_KEY"))
  (setq-default le-gpt-api-type 'openai)
  (setq-default le-gpt-python-path "/usr/bin/python")
  :hook
  (le-gpt-chat-mode . (lambda ()
                        (interactive)
                        (whitespace-mode -1))))

;;; NOTE(@lerax): dom 01 jun 2025 12:50:08
;; after upgrade version 20250525 stopped to work
;; - gpt-model custom var points to claude even with setq-default pointing to gpt-4o
;; - gpt-mode major mode is not found when called
;;
;; using le-gpt as alternative until got this fixed

;; (use-package gpt
;;   :bind (("C-M-g" . gpt-chat-no-context)
;;          ("C-M-S-g" . gpt-chat))
;;   :hook
;;   (gpt-mode . (lambda ()
;;                 (interactive)
;;                 (whitespace-mode -1)))
;;   :config
;;   (setq gpt-api-type 'openai)
;;   (setq gpt-openai-key (getenv "OPENAI_KEY"))
;;   (setq gpt-model "gpt-4o")
;;   (setq gpt-python-path "/usr/bin/python"))
