(use-package le-gpt
  :bind (("C-s-g" . le-gpt-chat)
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
(use-package gpt
  :bind (("C-M-g" . gpt-chat-no-context)
         ("C-M-S-g" . gpt-dwi))

  :config
  (setq-default gpt-openai-key (getenv "OPENAI_KEY"))
  (setq-default gpt-model "gpt-4o")
  (setq-default gpt-python-path "/usr/bin/python")

  :hook
  (gpt-mode . (lambda ()
                (interactive)
                (whitespace-mode -1))))
