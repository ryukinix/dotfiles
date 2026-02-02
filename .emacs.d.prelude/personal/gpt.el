(use-package gpt
  :demand t
  :bind (("C-M-g" . gpt-chat-no-context)
         ("C-M-b" . gpt-chat-all-buffers)
         ("C-M-c" . gpt-chat-current-buffer)
         ("C-M-e" . gpt-edit-current-buffer)))

(with-eval-after-load 'gpt-core
  (setq gpt-openai-key (getenv "OPENAI_KEY"))
  (setq gpt-google-key (getenv "GEMINI_KEY"))
  (setq gpt-model "gemini-3-pro-preview"))

(use-package gptel
  :bind (("C-s-t" . gptel-rewrite)
         ("C-M-S-g" . gptel))
  :config
  (setq gptel-default-mode 'org-mode
        gptel-model 'gpt-5-mini
        gptel-api-key (getenv "OPENAI_KEY")
        gptel-model 'gemini-3-pro-preview
        gptel-backend (gptel-make-gemini "Gemini"
                        :key (getenv "GEMINI_KEY")
                        :stream t)))
