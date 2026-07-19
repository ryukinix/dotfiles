;;; -*- lexical-binding: t -*-
;;; gpt.el --- AI and GPT integrations

(use-package gptel
  :defer t
  :bind (("C-s-t" . gptel-rewrite)
         ("C-M-S-g" . gptel))
  :config
  (setq gptel-default-mode 'org-mode
        gptel-api-key (getenv "OPENAI_KEY")
        gptel-model 'gemini-3-pro-preview
        gptel-backend (gptel-make-gemini "Gemini"
                        :key (getenv "GEMINI_KEY")
                        :stream t)))

(provide 'gpt)
;;; gpt.el ends here
