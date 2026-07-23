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

(use-package agent-shell
  :ensure t
  :config
  (setq agent-shell-google-authentication
        (agent-shell-google-make-authentication :api-key (lambda () (getenv "GEMINI_KEY")))))

(provide 'gpt)
;;; gpt.el ends here
