(use-package le-gpt
  :bind (("C-s-g" . le-gpt-chat)
         ("C-s-n" . le-gpt-complete-at-point)
;;         ("C-s-t" . le-gpt-transform-region) transfered to gptel with gemini
         ("C-s-s" . le-gpt-select-project-files)
         ("C-s-d" . le-gpt-deselect-project-files))
  :config
  (setq le-gpt-openai-key (getenv "OPENAI_KEY"))
  (setq le-gpt-model "gpt-4o")
  (setq le-gpt-api-type 'openai)
  (setq le-gpt-python-path "/usr/bin/python3")
  :hook
  (le-gpt-chat-mode . (lambda ()
                        (whitespace-mode -1))))

;;; NOTE(@lerax): dom 01 jun 2025 12:50:08
;; after upgrade version 20250525 stopped to work
;; - gpt-model custom var points to claude even with setq-default pointing to gpt-4o
;; - gpt-mode major mode Lis not found when called
;;
;; using le-gpt as alternative until got this fixed
;;

(use-package gpt
  :bind (("C-M-g" . gpt-chat-no-context))

  :hook
  (gpt-mode . (lambda ()
                (whitespace-mode -1))))

;;; FIXME(@lerax): Sun, 15 Jun 2025 15:28:24
;; Some fixes have been made and sent to upstream gpt.el + MELPA.
;; I reported the issues here: https://github.com/stuhlmueller/gpt.el/issues/24
;;
;; However, in the build of 20250610, I still encounter the bug of gpt-mode
;; not being declared properly, along with environment variables not being loaded.
;; Perhaps I should delete the .elc cache?
;;
;; Regardless, moving the configuration to `with-eval-after-load 'gpt-core` worked as a workaround.
(with-eval-after-load 'gpt-core
  (require 'gpt-mode) ;; otherwise raise error of gpt-mode is not defined!
  (setq gpt-api-type 'openai)
  (setq gpt-openai-key (getenv "OPENAI_KEY"))
  (setq gpt-model "gpt-4o")
  (setq gpt-python-path "/usr/bin/python3"))

(use-package gptel
  :bind (("C-s-t" . gptel-rewrite)
         ("C-M-S-g" . gptel))
  :config
  (setq gptel-default-mode 'org-mode
        gptel-api-key (getenv "OPENAI_KEY")
        gptel-model 'gemini-2.5-flash
        gptel-backend (gptel-make-gemini "Gemini"
                        :key (getenv "GEMINI_KEY")
                        :stream t)))
