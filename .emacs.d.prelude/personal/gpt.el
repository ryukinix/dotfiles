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
;;

(use-package gpt
  :bind (("C-M-g" . gpt-chat-no-context)
         ("C-M-S-g" . gpt-chat))

  :hook
  (gpt-mode . (lambda ()
                (interactive)
                (whitespace-mode -1))))

;;; FIXME(@lerax): dom 15 jun 2025 15:28:24
;; some fixes was made and sent to upstream gpt.el + melpa
;; I reported the issues here: https://github.com/stuhlmueller/gpt.el/issues/24
;;
;; however, in the build of 20250610 I still have the bug of gpt-mode
;; not being declared properly + env vars not being loaded. Maybe I should delete .elc cache?
;;
;; but anyway, moving config to with-aval-after-load 'gpt-core worked as workaround
(with-eval-after-load 'gpt-core
  (require 'gpt-mode) ;; otherwise raise error of gpt-mode is not defined!
  (setq gpt-api-type 'openai)
  (setq gpt-openai-key (getenv "OPENAI_KEY"))
  (setq gpt-model "gpt-4o"))
