;; -*- lexical-binding: t -*-

(require 'ispell)
(require 'manoel)

(defvar --additional-extension "")
(when (eq system-type 'windows-nt)
  (setq --additional-extension ".exe"))

(defvar hunspell-name (format "hunspell%s" --additional-extension))

(defvar aspell-name (format "aspell%s" --additional-extension))

(defvar hunspell-exists (file-exists-p (or (executable-find hunspell-name) "/not/found/")))

(defvar default-spell-program hunspell-name)
(defvar hunspell-dict "en_US,pt_BR")
(defvar aspell-dict "en_US")

(defun select-spell-program (spell-name)
  (cond ((equal spell-name hunspell-name)
         (setq ispell-program-name hunspell-name)
         (setq ispell-dictionary hunspell-dict)
         (ispell-set-spellchecker-params)
         (ispell-hunspell-add-multi-dic hunspell-dict)
         (ispell-change-dictionary hunspell-dict))
        ((equal spell-name aspell-name)
         (setq ispell-dictionary aspell-dict)
         (setq ispell-program-name aspell-name)
         (ispell-set-spellchecker-params)
         (ispell-change-dictionary aspell-dict))))

(defun hunspell:activate ()
  ;; only provide this shortcuts and changes if hunspell is available
  (when (and hunspell-exists
             (equal default-spell-program hunspell-name))

    (setq ispell-dictionary hunspell-dict)
    (select-spell-program hunspell-name))

  ;; enable hunspell shit thing
  (global-set-key [C-f6]
                  (lambda ()
                    (interactive)
                    (select-spell-program hunspell-name)))

  ;; enable aspell program
  (global-set-key [C-f5]
                  (lambda ()
                    (interactive)
                    (select-spell-program aspell-name)))
  (global-set-key [M-f5] 'flyspell-mode))


(when-system 'linux
             (hunspell:activate))
