;; -*- lexical-binding: t -*-

(require 'ispell)

(defvar hunspell:extension "")
(when (eq system-type 'windows-nt)
  (setq hunspell:extension ".exe"))

(defvar hunspell:hunspell-name (format "hunspell%s" hunspell:extension))

(defvar hunspell:aspell-name (format "aspell%s" hunspell:extension))

(defvar hunspell:hunspell-exists
  (file-exists-p (or (executable-find hunspell:hunspell-name) "/not/found/")))

(defvar hunspell:default-spell-program hunspell:hunspell-name)
(defvar hunspell:hunspell-dict "en_US,pt_BR")
(defvar hunspell:aspell-dict "en_US")

(defun hunspell:select-spell-program (spell-name)
  (cond ((equal spell-name hunspell:hunspell-name)
         (setq ispell-program-name hunspell:hunspell-name)
         (setq ispell-dictionary hunspell:hunspell-dict)
         (ispell-set-spellchecker-params)
         (ispell-hunspell-add-multi-dic hunspell:hunspell-dict)
         (ispell-change-dictionary hunspell:hunspell-dict))
        ((equal spell-name hunspell:aspell-name)
         (setq ispell-dictionary hunspell:aspell-dict)
         (setq ispell-program-name hunspell:aspell-name)
         (ispell-set-spellchecker-params)
         (ispell-change-dictionary hunspell:aspell-dict))))

(defun hunspell:activate ()
  ;; only provide this shortcuts and changes if hunspell is available
  (when (and hunspell:hunspell-exists
             (equal hunspell:default-spell-program hunspell:hunspell-name))

    (setq ispell-dictionary hunspell:hunspell-dict)
    (hunspell:select-spell-program hunspell:hunspell-name))

  ;; enable hunspell shit thing
  (global-set-key [C-f6]
                  (lambda ()
                    (interactive)
                    (hunspell:select-spell-program hunspell:hunspell-name)))

  ;; enable aspell program
  (global-set-key [C-f5]
                  (lambda ()
                    (interactive)
                    (hunspell:select-spell-program hunspell:aspell-name)))
  (global-set-key [M-f5] 'flyspell-mode))


(when (eq system-type 'gnu/linux)
  (hunspell:activate))
