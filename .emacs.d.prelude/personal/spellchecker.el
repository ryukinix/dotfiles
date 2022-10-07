;; -*- lexical-binding: t -*-

(require 'ispell)

(defvar spellchecker:extension "")
(when (eq system-type 'windows-nt)
  (setq spellchecker:extension ".exe"))

(defvar spellchecker:hunspell-name (format "hunspell%s" spellchecker:extension))

(defvar spellchecker:aspell-name (format "aspell%s" spellchecker:extension))

(defvar spellchecker:hunspell-exists
  (file-exists-p (or (executable-find spellchecker:hunspell-name) "/not/found/")))

(defvar spellchecker:default-spell-program spellchecker:aspell-name)
(defvar spellchecker:hunspell-dict "pt_BR")
(defvar spellchecker:aspell-dict "en_US")

(defun spellchecker:select-spell-program (spell-name)
  (message "%s"
           (print (cond ((equal spell-name spellchecker:hunspell-name)
                         (setq ispell-program-name spellchecker:hunspell-name))
                        ((equal spell-name spellchecker:aspell-name)
                         (setq ispell-program-name spellchecker:aspell-name))))))

(defun spellchecker:activate ()
  ;; only provide this shortcuts and changes if hunspell is available
  (when (and spellchecker:hunspell-exists
             (equal spellchecker:default-spell-program spellchecker:hunspell-name))

    (setq ispell-dictionary spellchecker:hunspell-dict)
    (spellchecker:select-spell-program spellchecker:hunspell-name))

  ;; enable hunspell shit thing
  (global-set-key [C-f6]
                  (lambda ()
                    (interactive)
                    (spellchecker:select-spell-program spellchecker:hunspell-name)))

  ;; enable aspell program
  (global-set-key [C-f5]
                  (lambda ()
                    (interactive)
                    (spellchecker:select-spell-program spellchecker:aspell-name)))
  (global-set-key [M-f5] 'flyspell-mode))


(when (eq system-type 'gnu/linux)
  (spellchecker:activate))
