(require 'ispell)

(defvar --additional-extension "")
(when (eq system-type 'windows-nt)
  (setq --additional-extension ".exe"))

(defvar hunspell-name (format "hunspell%s" --additional-extension))

(defvar aspell-name (format "aspell%s" --additional-extension))


;; only provide this shortcuts and changes if hunspell is available
(when nil (file-exists-p (or (executable-find hunspell-name) "/not/found/"))
  ;; personal setup for using multiple dictionaries with hunspell
  ;; you need: hunspell and hunspell-pt-br
  (with-eval-after-load 'ispell
    (setq ispell-program-name hunspell-name)
    (setq ispell-hunspell-dictionary-alist
          '(("english" "[[:alpha:]]" "[^[:alpha:]]" "[']" t ("-d" "en_US") nil utf-8)
            ("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" t ("-d" "en_US") nil utf-8)
            ("pt_BR" "[[:alpha:]]" "[^[:alpha:]]" "[']" t ("-d" "pt_BR") nil utf-8)
            ("brasileiro" "[[:alpha:]]" "[^[:alpha:]]" "[']" t ("-d" "pt_BR") nil utf-8)
            ("dev" "[[:alpha:]]" "[^[:alpha:]]" "[']" t ("-d" "pt_BR,en_US") nil utf-8)))
    (setq-default ispell-dictionary "dev") ;; default dictionary for hunspell
    (setq ispell-dictionary-alist ispell-hunspell-dictionary-alist))

  ;; only available for hunspell
  (global-set-key [C-f6]
                  (lambda ()
                    (interactive)
                    (when (not (equal ispell-program-name hunspell-name))
                      (setq-default ispell-program-name hunspell-name))
                    (ispell-change-dictionary "dev")))

  ;; toggle program for ispell: aspell/hunspell
  (global-set-key [C-f5]
                  (lambda ()
                    (interactive)
                    (message "Ispell program set to: %s"
                             (if (equal ispell-program-name hunspell-name)
                                 ;; default for aspell
                                 (progn (setq-local ispell-dictionary "english")
                                        (setq-local ispell-program-name aspell-name))
                               ;; default dictionary for hunspell
                               (setq-local ispell-dictionary "dev")
                               (setq-local ispell-program-name hunspell-name))))))
