(require 'ispell)

;; personal setup for using multiple dictionaries with hunspell
;; you need: hunspell and hunspell-pt-br
(with-eval-after-load 'ispell
                (setq ispell-program-name "hunspell.exe")
                 (setq ispell-hunspell-dictionary-alist
                       '(("english" "[[:alpha:]]" "[^[:alpha:]]" "[']" t ("-d" "en_US") nil utf-8)
                         ("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" t ("-d" "en_US") nil utf-8)
                         ("pt_BR" "[[:alpha:]]" "[^[:alpha:]]" "[']" t ("-d" "pt_BR") nil utf-8)
                         ("brasileiro" "[[:alpha:]]" "[^[:alpha:]]" "[']" t ("-d" "pt_BR") nil utf-8)
                         ("dev" "[[:alpha:]]" "[^[:alpha:]]" "[']" t ("-d" "pt_BR,en_US") nil utf-8)))
                 (setq ispell-dictionary "dev") ;; english and portuguese
                 (setq ispell-dictionary-alist ispell-hunspell-dictionary-alist))

;; only available for hunspell
(global-set-key [C-f6] (lambda ()
                         (interactive)
                         (when (not (equal ispell-program-name "hunspell.exe"))
                           (setq ispell-program-name "hunspell.exe"))
                         (ispell-change-dictionary "dev")))

;; toggle program for ispell: aspell/hunspell
(global-set-key [C-f5] (lambda ()
                         (interactive)
                         (message "Ispell program set to: %s"
                                  (if (equal ispell-program-name "hunspell.exe")
                                      (prog1 (setq ispell-program-name "aspell.exe")
                                             (ispell-change-dictionary "english"))
                                    (setq ispell-program-name "hunspell.exe")))))
