;; -*- lexical-binding: t -*-
;; Workarounds: making my packages working on any emacs version

(require 'manoel "~/.emacs.d/personal/preload/mano.el")
(require 'cl-lib)
(require 'subr-x)


;; related with not expand <s blocks on org-mode with emacs27
;; ref: https://github.com/syl20bnr/spacemacs/issues/11798#issuecomment-454941024
(when (version<= "9.2" (org-version))
  (require 'org-tempo))

;; ref: https://github.com/bbatsov/prelude/issues/1225
;; error: Package ‘undo-tree-’ is unavailable
;; affects emacs 26.1 and 26.2, fixed in emacs 26.3
(when (and (= emacs-major-version 26)
           (< emacs-minor-version 3))
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

(when (version<= "25.3" emacs-version) ;; only for pos-25.4
  (message "HOTFIX: fill-column matlab-mode fix")
  (defvar default-fill-column fill-column)
  (defalias 'default-fill-column 'fill-column
    "Trying fix matlab-mode for new versions.
     They are using this old, possibily, deprecated
     variable which at least is not defined on 27.0"))

;; this is necessary because a weird bug related to the haskell-mode and new ghc 8.2.1
;; module load interface at REPL
;; main issue here: https://github.com/haskell/haskell-mode/issues/1553
;; which seems fixed at: https://github.com/haskell/haskell-mode/pull/1625
(with-eval-after-load 'haskell-mode
  (defconst ghc-version (when (executable-find "ghc")
                          (string-trim (shell-command-to-string "ghc --numeric-version"))))

  (when (and ghc-version
             (version<= "8.2.1" ghc-version))
    (message "HOTFIX: haskell-mode hacks")
   (defconst haskell-process-args-ghci
     '("-ferror-spans" "-fshow-loaded-modules"))
   (defconst haskell-process-args-cabal-repl
     '("--ghc-options=-ferror-spans -fshow-loaded-modules"))
   (defconst haskell-process-args-stack-ghci
     '("--ghci-options=-ferror-spans -fshow-loaded-modules"
       "--no-build" "--no-load"))
   (defconst haskell-process-args-cabal-new-repl
     '("--ghc-options=-ferror-spans -fshow-loaded-modules"))))


(with-eval-after-load 'exec-path-from-shell
  (message "HOTFIX: exec-path-from-shell remove -i flag")
  (setq exec-path-from-shell-arguments (delete "-i" exec-path-from-shell-arguments) ))


(defun file-readlines (file)
  "FILE-READLINES reads FILE and return a LIST of STRINGS."
  (with-temp-buffer
    (insert-file-contents file)
    (split-string (buffer-string) "\n" t)))

;; dbus stuff
(defun fetch-dbus-address (&optional bus private)
  "FETCH-DBUS-ADDRESS reads the ~/.dbus/session-bus/ first file
parses and set DBUS_SESSION_BUS_ADDRES to its expected value."
  (ignore bus private)
  (let* ((file (car (directory-files "~/.dbus/session-bus/" t ".*-0")))
         (varname "DBUS_SESSION_BUS_ADDRESS")
         (regexp (format "^%s=" varname))
         (value nil))
    (when file
      (setq value (car (cl-remove-if-not (lambda (x)
                                        (equal (string-match regexp x) 0))
                                      (file-readlines file))))
      (setq value (replace-regexp-in-string regexp "" value))
      (setq value (replace-regexp-in-string "'" "" value))
      value)))


;; (advice-add 'dbus-init-bus :before #'fetch-dbus-address)

;; c-mode bullshitness
;; ref: https://github.com/Fuco1/smartparens/issues/783
;; date: Mon 11 Mar 2019 11:44:11 PM -03
;; avoid auto escape insertion of c-mode single quotes
(with-eval-after-load 'smartparens
  ;; 'c-mode-common-hook maybe help
  (add-hook 'c-mode-common-hook
            (lambda ()
              (setq-local sp-escape-quotes-after-insert nil))))

;; lab doesn't works fine
;; ref: https://emacs.stackexchange.com/questions/48437/how-to-integrate-emacs-lab-magit/48439#48439
(with-eval-after-load 'git-commit
  (defconst git-commit-filename-regexp
    "/\\(\\\(\\(COMMIT\\|NOTES\\|PULLREQ\\|TAG\\|MERGEREQ\\)_EDIT\\|MERGE_\\|\\)MSG\\\|\\(BRANCH\\|EDIT\\|_EDITMSG\\)_DESCRIPTION\\)\\'"))


;; Sat 18 Jul 2020 08:47:28 AM -03
;; arrow keys is not working on terminal
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (when (not (display-graphic-p frame))		;; Only use in tty-sessions.
              (defvar arrow-keys-map (make-sparse-keymap) "Keymap for arrow keys")
              (define-key esc-map "O" arrow-keys-map)
              (define-key arrow-keys-map "A" 'previous-line)
              (define-key arrow-keys-map "B" 'next-line)
              (define-key arrow-keys-map "C" 'forward-char)
              (define-key arrow-keys-map "D" 'backward-char))))
