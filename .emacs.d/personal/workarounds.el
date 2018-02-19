;; Workarounds: making my packages working on any emacs version

(when (version<= "25.3" emacs-version) ;; only for pos-25.4
  (setq default-fill-column fill-column)
  (defalias 'default-fill-column 'fill-column
    "Trying fix matlab-mode for new versions.
     They are using this old, possibily, deprecated
     variable which at least is not defined on 27.0"))

;; this is necessary because a weird bug related to the haskell-mode and new ghc 8.2.1
;; module load interface at REPL
(with-eval-after-load 'haskell-mode
  (setq haskell-process-args-ghci
        '("-ferror-spans" "-fshow-loaded-modules"))
  (setq haskell-process-args-cabal-repl
        '("--ghc-options=-ferror-spans -fshow-loaded-modules"))
  (setq haskell-process-args-stack-ghci
        '("--ghci-options=-ferror-spans -fshow-loaded-modules"
          "--no-build" "--no-load"))
  (setq haskell-process-args-cabal-new-repl
        '("--ghc-options=-ferror-spans -fshow-loaded-modules")))
