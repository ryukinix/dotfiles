;; Workarounds: making my packages working on any emacs version

(when (version<= "25.3" emacs-version) ;; only for pos-25.4
  (setq default-fill-column fill-column)
  (defalias 'default-fill-column 'fill-column
    "Trying fix matlab-mode for new versions.
     They are using this old, possibily, deprecated
     variable which at least is not defined on 27.0"))
