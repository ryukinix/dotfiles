;;; Additional modules required
(require 'prelude-packages)

(when (eq system-type 'windows-nt )
  (prelude-require-package 'intero))

(prelude-require-package 'slime-company)
