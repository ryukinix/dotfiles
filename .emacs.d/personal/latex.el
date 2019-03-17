;; this enable syntax highlight on org-mode org |> latex |> pdf
(prelude-require-package 'org-ref)
(require 'ox-latex)
(require 'org-ref)
(require 'org-bibtex)
(with-eval-after-load 'ox-latex
  (setq org-latex-listings 'minted
        org-latex-packages-alist '(("" "minted"))
        org-latex-minted-options '(("frame" "lines")
                                   ("linenos" "true"))
        org-latex-pdf-process
        '("latexmk -shell-escape -bibtex -f -pdf %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f")
        org-preview-latex-default-process 'imagemagick))

;; just increase the preview image
(plist-put org-format-latex-options :scale 1.2)

(defun clean-export-pdf (&rest _)
  (call-process-shell-command "rm -rf *.tex  _minte*&" nil 0)
  (call-process-shell-command "rm -rf _minte*&" nil 0))

(advice-add 'org-latex-export-to-pdf :after #'clean-export-pdf)
(advice-add 'org-beamer-export-to-pdf :after #'clean-export-pdf)
