;; this enable syntax highlight on org-mode org |> latex |> pdf
(with-eval-after-load 'ox-latex
  (setq org-latex-listings 'minted
        org-latex-packages-alist '(("" "minted"))
        org-latex-pdf-process
        '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f")))

(defun clean-export-pdf ()
  (interactive)
  (async-shell-command "rm -rfv *.tex _minted*"))
