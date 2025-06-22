;; this enable syntax highlight on org-mode org |> latex |> pdf

(require 'ox-latex)
(progn ;; citations
  (require 'oc-biblatex)
  (require 'oc-natbib)
  (require 'oc-csl)
  )


(defcustom lerax-latex-listing 'minted
  "If `lerax-latex-listing' is minted enable syntax highlight"
  :group 'lerax
  :type 'symbol)

(with-eval-after-load 'ox-latex
  (setq org-latex-listings lerax-latex-listing
        org-latex-minted-options '(("frame" "lines")
                                   ("linenos" "true"))
        org-preview-latex-default-process 'imagemagick))

;; just increase the preview image
(plist-put org-format-latex-options :scale 1.2)

(defun clean-export-pdf (&rest _)
  (let* ((fname (file-name-base (buffer-name)))
        (pattern (format "%s.!(pdf|org)" fname))
        (cmd (format "bash -c 'shopt -s extglob; rm -rf %s &'" pattern)))
   (call-process-shell-command cmd nil 0)))

(advice-add 'org-latex-export-to-pdf :after #'clean-export-pdf)
(advice-add 'org-beamer-export-to-pdf :after #'clean-export-pdf)
