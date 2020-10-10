;; this enable syntax highlight on org-mode org |> latex |> pdf
(require 'manoel "~/.emacs.d/personal/preload/mano.el")

(lerax-require-packages '(org-ref))

(require 'org-ref)
(require 'ox-latex)


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
  (call-process-shell-command "rm -rf *.tex* *.fdb* &" nil 0))

(advice-add 'org-latex-export-to-pdf :after #'clean-export-pdf)
(advice-add 'org-beamer-export-to-pdf :after #'clean-export-pdf)
