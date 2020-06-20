;; this enable syntax highlight on org-mode org |> latex |> pdf
(require 'manoel "~/.emacs.d/personal/preload/mano.el")

;; disabled due bug at docker-lerax: Sat 20 Jun 2020 03:36:44 PM -03
;; File error: https://melpa.org/packages/tablist-20200427.2205.tar, Bad Request
;; org-ref -> pdf-tools -> tablist
;; I'm not sure the reason for that ????

;; (lerax-require-packages '(org-ref))
;; (require 'org-ref)

(require 'ox-latex)
(require 'org-bibtex)
(with-eval-after-load 'ox-latex
  (setq org-latex-listings 'minted
        org-latex-minted-options '(("frame" "lines")
                                   ("linenos" "true"))
        org-preview-latex-default-process 'imagemagick))

;; just increase the preview image
(plist-put org-format-latex-options :scale 1.2)

(defun clean-export-pdf (&rest _)
  (call-process-shell-command "rm -rf *.tex* *.fdb* &" nil 0))

(advice-add 'org-latex-export-to-pdf :after #'clean-export-pdf)
(advice-add 'org-beamer-export-to-pdf :after #'clean-export-pdf)
