(deftheme leraxy
  "Leraxy it's a theme customization to make easier to read code. It can be used on top of doom-meltbus.")

(custom-theme-set-faces
 'leraxy

 '(font-lock-keyword-face ((t (:foreground "dodger blue"))))
 ;; necessary to ocaml display correctly keywords as dodger blue
 '(tuareg-font-lock-governing-face ((t (:foreground "dodger blue"))))
 '(tuareg-font-lock-governing-face ((t (:foreground "dodger blue"))))
 '(tuareg-font-lock-operator-face ((t (:foreground "dodger blue"))))
 )


(provide-theme 'leraxy)
