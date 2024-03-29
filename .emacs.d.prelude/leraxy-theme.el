(deftheme leraxy
  "Leraxy it's a theme customization to make easier to read code. It can be used on top of doom-meltbus.")

(custom-theme-set-faces
 'leraxy
 '(cursor ((t (:background "#cecece"))))
 '(fixed-pitch ((t (:family "Monospace"))))
 '(variable-pitch ((((type w32)) (:foundry "outline" :family "Arial")) (t (:family "Sans Serif"))))
 '(escape-glyph ((t (:foreground "#46D9FF"))))
 '(homoglyph ((((background dark)) (:foreground "cyan")) (((type pc)) (:foreground "magenta")) (t (:foreground "brown"))))
 '(minibuffer-prompt ((t (:weight bold :foreground "#8a8a8a" :background "#242424"))))
 '(highlight ((t (:foreground "black" :background "#87afff"))))
 '(region ((t (:extend t :inverse-video t))))
 '(shadow ((t (:foreground "#8a8a8a"))))
 '(secondary-selection ((t (:extend t :background "#686868"))))
 '(trailing-whitespace ((t (:background "#f8b0b0"))))
 '(font-lock-builtin-face ((t (:foreground "#8a8a8a"))))
 '(font-lock-comment-delimiter-face ((default (:inherit (font-lock-comment-face)))))
 '(font-lock-comment-face ((t (:foreground "#686868"))))
 '(font-lock-constant-face ((t (:foreground "#8a8a8a"))))
 '(font-lock-doc-face ((t (:foreground "#8a8a8a" :inherit (font-lock-comment-face)))))
 '(font-lock-doc-markup-face ((t (:inherit (font-lock-constant-face)))))
 '(font-lock-function-name-face ((t (:foreground "#ffffff" :width semi-expanded))))
 '(font-lock-keyword-face ((t (:foreground "dodger blue"))))
 '(font-lock-negation-char-face ((t (:foreground "#8a8a8a" :inherit (bold)))))
 '(font-lock-preprocessor-face ((t (:foreground "#8a8a8a" :inherit (bold)))))
 '(font-lock-regexp-grouping-backslash ((t (:foreground "#8a8a8a" :inherit (bold)))))
 '(font-lock-regexp-grouping-construct ((t (:foreground "#8a8a8a" :inherit (bold)))))
 '(font-lock-string-face ((t (:foreground "#acacac"))))
 '(font-lock-type-face ((t (:foreground "#acacac"))))
 '(font-lock-variable-name-face ((t (:foreground "#dddddd"))))
 '(font-lock-warning-face ((t (:inherit (warning)))))
 '(button ((t (:inherit (link)))))
 '(link ((t (:weight normal :underline nil :foreground "#87afff"))))
 '(link-visited ((t (:inherit (link)))))
 '(fringe ((t (:foreground "#686868" :inherit (default)))))
 '(header-line ((t (:inherit (mode-line)))))
 '(tooltip ((t (:foreground "#dddddd" :background "black"))))
 '(mode-line ((t (:foreground "#dddddd" :background "black"))))
 '(mode-line-buffer-id ((t (:weight bold))))
 '(mode-line-emphasis ((t (:weight bold :foreground "#dddddd"))))
 '(mode-line-highlight ((t (:weight bold :foreground "#dddddd"))))
 '(mode-line-inactive ((t (:foreground "#8a8a8a"))))
 '(isearch ((t (:weight bold :inherit (lazy-highlight)))))
 '(isearch-fail ((t (:weight bold :foreground "black" :background "#f8b0b0"))))
 '(lazy-highlight ((t (:weight bold :foreground "#efefef" :background "#5e7ab2"))))
 '(match ((t (:weight bold :foreground "#448844" :background "black"))))
 '(next-error ((t (:inherit (region)))))
 '(query-replace ((t (:inherit (isearch)))))
 '(default ((t (:family "JuliaMono" :foundry "UKWN" :width normal :height 120 :weight normal :slant normal :underline nil :overline nil :extend nil :strike-through nil :box nil :inverse-video nil :foreground "#dddddd" :background "black" :stipple nil :inherit nil)))))

(provide-theme 'leraxy)
