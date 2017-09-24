;; set all the defaults about coding system to utf-8 with unix endlines
;; this is only necessary on windows
(when (eq system-type 'windows-nt)
  (prefer-coding-system 'utf-8-unix)
  (set-default-coding-systems 'utf-8-unix)
  (set-terminal-coding-system 'utf-8-unix)
  (set-keyboard-coding-system 'utf-8-unix)
  (set-file-name-coding-system 'latin-1) ;; some how utf-8 doesn't works here
  (setq-buffer-file-coding-system 'utf-8-unix)

  ;; Treat clipboard input as UTF-8 string first; compound text next, etc.
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))
