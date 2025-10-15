(defun projectile-todo ()
  (interactive)
  (projectile-ripgrep "\\b(TODO|FIXME)\\b" t))

(defun projectile-todo-all ()
  (interactive)
  (let ((pattern (string-join (map 'list 'car hl-todo-keyword-faces) "|")))
    (projectile-ripgrep (format "\\b(%s)\\b"pattern) t)))
