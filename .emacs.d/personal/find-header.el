;;; find-header.el --- Find header of C/C++ files
;; Author: Tatsuhiko Kubo
;; This elisp can open header file on current line.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.


;;; Commentary:
;; This is useful as fuck, but would be nice bind the M-, to go back
;; on stack pointer cursor.  That way calling nested find-header
;; would not be messy.

(require 'cc-mode)
;;; Code:

(setq-default find-header-file-header-file-prefixes (list "/usr/include/"
                                                          "/usr/local/include/"
                                                          "/usr/include/c++/7.1.2/"
                                                          "/usr/lib/gcc/x86_64-pc-linux-gnu/7.2.1/include/"))

(defun find-header-file-current-char ()
  "Get the next char after point and return as string."
  (char-to-string (char-after (point))))

(defun find-header-file-current-line-string ()
  "Get current line as string."
  (let ((line-string ""))
    (save-excursion
      (while (not (bolp))
       (backward-char))
      (while (not (eolp))
       (setq line-string (concat line-string (find-header-file-current-char)))
       (forward-char)))
    line-string))

(defun find-header-file-buffer-on-path (prefix-list filename)
  "Get path of the header from PREFIX-LIST paths based on FILENAME string."
  (if (null (car prefix-list))
      nil
    (if (file-exists-p (concat (car prefix-list) filename))
       (find-file-noselect (concat (car prefix-list) filename))
      (find-header-file-buffer-on-path (cdr prefix-list) filename))))

(defun find-header-file ()
  "Open the buffer of the current cursor line header."
  (interactive)
  (let ((current-line-string (find-header-file-current-line-string))
        (header-file-buffer nil))
    (cond ((string-match "^\\s-*#\\s-*include\\s-*<\\s-*\\([^< ]+\\)\\s-*>" current-line-string)
           (let ((header-file-path (match-string 1 current-line-string)))
             (setq header-file-buffer (find-header-file-buffer-on-path find-header-file-header-file-prefixes
                                                                       header-file-path))))
          ((string-match "^\\s-*#\\s-*include\\s-*\"\\([^\"]+\\)\"\\s-*" current-line-string)
           (let* ((header-file-path (match-string 1 current-line-string))
                  (buffer           (if (file-exists-p (concat default-directory header-file-path))
                                        (find-file-noselect (concat default-directory header-file-path))
                                      nil)))
             (setq header-file-buffer buffer)
             (if (null header-file-buffer)
                 (setq header-file-buffer (find-header-file-buffer-on-path find-header-file-header-file-prefixes
                                                                           header-file-path))
               nil)))
          (t nil))
    (if (null header-file-buffer)
        (message "not found header file")
      (switch-to-buffer header-file-buffer))))

;; binding keys for C and C++ to C-c C-. on `find-header-file' function
(define-key c++-mode-map (kbd "C-c C-.") 'find-header-file)
(define-key c-mode-map (kbd "C-c C-.") 'find-header-file)

(provide 'find-header)

;;; find-header.el ends here
