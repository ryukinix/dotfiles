;;; mano.el -- useful macros and functions made by me, Manoel

;; Copyright © 2017 Manoel Vilela
;;
;; Author: Manoel Vilela <manoel_vilela@engineer.com>
;; URL: https://github.com/ryukinix/dotfiles
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; I'm trying organize my utils functions.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

;; sorry
(require 'cl-lib)
(require 'package)

;; FIXME: remove me
(when (and (= emacs-major-version 26)
           (< emacs-minor-version 3))
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

;; accessing a package repo over https on Windows is a no go, so we
;; fallback to http there
(unless nil ; package--initialized
  (if (eq system-type 'windows-nt)
      (add-to-list 'package-archives
                   '("melpa" . "http://melpa.org/packages/") t)
    (add-to-list 'package-archives
                 '("melpa" . "https://melpa.org/packages/") t))

  (package-initialize))

(defgroup lerax nil
  "My variable group collection"
  :prefix "lerax-"
  :group 'convenience)

(defmacro try-quote (symbol)
  "Quote the SYMBOL if is not quoted.
SYMBOL is any datum."
  (or (and (consp symbol)
           (eq (car symbol) 'quote)
           symbol)
      `(quote ,symbol)))

(defmacro when-system (system &rest body)
  "Only eval BODY when SYSTEM is the name/alias of the running system.
SYSTEM can be a unquoted symbol designing a `system-type'
or one of its alias: linux, windows and mac-os.
BODY is the rest of eval forms to be executed when SYSTEM is equal
to `system-type'."
  (let ((system-alist (make-symbol "system-alist"))
        (system-symbol (make-symbol "system-symbol"))
        (quoted-system (make-symbol "quoted-system")))
    `(let* ((,system-alist '((linux gnu/linux)
                             (mac-os darwin)
                             (windows windows-nt)))
            (,quoted-system (try-quote ,system))
            (,system-symbol (or (second (assoc ,quoted-system ,system-alist))
                              ,quoted-system)))
       (when (eq system-type ,system-symbol)
         ,@body))))


(defmacro memoize (func &rest body)
  "Create a lexical cache for the given FUNC.
FUNC must be a pure function of only one parameter.
BODY is the rest of eval forms to be used FUNC memoized."
  `(let ((table (make-hash-table))
         (old-func (symbol-function ',func)))
     (labels ((,func (x)
                     (if (not (null (nth-value 1 (gethash x table))))
                         (gethash x table)
                       (setf (gethash x table)
                             (funcall old-func x)))))
       (setf (symbol-function ',func) #',func)
       (prog1 ,@body
         (setf (symbol-function ',func) old-func)))))

(defun tab-mode ()
  "Toggle `indent-tabs-mode'."
  (interactive)
  (progn (setq-local indent-tabs-mode (not indent-tabs-mode))
         (message "%s" indent-tabs-mode))
  (if indent-tabs-mode
      (setq-local backward-delete-char-untabify-method 'hungry)
    (setq-local backward-delete-char-untabify-method 'untabify)))

;;;###autoload
(defun lerax-c-reformat-region (&optional b e)
  "Format the region selected with clang-format -style=LLVM"
  (interactive "r")
  (when (not (buffer-file-name))
    (error "A buffer must be associated with a file in order to use REFORMAT-REGION."))
  (when (not (executable-find "clang-format"))
    (error "clang-format not found."))
  (shell-command-on-region b e
                           "clang-format -style=LLVM"
                           (current-buffer) t)
  (indent-region b e))

;;; Jump to a random line
; sburke@cpan.org
;;;###autoload
(defun goto-random-line ()
  "Go to a random line in this buffer."
                                        ; good for electrobibliomancy.
  (interactive)
  (forward-line (1+ (random (buffer-line-count)))))

(defun buffer-line-count ()
  "Return the number of lines in this buffer."
  (count-lines (point-min) (point-max)))

;; stoled from prelude, trying to decouple my config from prelude by steps

(defun lerax-packages-installed-p (packages)
  "Check if all packages in `packages' are installed."
  (cl-every #'package-installed-p packages))

(defun package-refresh-contents-once ()
  (defvar lerax-package-refreshed nil)
  (unless lerax-package-refreshed
    (package-refresh-contents)
    (setq lerax-package-refreshed t)))

(defun lerax-require-package (package)
  "Install PACKAGE unless already installed."
  (push package package-selected-packages)
  (unless (package-installed-p package)
    (package-refresh-contents-once)
    (package-install package)))

(defun lerax-require-packages (packages)
  "Ensure PACKAGES are installed.
Missing packages are installed automatically."
  (mapc #'lerax-require-package packages))

(defun lerax-memory-ram ()
  "Return total RAM available in the host system"
  (when (eq system-type 'gnu/linux)
    (string-to-number
     (shell-command-to-string "free --giga  | grep Mem | awk '{print $2}'"))))

(defun lerax-remove-tags (text)
  (replace-regexp-in-string "</?[A-Z]+>" "" text))

(defun lerax-add-tag (tag)
  "Wrap a XML-like TAG into a specific text region"
  (interactive (list
                (completing-read "TAG: " '("JOB" "PERSON" "DATE"))))
  (let* ((begin (region-beginning))
         (end (region-end))
         (text (buffer-substring-no-properties begin end)))
    (delete-region (region-beginning) (region-end))
    (insert
     (format "<%s>" tag)
     (lerax-remove-tags text)
     (format "</%s>" tag))
    (skip-chars-forward "\n")))

(defun lerax-delete-tag ()
  "Delete a XML-like TAG of a specific text region"
  (interactive)
  (let* ((begin (region-beginning))
         (end (region-end))
         (text (buffer-substring-no-properties begin end)))
    (delete-region (region-beginning) (region-end))
    (insert (lerax-remove-tags text))
    (skip-chars-forward "\n")))




(provide 'manoel)
;;; mano.el ends here
