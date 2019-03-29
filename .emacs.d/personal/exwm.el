;; -*- lexical-binding: t -*-

(require 'prelude-packages nil t)
(prelude-require-packages '(exwm
                            helm-exwm
                            pulseaudio-control))
(require 'exwm)


(defconst lerax-exwm-autostart-programs
    (let ((host (system-name)))
      (append '("xfce4-clipman"
                "volumeicon"
                "fluxgui")
              (when (equal host "celeste")
                '("xfce4-power-manager"))
              )))


(defun lerax-exwm-rename-buffer ()
  (interactive)
  (exwm-workspace-rename-buffer exwm-class-name) )


;;; Those cannot be set globally: if Emacs would be run in another WM, the "s-"
(defun lerax-switch-to-last-buffer ()
  "Switch to last open buffer in current window."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

;;; Lock screen
(defun lerax-exwm-start-lock ()
  (interactive)
  (start-process "slock" nil "slock"))

;;; Screenshot
(defun lerax-exwm-start-screenshot ()
  (interactive)
  (start-process-shell-command "xfce4-screenshooter"
                               nil
                               "xfce4-screenshooter"))

(defun lerax-setup-exwm ()

  ;; Add these hooks in a suitable place (e.g., as done in exwm-config-default)
  (add-hook 'exwm-update-class-hook 'lerax-exwm-rename-buffer)
  (add-hook 'exwm-update-title-hook 'lerax-exwm-rename-buffer)



  (global-set-key (kbd "C-x C-c") 'save-buffers-kill-emacs)
  (add-hook 'exwm-floating-setup-hook 'exwm-layout-hide-mode-line)
  (add-hook 'exwm-floating-exit-hook 'exwm-layout-show-mode-line)

  ;;; Allow non-floating resizing with mouse.
  (setq window-divider-default-bottom-width 2
        window-divider-default-right-width 2)
  (window-divider-mode)

  ;;; System tray
  (require 'exwm-systemtray)
  (exwm-systemtray-enable)
  (setq exwm-systemtray-height 16)


  ;;; Those cannot be set globally: if Emacs would be run in another WM, the "s-"
  ;;; prefix will conflict with the WM bindings.
  (exwm-input-set-key (kbd "s-R") #'exwm-reset)
  (exwm-input-set-key (kbd "s-x") #'exwm-input-toggle-keyboard)
  (exwm-input-set-key (kbd "s-h") #'windmove-left)
  (exwm-input-set-key (kbd "<s-down>") #'windmove-down)
  (exwm-input-set-key (kbd "<s-up>") #'windmove-up)
  (exwm-input-set-key (kbd "<s-right>") #'windmove-right)
  (exwm-input-set-key (kbd "<s-left>") #'windmove-left)

  (exwm-input-set-key (kbd "s-D") #'kill-this-buffer)
  (exwm-input-set-key (kbd "s-b") #'list-buffers)
  (exwm-input-set-key (kbd "s-F") #'find-file)

  ;; The following can only apply to EXWM buffers, else it could have unexpected effects.
  (push ?\s-  exwm-input-prefix-keys)
  (define-key exwm-mode-map (kbd "s-SPC") #'exwm-floating-toggle-floating)

  (exwm-input-set-key (kbd "s-i") #'follow-delete-other-windows-and-split)
  (exwm-input-set-key (kbd "s-f") #'exwm-layout-toggle-fullscreen)

  (with-eval-after-load 'helm
    ;; Need `with-eval-after-load' here since 'helm-map is not defined in 'helm-config.
    (exwm-input-set-key (kbd "s-c") #'helm-resume)
    (exwm-input-set-key (kbd "s-b") #'helm-mini)
    (exwm-input-set-key (kbd "s-f") #'helm-find-files)
    (exwm-input-set-key (kbd "s-F") #'helm-locate)
    (when (fboundp 'lerax-helm-locate-meta)
      (exwm-input-set-key (kbd "s-F") #'lerax-helm-locate-meta)))


  (exwm-input-set-key (kbd "s-<tab>") #'lerax-switch-to-last-buffer)

;;; Emacs mode shortcuts.
  (exwm-input-set-key (kbd "s-<return>")
                      (lambda ()
                        (interactive)
                        (start-process-shell-command "xfce4-terminal" nil
                                                     "xfce4-terminal")))

  (when (fboundp 'magit-status)
    (exwm-input-set-key (kbd "s-v") #'magit-status))

  (exwm-input-set-key (kbd "s-E") #'eww)

;;; External application shortcuts.
  (defun lerax-exwm-launch (command)
    (interactive (list (read-shell-command "$ ")))
    (start-process-shell-command command nil command))

  (exwm-input-set-key (kbd "s-:") #'lerax-exwm-launch)

  (when (require 'helm-exwm nil t)
    (add-to-list 'helm-source-names-using-follow "EXWM buffers")
    (setq helm-exwm-emacs-buffers-source (helm-exwm-build-emacs-buffers-source))
    (setq helm-exwm-source (helm-exwm-build-source))
    (setq helm-mini-default-sources `(helm-exwm-emacs-buffers-source
                                      helm-exwm-source
                                      helm-source-recentf
                                      ,(when (boundp 'helm-source-ls-git)
                                         'helm-source-ls-git)
                                      helm-source-bookmarks
                                      helm-source-bookmark-set
                                      helm-source-buffer-not-found))
    ;; Launcher
    (exwm-input-set-key (kbd "s-r") 'helm-run-external-command)
    ;; Web browser
    (exwm-input-set-key (kbd "s-w") #'helm-exwm-switch-browser)
    (exwm-input-set-key (kbd "s-W") #'helm-exwm-switch-browser-other-window))


  (exwm-input-set-key (kbd "<print>") #'lerax-exwm-start-screenshot)

  ;;; Volume control
  (when (require 'pulseaudio-control nil t)
    (exwm-input-set-key (kbd "s-<kp-subtract>") #'pulseaudio-control-decrease-volume)
    (exwm-input-set-key (kbd "s-<kp-add>") #'pulseaudio-control-increase-volume)
    (exwm-input-set-key (kbd "s-<kp-enter>") #'pulseaudio-control-toggle-current-sink-mute)

    (exwm-input-set-key (kbd "<XF86AudioLowerVolume>") #'pulseaudio-control-decrease-volume)
    (exwm-input-set-key (kbd "<XF86AudioRaiseVolume>") #'pulseaudio-control-increase-volume)
    (exwm-input-set-key (kbd "<XF86AudioMute>") #'pulseaudio-control-toggle-current-sink-mute)
    (exwm-input-set-key (kbd "<XF86AudioMicMute>") #'pulseaudio-control-toggle-current-source-mute)

    (exwm-input-set-key (kbd "s--") #'pulseaudio-control-decrease-volume)
    (exwm-input-set-key (kbd "s-=") #'pulseaudio-control-increase-volume)
    (exwm-input-set-key (kbd "s-0") #'pulseaudio-control-toggle-current-sink-mute))

;;; Check for start-up errors. See ~/.profile.
  (let ((error-logs (directory-files "~" t "errors.*log$")))
    (when error-logs
      (warn "Error during system startup.  See %s." (mapconcat 'identity error-logs ", "))
      (when (daemonp)
        ;; Non-daemon Emacs already brings up the *Warning* buffer.
        (setq initial-buffer-choice
              (lambda () (get-buffer "*Warnings*"))))))
  (mapcar (lambda (p)
            (start-process-shell-command p nil p))
          lerax-exwm-autostart-programs)
  )


(defun lerax-exwm-start (&optional frame)
  (when (not (null (getenv "EXWM")) )
    (require 'exwm)
    (lerax-setup-exwm)
    (exwm-init frame)))


(add-hook 'after-make-frame-functions
          #'lerax-exwm-start)
