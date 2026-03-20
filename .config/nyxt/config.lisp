(define-configuration nyxt/mode/proxy:proxy-mode
     ((nyxt/mode/proxy:proxy (make-instance 'nyxt:proxy
                                            :url (quri:uri "http://starfox:3128")
                                            ;; Optional: bypass the proxy for local addresses
                                            :allowlist '("localhost" "127.0.0.1")
                                            ;; Optional: route downloads through the proxy as well
                                            :proxied-downloads-p t))))

;; 2. Enable proxy-mode by default for all new web buffers
(define-configuration web-buffer
    ((default-modes (append '(nyxt/mode/proxy:proxy-mode blocker-mode) %slot-value%))))

(define-configuration buffer
    ((default-modes (pushnew 'nyxt/mode/emacs:emacs-mode %slot-value%))))
