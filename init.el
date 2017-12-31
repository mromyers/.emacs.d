;;; -*- lexical-binding: t -*-
(let ((file-name-handler-alist nil))
  (setq gc-cons-threshold  402653184
        gc-cons-percentage 0.6)
  (require 'package)
  (package-initialize)

  ;; configurations not depending on external packages
  (load (concat user-emacs-directory "init/core-init"))
  ;; configuration for installed packages
  (load (concat user-emacs-directory "init/package-init"))

  (setq custom-file ;; custom-set-(variables|faces)
        (expand-file-name "custom.el" user-emacs-directory))
  (when (file-exists-p custom-file) (load custom-file))

  (when (daemonp)   ;; configuraton for emacsclient
    (load (concat user-emacs-directory "init/daemon-init")))

  (run-with-idle-timer
   2 nil (lambda()(setq gc-cons-threshold  16777216
                        gc-cons-percentage 0.1))))
