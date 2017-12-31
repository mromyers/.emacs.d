;;; -*- lexical-binding: t -*-

;; emacsclient messes up themes. Unmess it up.
(defun my-init-theme (&optional _frame)
  (mapc #'load-theme custom-enabled-themes))

(defun my-reload-theme-in-daemon (_frame)
  (run-with-timer 0 nil #'my-init-theme)
  (remove-hook 'after-make-frame-functions
               #'my-reload-theme-in-daemon))

(add-hook 'after-make-frame-functions
          #'my-reload-theme-in-daemon)

