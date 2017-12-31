;;; -*- lexical-binding: t -*-

;; Basic Defaults
(tool-bar-mode     -1)
(toggle-scroll-bar -1)
(delete-selection-mode)
(column-number-mode)

(setq-default inhibit-splash-screen t
              indent-tabs-mode nil
              x-select-enable-clipboard t
              x-stretch-cursor nil
              bidi-display-reordering nil)

(global-set-key (kbd "C-x C-b") 'ibuffer)
(define-key isearch-mode-map (kbd "C-n") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "C-p") 'isearch-repeat-backward)


;; Ido
(setq-default ido-everywhere t
              ido-max-window-height 1)
(ido-mode 1)
(add-to-list 'ido-ignore-buffers "^*")

(defun ido-define-keys ()
  (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
  (define-key ido-completion-map (kbd "C-p") 'ido-prev-match))
(add-hook 'ido-setup-hook 'ido-define-keys)


;; Alternative for if Smex is not installed
(unless (package-installed-p 'smex)
  (defun smex-backup ()
    (interactive)
    (call-interactively
     (intern (ido-completing-read
              "M-x " (all-completions "" obarray 'commandp)))))
  (global-set-key "\M-x" 'smex-backup))


;; Dired
(require 'dired-x)
(put 'dired-find-alternate-file 'disabled nil)
(setq dired-omit-files "^\\...+$")
(add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1)))


;; EShell
(require 'eshell)
(defun eshell/clear (&optional _scrollback)
  (let ((eshell-buffer-maximum-lines 0))
    (eshell-truncate-buffer)))


;; Org
(setq-default org-hide-emphasis-markers t
              org-src-tab-acts-natively t
              org-src-fontify-natively t
              org-fontify-whole-heading-line t
              org-fontify-quote-and-verse-blocks t
              org-fontify-done-headline t
              org-edit-src-content-indentation 0)


;; Windows and Display
(setq-default split-height-threshold 0
              split-width-threshold nil
              window-sides-vertical t)
(add-to-list
 'display-buffer-alist
 `("^\*.*"
   (display-buffer-reuse-window
    display-buffer-in-side-window)
   (reusable-frames . visible)
   (side            . bottom)
   (window-height   . 0.2)))

(defun lunaryorn-quit-bottom-side-windows ()
  "Quit side windows of the current frame."
  (interactive)
  (dolist (window (window-at-side-list))
    (quit-window nil window)))
(global-set-key (kbd "C-c q") #'lunaryorn-quit-bottom-side-windows)
