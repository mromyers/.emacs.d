;;; -*- lexical-binding: t -*-
(add-to-list
 'package-archives
 '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; Ideally, I want emacs to load up and be functional even if the
;; elpa/ dir vanishes, or I install it from git somewhere and can't
;; reach the package server for some time.

;; use-package is pretty good about not throwing a fit for missing
;; packages, but it does throw up a warning for missing dependencies
;; and half the stuff here depends on 'bind-key, so it's easier to
;; just wrap it like this.
(eval-when-compile
  (require 'use-package))

(when (package-installed-p 'bind-key)
  (require 'bind-key)

  ;;; Core Packages

  (use-package rainbow-delimiters
    :defines rainbow-delimiters-mode
    :config
    (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))
  
  (use-package smex
    :defer t
    :bind (("M-x"         . smex)
           ("M-X"         . smex-major-mode-commands)
           ("C-c C-c M-x" . execute-extended-command)))
  
  (use-package avy
    :defer t
    :bind (("M-j"     . avy-goto-char-timer)
           ("M-g M-j" . avy-goto-char-timer)
           ("M-g M-s" . avy-goto-char)
           ("M-g M-d" . avy-goto-char-2)
           ("M-g M-w" . avy-goto-word-1)
           ("M-g M-e" . avy-goto-word-0)
           ("M-g M-g" . avy-goto-line)
           :map isearch-mode-map
           ("M-g"     . avy-isearch)))

  (use-package company
    :defer t
    :bind (("M-c" . company-complete))
    :functions global-company-mode
    :config
    (global-company-mode)
    (add-to-list 'company-backends 'company-math-symbols-unicode)
    (bind-keys :map company-active-map
               ("C-n"   . company-select-next)
               ("C-p"   . company-select-previous)
               ("<tab>" . company-complete-common)))

  (use-package neotree
    :bind (("<f8>" . neotree-toggle))
    :config
    (add-to-list 'display-buffer-alist
                 '("\\(?: \\*NeoTree\\*\\)"
                   (display-buffer-reuse-window
                    display-buffer-in-side-window)
                   (reusable-frames . visible)
                   (side            . left))))

  (use-package perspective
    :functions persp-mode
    :config
    (persp-mode)
    (bind-keys ("C-x p" . persp-switch)))

  
  ;;; Language Specific Packages

  (use-package tex
    :defer t
    :config
    (add-hook 'LaTeX-mode-hook
              '(lambda ()
                 (define-key LaTeX-mode-map
                   (kbd "$") 'self-insert-command)))

    (setq TeX-auto-save t
          TeX-parse-self t
          reftex-plug-into-AUCTeX t)
    (setq-default TeX-master nil)
  
    (add-hook 'LaTeX-mode-hook 'visual-line-mode)
    (add-hook 'LaTeX-mode-hook 'flyspell-mode)
    (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
    (add-hook 'LaTeX-mode-hook 'turn-on-reftex))

  (use-package markdown-mode
    :ensure t
    :commands (markdown-mode gfm-mode)
    :mode (("README\\.md\\'" . gfm-mode)
           ("\\.md\\'" . markdown-mode)
           ("\\.markdown\\'" . markdown-mode))
    :init (setq markdown-command "multimarkdown"))

  (use-package haskell-mode
    :config
    (add-hook 'haskell-mode-hook 'interactive-haskell-mode))

  ;;; Themes
  
  (use-package doom-themes
    :defer t
    :config
    (setq doom-themes-enable-bold   t ; if nil, bold is universally disabled
          doom-themes-enable-italic t); if nil, italics is universally disabled
    ;; Enable flashing mode-line on errors
    ;;(doom-themes-visual-bell-config)
    ;; Enable custom neotree theme
    (doom-themes-neotree-config)  ; all-the-icons fonts must be installed!
    ;; Corrects (and improves) org-mode's native fontification.
    ;;(doom-themes-org-config)
    )

  ;;; Misc and Single Purpose Modes

  (use-package nov
    :defer t
    :mode (("\\.epub\\'" . nov-mode)))
)
