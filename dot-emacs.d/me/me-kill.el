;; -*- mode: emacs-lisp; lexical-binding: t -*-

;; kill ring

;; https://github.com/mwfogleman/.emacs.d/
(use-package browse-kill-ring
  :bind ("C-x C-y" . browse-kill-ring)
  :config
  (setq browse-kill-ring-quit-action 'kill-and-delete-window))

(setq save-interprogram-paste-before-kill t)
