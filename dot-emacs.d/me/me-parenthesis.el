;; -*- mode: emacs-lisp; lexical-binding: t -*-

;; highlight-parentheses

;; https://emacs.stackexchange.com/questions/27036/enable-show-paren-mode-in-init-el
(use-package highlight-parentheses
   :ensure t
   :config 
   (setq hl-paren-background-colors '("orangered4"))
   (setq hl-paren-colors '("black")))

(electric-pair-mode 1)

(provide 'me-parenthesis)
