;;; package --- tools for programming ;; -*- mode: emacs-lisp; lexical-binding: t -*-

;;; Code:


;; code folding

(add-hook 'prog-mode-hook 'hs-minor-mode)


;; highlight-parentheses

;; https://emacs.stackexchange.com/questions/27036/enable-show-paren-mode-in-init-el
(use-package highlight-parentheses
   :ensure t
   :config 
   (setq hl-paren-background-colors '("orangered4"))
   (setq hl-paren-colors '("black")))

(electric-pair-mode 1)


;; R

(use-package ess
  :config
  (progn (add-to-list 'auto-mode-alist '("\\.[rR]" . R-mode))))


;; yaml

(use-package yaml-mode
  :config
  (progn (add-to-list 'auto-mode-alist '("\\.ya?ml" . yaml-mode))))


(provide 'me-prog)

;;; me-prog.el ends here
