;; -*- mode: emacs-lisp; lexical-binding: t -*-

;; yaml

(use-package yaml-mode
  :config
  (progn (add-to-list 'auto-mode-alist '("\\.ya?ml" . yaml-mode))))
