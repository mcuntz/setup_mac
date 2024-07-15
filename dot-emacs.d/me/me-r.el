;; -*- mode: emacs-lisp; lexical-binding: t -*-

;; R

(use-package ess
  :config
  (progn (add-to-list 'auto-mode-alist '("\\.[rR]" . R-mode))))
