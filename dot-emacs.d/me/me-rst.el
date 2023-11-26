;; -*- mode: emacs-lisp; lexical-binding: t -*-

;; rst

(eval-after-load "rst"
  '(progn
     (setq make-backup-files t)))

(use-package sphinx-mode)
(add-hook 'rst-mode #'sphinx-mode)
