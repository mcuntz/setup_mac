;; -*- mode: emacs-lisp; lexical-binding: t -*-

;; LaTeX

(add-hook 'latex-mode #'flyspell-mode)

(setq latex-build-command "LatexMk")
