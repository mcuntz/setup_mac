;; -*- mode: emacs-lisp; lexical-binding: t -*-

;; quarto

(use-package quarto-mode)

;; https://emacs.stackexchange.com/questions/74478/evaluate-single-python-code-blocks-in-a-quarto-file-like-in-r-studio-or-jupyter
(defun poly-python-eval-region (beg end msg)
  (python-shell-send-region beg end nil msg t))

(defun poly-python-mode-setup ()
  (setq-local polymode-eval-region-function #'poly-python-eval-region))

(add-hook 'python-mode-hook #'poly-python-mode-setup)
