;; -*- mode: emacs-lisp; lexical-binding: t -*-

;; flycheck

(defun is-mac-p
    ()
  (eq system-type 'darwin))

(defun is-linux-p
    ()
  (eq system-type 'gnu/linux))

(use-package flycheck
  :custom
  (flycheck-checker-error-threshold 800)
  (flycheck-disabled-checkers '(python-mypy)))
(when (or (is-mac-p) (is-linux-p))
  (global-flycheck-mode))

;; (setq flycheck-checker-error-threshold 800)
;; (setq-default flycheck-disabled-checkers '(python-mypy))
