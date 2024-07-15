;; -*- mode: emacs-lisp; lexical-binding: t -*-

;; flycheck

(defun is-mac-p
    ()
  (eq system-type 'darwin))

(use-package flycheck
  :custom
  (flycheck-checker-error-threshold 800)
  (flycheck-disabled-checkers '(python-mypy)))
(when (is-mac-p)
  (global-flycheck-mode))

;; (setq flycheck-checker-error-threshold 800)
;; (setq-default flycheck-disabled-checkers '(python-mypy))
