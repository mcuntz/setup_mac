;; -*- mode: emacs-lisp; lexical-binding: t -*-

;; flycheck

(use-package flycheck
  :custom
  (flycheck-checker-error-threshold 800)
  (flycheck-disabled-checkers '(python-mypy))
  )
(global-flycheck-mode)

;; (setq flycheck-checker-error-threshold 800)
;; (setq-default flycheck-disabled-checkers '(python-mypy))
