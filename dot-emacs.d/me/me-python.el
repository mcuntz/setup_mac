;; -*- mode: emacs-lisp; lexical-binding: t -*-

;; python

;; (require 'python)
;; (setq python-shell-interpreter "ipython")
;; (setq python-shell-completion-native-disabled-interpreters '("ipython"))

(use-package pyenv-mode
  :init
  (add-to-list 'exec-path "~/.pyenv/shims")
  (setenv "WORKON_HOME" "~/.pyenv/versions/")
  :config
  (pyenv-mode))

(defvar pyenv-current-version nil nil)

(defun pyenv-init()
  "Initialize pyenv's current version to the global one."
  (let ((global-pyenv (replace-regexp-in-string "\n" "" (shell-command-to-string "pyenv global"))))
    (message (concat "Setting pyenv version to " global-pyenv))
    (pyenv-mode-set global-pyenv)
    (setq pyenv-current-version global-pyenv)))
(add-hook 'after-init-hook 'pyenv-init)
