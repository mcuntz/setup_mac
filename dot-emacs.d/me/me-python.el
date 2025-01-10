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

;; ;; https://www.reddit.com/r/emacs/comments/1em4ua3/emacs_for_python/
;; (require 'python)
;; (setq python-shell-interpreter "ipython")
;; (setq python-shell-interpreter-args "-i --simple-prompt")


;; https://stackoverflow.com/questions/24119221/customize-window-splitting-in-emacs-python-mode
;; If you want to customize behavior for python-mode buffers only, you can simply wrap the call to setq (last line of the code in the answer) in a function: (defun prefer-side-by-side-splits () ...) and add that function to python-mode-hook via (add-hook 'python-mode-hook 'prefer-side-by-side-splits)
;; https://stackoverflow.com/questions/23659909/reverse-evaluation-order-of-split-height-threshold-and-split-width-threshold-in
(defun my-split-window-sensibly (&optional window)
  (let ((window (or window (selected-window))))
    (or (and (window-splittable-p window t)
             ;; Split window horizontally.
             (with-selected-window window
               (split-window-right)))
	(and (window-splittable-p window)
             ;; Split window vertically.
             (with-selected-window window
               (split-window-below)))
	(and (eq window (frame-root-window (window-frame window)))
             (not (window-minibuffer-p window))
             ;; If WINDOW is the only window on its frame and is not the
             ;; minibuffer window, try to split it horizontally disregarding
             ;; the value of `split-width-threshold'.
             (let ((split-width-threshold 0))
               (when (window-splittable-p window t)
		 (with-selected-window window
                   (split-window-right))))))))

(setq split-window-preferred-function 'my-split-window-sensibly)
