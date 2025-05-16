;; -*- mode: emacs-lisp; lexical-binding: t -*-

;; org

(defun is-mac-p
    ()
  (eq system-type 'darwin))

(defun is-linux-p
    ()
  (eq system-type 'gnu/linux))

(when (or (is-mac-p) (is-linux-p))
  (straight-use-package 'toc-org))

(eval-after-load "org"
  '(progn
     (setq org-support-shift-select t)
     (add-hook 'org-mode-hook 'turn-on-visual-line-mode)
     (add-hook 'org-mode-hook 'toc-org-mode)
     (define-key org-mode-map (kbd "<M-right>") 'right-word)
     (define-key org-mode-map (kbd "<M-left>") 'left-word)
     (define-key org-mode-map (kbd "<M-S-right>") nil)
     (define-key org-mode-map (kbd "<M-S-left>") nil)))
