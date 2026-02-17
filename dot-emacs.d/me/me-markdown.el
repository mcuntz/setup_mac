;;; package --- markdown ;; -*- mode: emacs-lisp; lexical-binding: t -*-

;;; Code:

(defun is-mac-p ()
  (eq system-type 'darwin))

(defun is-linux-p ()
  (eq system-type 'gnu/linux))


;; quarto

(use-package quarto-mode)

;; https://emacs.stackexchange.com/questions/74478/evaluate-single-python-code-blocks-in-a-quarto-file-like-in-r-studio-or-jupyter
(defun poly-python-eval-region (beg end msg)
  (python-shell-send-region beg end nil msg t))
(defun poly-python-mode-setup ()
  (setq-local polymode-eval-region-function #'poly-python-eval-region))
(add-hook 'python-mode-hook #'poly-python-mode-setup)


;; typst

;; After installing, execute command `M-x typst-ts-mc-install-grammar`
(use-package typst-ts-mode
  :custom
  (typst-ts-watch-options "--open")
  (typst-ts-mode-grammar-location (expand-file-name "tree-sitter/libtree-sitter-typst.dylib" user-emacs-directory))
  (typst-ts-mode-enable-raw-blocks-highlight t)
  :config
  (keymap-set typst-ts-mode-map "C-c C-c" #'typst-ts-tmenu))


;; org

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


;; reStructuredText

(eval-after-load "rst"
  '(progn
     (setq make-backup-files t)
     (add-to-list 'auto-mode-alist '("\\.[rR]" . R-mode))
     )
  )

(use-package sphinx-mode)
(add-hook 'rst-mode #'sphinx-mode)


(provide 'me-markdown)

;;; me-markdown.el ends here
