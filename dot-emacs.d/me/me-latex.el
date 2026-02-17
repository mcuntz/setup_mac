;;; package --- LaTeX ;; -*- mode: emacs-lisp; lexical-binding: t -*-

;;; Code:

;; incremental narrowing framework
(add-hook 'LaTeX-mode-hook (lambda () (set-fill-column 99)))

(add-hook 'latex-mode #'flyspell-mode)

(setq latex-build-command "LatexMk")


(provide 'me-latex)

;;; me-latex.el ends here
