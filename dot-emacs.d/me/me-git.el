;;; package --- git ;; -*- mode: emacs-lisp; lexical-binding: t -*-

;;; Code:

;; https://github.com/CSRaghunandan/.emacs.d/

(use-package magit
  :bind (("C-c m c" . magit-clone)
         :map magit-status-mode-map
         ("Q" . mu-magit-kill-buffers)
         (:map dired-mode-map
               ("l" . magit-dired-log)))
  :bind* (("C-c m s" . magit-status)))

(provide 'me-git)

;;; me-git.el ends here
