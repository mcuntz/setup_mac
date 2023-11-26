;; -*- mode: emacs-lisp; lexical-binding: t -*-

;; git

;; https://github.com/CSRaghunandan/.emacs.d/

(use-package magit
  :bind (("C-c m c" . magit-clone)
         :map magit-status-mode-map
         ("Q" . mu-magit-kill-buffers)
         (:map dired-mode-map
               ("l" . magit-dired-log)))
  :bind* (("C-c m s" . magit-status)))
