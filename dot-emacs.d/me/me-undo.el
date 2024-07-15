;; -*- mode: emacs-lisp; lexical-binding: t -*-

;; undo-tree

;; (use-package undo-tree
;;   :bind (("s-z" . undo-tree-undo)
;;          ("H-z" . undo-tree-undo)
;; 	 ("s-Z" . undo-tree-redo)
;;          ("H-Z" . undo-tree-redo))
;;   :config (global-undo-tree-mode))

(defun is-mac-p
    ()
  (eq system-type 'darwin))

(use-package undo-tree
  :defer t
  :init
  (setq undo-tree-visualizer-timestamps t
        undo-tree-visualizer-diff t
        ;; See `vim-style-enable-undo-region'.
        undo-tree-enable-undo-in-region t
        ;; 10X bump of the undo limits to avoid issues with premature
        ;; Emacs GC which truncages the undo history very aggresively
        undo-limit 800000
        undo-strong-limit 12000000
        undo-outer-limit 120000000
        undo-tree-history-directory-alist
        `(("." . ,(let ((dir (expand-file-name "undo-tree-history" user-cache-directory)))
                    (if (file-exists-p dir)
                        (unless (file-accessible-directory-p dir)
                          (warn "Cannot access directory `%s'.
Perhaps you don't have required permissions, or it's not a directory.
See variable `undo-tree-history-directory-alist'." dir))
                      (make-directory dir t))
                    dir))))
  :bind (("s-z" . undo-tree-undo)
         ("H-z" . undo-tree-undo)
	 ("s-Z" . undo-tree-redo)
         ("H-Z" . undo-tree-redo)))
(when (is-mac-p)
  (global-undo-tree-mode))
