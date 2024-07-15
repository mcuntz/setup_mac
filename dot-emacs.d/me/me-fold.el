;; -*- mode: emacs-lisp; lexical-binding: t -*-

;; origami / vimish fold

;; ;; origami
;; ;; https://github.com/CSRaghunandan/.emacs.d

;; (use-package origami
;;   :bind (("C-c f" . origami-recursively-toggle-node)
;; 	 ("C-c h f" . hydra-origami/body))
;;   :config
;;   (defhydra hydra-origami (:color red :hint nil)
;;     "
;;     _t_: toggle  _u_: undo  _p_: prev  _o_: open all
;;     _q_: quit    _r_: redo  _n_: next  _c_: close all
;;     "
;;     ("t" origami-recursively-toggle-node)
;;     ("q" nil "Quit" :color blue)
;;     ("u" origami-undo)
;;     ("r" origami-redo)
;;     ("p" origami-previous-fold)
;;     ("n" origami-next-fold)
;;     ("o" origami-open-all-nodes)
;;     ("c" origami-close-all-nodes))
;;   ;; (defhydra hydra-origami (:color red)
;;   ;;   "    Origami folding"
;;   ;;   ("t" origami-recursively-toggle-node :column "Toggle")
;;   ;;   ("q" nil "Quit" :color blue)
;;   ;;   ("o" origami-open-all-nodes :column "All")
;;   ;;   ("c" origami-close-all-nodes)
;;   ;;   ("u" origami-undo :column "Undo")
;;   ;;   ("r" origami-redo)
;;   ;;   ("p" origami-previous-fold :column "Next")
;;   ;;   ("n" origami-next-fold))
;;   (global-origami-mode))

;; ;; ;; lsp-origami provides support for origami.el using language server protocol’s
;; ;; ;; textDocument/foldingRange functionality.
;; ;; ;; https://github.com/emacs-lsp/lsp-origami/
;; ;; (use-package lsp-origami
;; ;;   :hook ((lsp-after-open . lsp-origami-mode)))

;; vimish fold
;; https://sriramkswamy.github.io/dotemacs/

;; (use-package origami
;;   :bind (("C-c f" . origami-recursively-toggle-node)
;; 	 ("C-c h f" . hydra-origami/body))
;;   :config
(use-package vimish-fold
  :bind (("C-c f" . vimish-fold-toggle)
 	 ("C-c h f" . hydra-vimish-fold/body))
  :config
  (defhydra hydra-vimish-fold (:color red :hint nil)
    "
    _f_: fold   _u_: unfold _r_: refold _q_: quit     _U_: unfold all _R_: refold all
    _t_: toggle _d_: delete _n_: next   _p_: previous _T_: toggle all _D_: delete all
    "
    ("f" vimish-fold)
    ("u" vimish-fold-unfold)
    ("r" vimish-fold-refold)
    ("q" nil :color blue)
    ("U" vimish-fold-unfold-all)
    ("R" vimish-fold-refold-all)
    ("t" vimish-fold-toggle)
    ("d" vimish-fold-delete)
    ("n" vimish-fold-next-fold)
    ("p" vimish-fold-previous-fold)
    ("T" vimish-fold-toggle-all)
    ("D" vimish-fold-delete-all))
  (vimish-fold-global-mode 1))
