;;; package --- miscellaneous tools ;; -*- mode: emacs-lisp; lexical-binding: t -*-

;;; Code:


;; hydra

;; https://www.reddit.com/r/emacs/comments/8of6tx/tip_how_to_be_a_beast_with_hydra/
(use-package hydra
  :defer 2
  :config (hydra-add-font-lock))


;; kill ring

;; https://github.com/mwfogleman/.emacs.d/
(use-package browse-kill-ring
  :bind ("C-x C-y" . browse-kill-ring)
  :config
  (setq browse-kill-ring-quit-action 'kill-and-delete-window))

(setq save-interprogram-paste-before-kill t)


;; unfill

(use-package unfill)


(provide 'me-misc)

;;; me-misc.el ends here
