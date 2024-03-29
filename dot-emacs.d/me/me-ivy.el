;; -*- mode: emacs-lisp; lexical-binding: t -*-

;; ivy

;; https://github.com/CSRaghunandan/.emacs.d

;; Alternative M-x interface
(use-package amx :defer 0.5
  :config (amx-mode))

;; isearch with an overview!
;; `M-p' -> previous search item
;; `M-n' -> next search item
;; `M-n' -> to select the symbol at point in swiper
(use-package swiper
  ;; :bind (("C-s" . swiper-isearch)
  ;;        ("M-s ." . swiper-isearch-thing-at-point))
  :bind (("C-s" . swiper)
         ("M-s ." . swiper-thing-at-point))
  :config
  (setq swiper-action-recenter t
        ;; Jump to the beginning of match when leaving Swiper
        swiper-goto-start-of-match t))

;; incremental narrowing framework
(use-package ivy
  :bind (("C-c u" . ivy-resume))
  :config
  (ivy-mode)

  (setq ivy-use-virtual-buffers t
        ivy-height 13
        ivy-count-format "%d/%d "
        ivy-virtual-abbreviate 'full ; Show the full virtual file paths
        ivy-extra-directories nil ; default value: ("../" "./")
        ivy-wrap t
        ivy-action-wrap t
        ivy-use-selectable-prompt t)

  ;; modify default search behaviour of ivy
  (setq ivy-re-builders-alist
        '((t . ivy--regex-plus)))

  (bind-keys
   :map ivy-occur-grep-mode-map
   ("n" . ivy-occur-next-line)
   ("p" . ivy-occur-previous-line)
   ("b" . backward-char)
   ("f" . forward-char)
   ("v" . ivy-occur-press) ; default f
   ("RET" . ivy-occur-press))

  (define-key ivy-minibuffer-map (kbd "C-<return>") 'ivy-immediate-done)) ; Ctrl-return

;; https://gitlab.com/dwt1/dotfiles/-/blob/37489c2a4dd8fb17e4e2b1a02539ed47a483c6e6/.emacs.d.gnu/config.org
;; The following line removes the annoying ‘^’ in things like
;; counsel-M-x and other ivy/counsel prompts.  The default ‘^’ string
;; means that if you type something immediately after this string only
;; completion candidates that begin with what you typed are shown.
;; Most of the time, I’m searching for a command without knowing what
;; it begins with though.
(setq ivy-initial-inputs-alist nil)

;; Better experience with icons for ivy
(use-package all-the-icons-ivy-rich :defer 1
  :config
  (all-the-icons-ivy-rich-mode 1)
  (setq all-the-icons-ivy-rich-icon-size 0.8))

;; More friendly interface for ivy
(use-package ivy-rich
  :hook (counsel-mode . ivy-rich-mode)
  :config
  ;; For better performance
  ;; Better experience with icons
  (setq ivy-rich-parse-remote-buffer nil))

;; hydra for ivy, its a part of the same repo as swiper
(use-package ivy-hydra)
