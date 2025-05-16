;; -*- mode: emacs-lisp; lexical-binding: t -*-

;; flyspell


(setq ispell-program-name
      (locate-file "aspell" '("/usr/local/bin" "/opt/homebrew/bin")))

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; right-click on word to correct spelling
;; (setq mac-emulate-three-button-mouse t)
;; (define-key key-translation-map (kbd "<C-down-mouse-1>") (kbd "<down-mouse-2>"))
;; (define-key key-translation-map (kbd "<M-mouse-1>") (kbd "<mouse-2>"))
(eval-after-load "flyspell"
  '(progn
     (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
     (define-key flyspell-mouse-map [mouse-3] #'undefined)))
