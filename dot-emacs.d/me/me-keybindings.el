;; -*- mode: emacs-lisp; lexical-binding: t -*-

;; key bindings

;; (when (is-mac-p)
;;   (setq mac-command-modifier 'meta
;;         mac-option-modifier 'super
;;         mac-control-modifier 'control
;;         ns-function-modifier 'hyper))

;; key bindings C- Ctrl, M- option, S- shift, s- command
;; (global-set-key [f1]       'advertised-undo)
(global-set-key (kbd "<f2>")      'query-replace-regexp)
(global-set-key (kbd "<f3>")      'comment-or-uncomment-region)
(global-set-key (kbd "<f12>")     'kill-this-buffer)
(global-set-key (kbd "C-x C-g")   'goto-line)
(global-set-key (kbd "C-h")       'delete-backward-char)
(global-set-key (kbd "C-x C-b")   'switch-to-buffer)
(global-set-key (kbd "H-q")       nil) ; Cmd-q
(global-set-key (kbd "s-q")       nil) ; Cmd-q
(global-set-key (kbd "H-<up>")    'beginning-of-buffer)    ; Cmd-up
(global-set-key (kbd "s-<up>")    'beginning-of-buffer)    ; Cmd-up
(global-set-key (kbd "H-<down>")  'end-of-buffer)          ; Cmd-down
(global-set-key (kbd "s-<down>")  'end-of-buffer)          ; Cmd-down
(global-set-key (kbd "H-<left>")  'beginning-of-line)      ; Cmd-left
(global-set-key (kbd "s-<left>")  'beginning-of-line)      ; Cmd-left
(global-set-key (kbd "H-<right>") 'end-of-line)            ; Cmd-right
(global-set-key (kbd "s-<right>") 'end-of-line)            ; Cmd-right
(global-set-key (kbd "M-#")       'dictionary-lookup-definition)
(global-set-key (kbd "C-c <right>") 'next-window-any-frame)
(global-set-key (kbd "C-c <left>") 'previous-window-any-frame)
