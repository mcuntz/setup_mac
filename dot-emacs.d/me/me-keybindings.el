;;; package --- key bindings ;; -*- mode: emacs-lisp; lexical-binding: t -*-

;;; Code:

(defun is-linux-p ()
  (eq system-type 'gnu/linux))

;; key bindings C- Ctrl, M- option, S- shift, s- command

;; miscellaneous
;; (global-set-key [f1]       'advertised-undo)
(global-set-key (kbd "<f2>")    'query-replace-regexp)
(global-set-key (kbd "<f3>")    'comment-or-uncomment-region)
(global-set-key (kbd "<f12>")   'kill-current-buffer)
(global-set-key (kbd "C-x C-g") 'goto-line)
(global-set-key (kbd "C-h")     'delete-backward-char)
(global-set-key (kbd "C-x C-b") 'switch-to-buffer)
(global-set-key (kbd "M-#")     'dictionary-lookup-definition)

;; Cmd-q does not close Emacs, only C-x C-c
(global-set-key (kbd "H-q") nil) ; Cmd-q
(global-set-key (kbd "s-q") nil) ; Cmd-q

;; arrow keys, etc.
(global-set-key (kbd "H-<up>")      'beginning-of-buffer)    ; Cmd-up
(global-set-key (kbd "s-<up>")      'beginning-of-buffer)    ; Cmd-up
(global-set-key (kbd "H-<down>")    'end-of-buffer)          ; Cmd-down
(global-set-key (kbd "s-<down>")    'end-of-buffer)          ; Cmd-down
(global-set-key (kbd "H-<left>")    'beginning-of-line)      ; Cmd-left
(global-set-key (kbd "s-<left>")    'beginning-of-line)      ; Cmd-left
(global-set-key (kbd "H-<right>")   'end-of-line)            ; Cmd-right
(global-set-key (kbd "s-<right>")   'end-of-line)            ; Cmd-right

;; folding
(global-set-key (kbd "C-c f") 'hs-toggle-hiding)

;; quarto
(global-set-key (kbd "C-c c") 'polymode-eval-region-or-chunk)

(when (is-linux-p)
  (global-set-key (kbd "C-v") 'yank)
  ;; Use alt instead of Apple key
  (global-set-key (kbd "M-c") 'kill-ring-save)
  (global-set-key (kbd "M-v") 'yank)
  (global-set-key (kbd "M-a") 'mark-whole-buffer)
  (global-set-key (kbd "M-z") 'undo)
  (global-set-key (kbd "M-<up>")   'beginning-of-buffer)
  (global-set-key (kbd "M-<down>") 'end-of-buffer)
  )

;; Main use is to have my key bindings have the highest priority
;; https://github.com/kaushalmodi/.emacs.d/blob/master/elisp/modi-mode.el
(defvar my-mode-map (make-sparse-keymap)
  "Keymap for `my-mode'.")

;; autoload
(define-minor-mode my-mode
  "A minor mode so that my key settings override annoying major modes."
  ;; If init-value is not set to t, this mode does not get enabled in
  ;; `fundamental-mode' buffers even after doing \"(global-my-mode 1)\".
  ;; More info: http://emacs.stackexchange.com/q/16693/115
  :init-value t
  :lighter " my-mode"
  :keymap my-mode-map)

;; autoload
(define-globalized-minor-mode global-my-mode my-mode my-mode)

;; https://github.com/jwiegley/use-package/blob/master/bind-key.el
;; The keymaps in `emulation-mode-map-alists' take precedence over
;; `minor-mode-map-alist'
(add-to-list 'emulation-mode-map-alists `((my-mode . ,my-mode-map)))

;; Turn off the minor mode in the minibuffer
(defun turn-off-my-mode ()
  "Turn off my-mode."
  (my-mode -1))
(add-hook 'minibuffer-setup-hook #'turn-off-my-mode)

(provide 'my-mode)

;; were overwritten in markdown-mode
(define-key my-mode-map (kbd "C-c <right>") #'next-window-any-frame)
(define-key my-mode-map (kbd "C-c <left>") #'previous-window-any-frame)

(provide 'me-keybindings)

;;; me-keybindings.el ends here
