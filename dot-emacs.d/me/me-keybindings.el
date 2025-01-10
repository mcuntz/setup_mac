;; -*- mode: emacs-lisp; lexical-binding: t -*-

;; key bindings

;; (when (is-mac-p)
;;   (setq mac-command-modifier 'meta
;;         mac-option-modifier 'super
;;         mac-control-modifier 'control
;;         ns-function-modifier 'hyper))

;; key bindings C- Ctrl, M- option, S- shift, s- command

;; miscellaneous
;; (global-set-key [f1]       'advertised-undo)
(global-set-key (kbd "<f2>")    'query-replace-regexp)
(global-set-key (kbd "<f3>")    'comment-or-uncomment-region)
(global-set-key (kbd "<f12>")   'kill-this-buffer)
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
(global-set-key (kbd "C-c e") 'polymode-eval-chunk)

;; https://emacs.stackexchange.com/questions/352/how-to-override-major-mode-bindings

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


;; DID NOT WORK
;; ;; drag-and-drop
;; ;; https://stackoverflow.com/questions/3805658/how-to-configure-emacs-drag-and-drop-to-open-instead-of-append-on-osx
;; ;; (if (fboundp 'ns-find-file)
;; ;;     (global-set-key [ns-drag-file] 'ns-find-file))
;; ;; (setq ns-pop-up-frames nil)
;; ;; or
;; ;; Find the file, w/shift insert filename; w/meta insert file contents.
;; ;; Note that the emacs window must be selected (CMD-TAB) for the modifiers
;; ;; to register.
;; (define-key global-map [M-ns-drag-file] 'ns-insert-file)
;; (define-key global-map [S-ns-drag-file] 'ns-insert-filename)
;; (define-key global-map [ns-drag-file] 'ns-find-file-in-frame)
;; (defun ns-insert-filename ()
;;   "Insert contents of first element of `ns-input-file' at point."
;;   (interactive)
;;   (let ((f (pop ns-input-file)))
;;     (insert f))
;;   (if ns-input-file   ; any more? Separate by " "
;;       (insert " ")))
;; (defun ns-find-file-in-frame ()
;;   "Do a `find-file' with the `ns-input-file' as argument; staying in frame."
;;   (interactive)
;;   (let ((ns-pop-up-frames nil))
;;     (ns-find-file)))
