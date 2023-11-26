;; -*- mode: emacs-lisp; lexical-binding: t -*-

;; Setting `file-name-handler-alist' to nil should boost startup time.
;; https://github.com/abougouffa/minemacs
;; reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start
;; Store the current value so we can reset it after Emacs startup.
(put 'file-name-handler-alist 'original-value (default-toplevel-value 'file-name-handler-alist))
;; Make sure the new value survives any current let-binding.
(set-default-toplevel-value 'file-name-handler-alist nil)
;; After Emacs startup, we restore `file-name-handler-alist' while conserving
;; the potential edits made during startup.
(add-hook
 'emacs-startup-hook
 (defun +mineamcs--restore-file-name-handler-alist-h ()
   (setq file-name-handler-alist
         (delete-dups
          (append file-name-handler-alist
                  (get 'file-name-handler-alist 'original-value)))))
 101)

;; Set new user-emacs-directory
;; https://github.com/abougouffa/minemacs
;; Most core and third-party packages depends on the
;; `user-emacs-directory' variable to store some cache information and generated
;; configuration files. However, this will mess up the .emacs.d directory (which
;; defaults to `user-emacs-directory'). To keep the "~/.emacs.d/" directory
;; clean, we overwrite the `user-emacs-directory' with `you/` so
;; all generated files gets stored in "~/.emacs.d/you/".
;; Note, it is important to set this here and not in "early-init.el", otherwise,
;; it won't work with Chemacs2-based installations.
(setq user-emacs-directory (file-name-concat user-emacs-directory "you"))
;; Set also package-user-dir to the new user directory
(setq package-user-dir user-emacs-directory)

;; exec path
;; https://www.emacswiki.org/emacs/ExecPath
(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable
 to match that used by the user's shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$" "" (shell-command-to-string
                                          "$SHELL --login -c 'echo $PATH'"
                                          )))
        )
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))
    )
  )
(set-exec-path-from-shell-PATH)

;; Setup straight.el
;; https://github.com/radian-software/straight.el#getting-started
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
;; Effectively replace use-package with straight-use-package
;; https://github.com/raxod502/straight.el/blob/develop/README.md#integration-with-use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
;; so package-list-packages includes use-packages
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

;; Miscellaneous settings
(delete-selection-mode 1) ; if 0, typed text is just inserted regardless of any selection
(setq auto-save-default t
      make-backup-files t
      backup-by-copying-when-linked t  ; preserves links when saving
      vc-make-backup-files t)          ; also backup files under version control
(setq set-scroll-bar-mode 'right
      menu-bar-right-scroll-bar t)
;; (setq tab-always-indent t) ; if nil, tab opens helm suggestions
(setq blink-cursor-mode nil)
(setq column-number-mode t)
(setq evil-want-Y-yank-to-eol nil)
(setq safe-local-variable-values '((require-trailing-newline . t)))
(setq tool-bar-mode nil)
(setq transient-mark-mode t)
(put 'downcase-region 'disabled nil)
;; Set default coding system (especially for Windows)
(set-default-coding-systems 'utf-8)
;; ;; turn off beeps, make them flash!
;; (customize-set-variable 'visible-bell 1)
;; change to ~100 MB
(customize-set-variable 'large-file-warning-threshold 100000000)
(global-display-line-numbers-mode)
(savehist-mode t)
(setq fill-column 80)
;; ediff frames will be side-by-side
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-merge-split-window-function 'split-window-horizontally)

;; Frame
(add-to-list 'default-frame-alist '(width  . 90))
(add-to-list 'default-frame-alist '(height . 45))
(set-face-attribute 'default nil
                    :family "Source Code Pro"
                    :height 150
                    :weight 'normal)

;; prevent (old < 28?) error on startup
(add-to-list 'image-types 'svg)

;; Modes and mode-specific settings
(load "me-keybindings")
;; keyboard macros
(fset 'comment-on-line-above
      [?\C-k ?\C-p ?\C-e return ?\C-y tab ?\C-n ?\C-e])
(load "me-ivy")
(load "me-flycheck")
(load "me-flyspell")
(load "me-org")
(load "me-f90")
(load "me-python")
(load "me-rst")
(load "me-latex")
(load "me-git")
(load "me-parenthesis")

;; start Emacs daemon
(load "server")
(unless (server-running-p) (server-start))

;; after started up, reset GC threshold to normal.
;; https://github.com/angrybacon/dotemacs
(run-with-idle-timer 4 nil
                     (lambda ()
                       "Clean up gc."
                       (setq gc-cons-threshold 67108864) ; 64M
                       (setq gc-cons-percentage 0.1) ; original value
                       (garbage-collect)))

(provide 'init)
