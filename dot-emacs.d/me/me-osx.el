;; -*- mode: emacs-lisp; lexical-binding: t -*-

;; macOS related

;; https://github.com/CSRaghunandan/.emacs.d

(defun is-mac-p ()
  (eq system-type 'darwin))

(when (is-mac-p)
  (setq ns-function-modifier 'hyper ; make Fn key do Hyper
        ns-use-thin-smoothing t
        ;; sane trackpad/mouse scroll settings
        mac-redisplay-dont-reset-vscroll t
        mac-mouse-wheel-smooth-scroll nil
        mouse-wheel-scroll-amount '(5 ((shift) . 2))  ; one line at a time
        mouse-wheel-progressive-speed nil             ; don't accelerate scrolling
        ;; Curse Lion and its sudden but inevitable fullscreen mode!
        ;; NOTE Meaningless to railwaycat's emacs-mac build
        ns-use-native-fullscreen nil
        ;; Don't open files from the workspace in a new frame
        ns-pop-up-frames nil)
  ;; Use the macOS Emoji font for Emoticons.
  (when (fboundp 'set-fontset-font)
    (set-fontset-font "fontset-default"
                      '(#x1F600 . #x1F64F)
                      (font-spec :name "Apple Color Emoji") nil 'prepend)))

;; exec-path-from-shell: ensure environment variables inside Emacs look the same
;; as in the users shell
;; https://github.com/purcell/exec-path-from-shell
(use-package exec-path-from-shell
  :if (is-mac-p))

(cond ((display-graphic-p)
       ;; A known problem with GUI Emacs on MacOS: it runs in an isolated
       ;; environment, so envvars will be wrong. That includes the PATH
       ;; Emacs picks up. `exec-path-from-shell' fixes this. This is slow
       ;; and benefits greatly from compilation.
       (setq exec-path
             (or (eval-when-compile
                   (when (require 'exec-path-from-shell nil t)
                     (setq exec-path-from-shell-check-startup-files nil)
                     (nconc exec-path-from-shell-variables '("GOPATH" "GOROOT" "PYTHONPATH"))
                     (exec-path-from-shell-initialize)
                     exec-path))
                 exec-path)))
      (t
       (when (require 'osx-clipboard nil t)
         (osx-clipboard-mode +1))))
