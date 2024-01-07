;; -*- mode: emacs-lisp; lexical-binding: t -*-

;; https://github.com/abougouffa/minemacs
;; and others
(setq
 inhibit-startup-message t
 ;; Do not make installed packages available when Emacs starts
 package-enable-at-startup nil
 ;; Increase the garbage collection (GC) threshold for faster startup.
 ;; Reset it in init.el later. Not resetting it later will cause stuttering/freezes.
 gc-cons-percentage 0.6
 gc-cons-threshold most-positive-fixnum
 ;; Prefer loading newer files
 load-prefer-newer t
 ;; Remove unneeded UI elements
 default-frame-alist '((tool-bar-lines . 0)
                       (menu-bar-lines . 0)
                       (mouse-color . "blue")
                       (left-fringe . 8)
                       (right-fringe . 13))
 ;; Explicitly set modes disabled in `default-frame-alist' to nil
 tool-bar-mode nil
 menu-bar-mode nil
 )

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
;; https://github.com/CSRaghunandan/.emacs.d/blob/master/early-init.el
(setq frame-inhibit-implied-resize t)

;; Loads theme, avoids the white screen flash on startup.
;; https://github.com/SystemCrafters/crafted-emacs
(load-theme 'manoj-dark t)

;; Native compilation settings
;; https://github.com/SystemCrafters/crafted-emacs
(when (featurep 'native-compile)
  ;; Silence compiler warnings as they can be pretty disruptive
  (setq native-comp-async-report-warnings-errors nil)
  ;; Make native compilation happens asynchronously
  (setq native-comp-deferred-compilation t)
  )

;; https://github.com/angrybacon/dotemacs
(setq
 ;; use-package is a macro. Don't let the macro expands into
 ;; codes with too much of irrelevant checks.
 use-package-expand-minimally t
 ;; This is a useful trick to speed up your startup time. Only
 ;; use `require' when it's necessary. By setting the
 ;; `use-package-always-defer' option to t, use-package won't
 ;; call `require' in all cases unless you explicitly include
 ;; :demand t'. This will prevent unnecessary package loading and
 ;; speed up Emacs startup time.
 use-package-always-defer t
 ;; ;; This is a useful trick to further optimize your startup
 ;; ;; time. Instead of using `straight-check-for-modifications' to
 ;; ;; check if a package has been modified, you can manually
 ;; ;; rebuild the package by `straight-rebuild-package' when you
 ;; ;; know its source code has changed. This avoids the overhead of
 ;; ;; the check. Make sure you know what you are doing here when
 ;; ;; setting this option.
 ;; straight-check-for-modifications nil ;;'(find-at-startup)
 ;;
 ;; debug-on-error t
 )

;; ;; Better titlebar on MacOS
;; ;; https://github.com/abougouffa/minemacs
;; ;; (defconst os/linux (eq system-type 'gnu/linux) "Non-nil on GNU/Linux systems.")
;; ;; (defconst os/bsd (and (memq system-type '(berkeley-unix gnu/kfreebsd)) t) "Non-nil on BSD systems.")
;; ;; (defconst os/win (and (memq system-type '(cygwin windows-nt ms-dos)) t) "Non-nil on Windows systems.")
;; (defconst os/mac (eq system-type 'darwin) "Non-nil on MacOS systems.")
;; (when (and os/mac (featurep 'ns))
;;   (push '(ns-transparent-titlebar . t) default-frame-alist))

;; add directory to load path
;; (push (file-name-concat user-emacs-directory "me") load-path)
(add-to-list 'load-path (file-name-concat user-emacs-directory "me"))

;; Make the initial buffer load faster by setting its mode to fundamental-mode
;; https://github.com/SystemCrafters/crafted-emacs
(customize-set-variable 'initial-major-mode 'fundamental-mode)

(provide 'early-init)
