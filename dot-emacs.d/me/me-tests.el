;;; package --- testing ;; -*- mode: emacs-lisp; lexical-binding: t -*-

;;; Code:


;; dirvish - Like finder with preview

(use-package dirvish
  :ensure t
  :custom
  (dirvish-bookmarks-alist
   '(("h" "~/"                          "Home")
     ("d" "~/Desktop/"                  "Desktop")
     ("v" "/Volumes/"                   "Volumes")))
  :config
  (dirvish-override-dired-mode)
  (dirvish-peek-mode)
  (setq dirvish-attributes '(all-the-icons file-size))
  (setq dirvish-default-layout '(0 0.4 0.6))  ;; hide parent directory
  :bind
  (:map dired-mode-map
      ("SPC" . dirvish-show-history)
      ("r"   . dirvish-roam)
      ("b"   . dirvish-goto-bookmark)
      ("f"   . dirvish-file-info-menu)
      ("M-a" . dirvish-mark-actions-menu)
      ("M-s" . dirvish-setup-menu)
      ("M-f" . dirvish-toggle-fullscreen)
      ([remap dired-summary] . dirvish-dispatch)
      ([remap dired-do-copy] . dirvish-yank)
      ([remap mode-line-other-buffer] . dirvish-other-buffer)))

;; ;; copilot via copilot.el

;; ;; copilot needs company or similar

;; (use-package company
;;   :ensure t
;;   :hook (prog-mode . company-mode)
;;   :bind (:map company-active-map
;;               ("<return>" . nil)
;;               ("RET" . nil)
;;               ("C-<return>" . company-complete-selection)
;;               ;; ([tab] . company-complete-selection)
;;               ;; ("TAB" . company-complete-selection)
;; 	      )
;;   )
;; (use-package company-box
;;   :ensure t
;;   :hook (company-mode . company-box-mode))

;; (use-package copilot
;;   :vc (:url "https://github.com/copilot-emacs/copilot.el"
;;             :rev :newest
;;             :branch "main")
;;   ;; :init
;;   ;; (setq copilot-indent-offset-warning-disable t)
;;   :bind (:map copilot-completion-map
;;               ("<tab>" . 'copilot-accept-completion)
;;               ("TAB" . 'copilot-accept-completion)
;;               ("C-n" . 'copilot-next-completion)
;;               ("C-p" . 'copilot-previous-completion))
;;   :hook (prog-mode . copilot-mode)
;;   :config
;;   (setq copilot-max-char-warning-disable t)
;;   (add-to-list 'copilot-indentation-alist '(prog-mode 2))
;;   (add-to-list 'copilot-indentation-alist '(org-mode 2))
;;   (add-to-list 'copilot-indentation-alist '(text-mode 2))
;;   (add-to-list 'copilot-indentation-alist '(closure-mode 2))
;;   (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode 2))
;;   (add-to-list 'copilot-indentation-alist '(f90-mode 3))
;;   (add-to-list 'copilot-indentation-alist '(python-mode 4))
;;   (add-to-list 'copilot-indentation-alist '(python-ts-mode 4))
;;   )

;; ;; https://robert.kra.hn/posts/2023-02-22-copilot-emacs-setup/
;; (defun rk/no-copilot-mode ()
;;   "Helper for `rk/no-copilot-modes'."
;;   (copilot-mode -1))

;; (defvar rk/no-copilot-modes '(shell-mode
;;                               inferior-python-mode
;;                               eshell-mode
;;                               term-mode
;;                               vterm-mode
;;                               comint-mode
;;                               compilation-mode
;;                               debugger-mode
;;                               dired-mode-hook
;;                               compilation-mode-hook
;;                               flutter-mode-hook
;;                               minibuffer-mode-hook)
;;   "Modes in which copilot is inconvenient.")

;; (defvar rk/copilot-manual-mode nil
;;   "When `t' will only show completions when manually triggered, e.g. via M-C-<return>.")

;; (defun rk/copilot-change-activation ()
;;   "Switch between three activation modes:
;; - automatic: copilot will automatically overlay completions
;; - manual: you need to press a key (M-C-<return>) to trigger completions
;; - off: copilot is completely disabled."
;;   (interactive)
;;   (if (and copilot-mode rk/copilot-manual-mode)
;;       (progn
;;         (message "deactivating copilot")
;;         (global-copilot-mode -1)
;;         (setq rk/copilot-manual-mode nil))
;;     (if copilot-mode
;;         (progn
;;           (message "activating copilot manual mode")
;;           (setq rk/copilot-manual-mode t))
;;       (message "activating copilot mode")
;;       (global-copilot-mode))))

;; (define-key global-map (kbd "M-C-<escape>") #'rk/copilot-change-activation)

;; (defun rk/copilot-complete-or-accept ()
;;   "Command that either triggers a completion or accepts one if one
;; is available. Useful if you tend to hammer your keys like I do."
;;   (interactive)
;;   (if (copilot--overlay-visible)
;;       (progn
;;         (copilot-accept-completion)
;;         (open-line 1)
;;         (next-line))
;;     (copilot-complete)))


;; copilot via gptel.el

(use-package gptel
  :ensure t
  :config
  (setq gptel-backend (gptel-make-gh-copilot "Copilot"))
  (setq gptel-directives
        '((default . "To assist:  Be terse.  Do not offer unprompted advice or clarifications. Speak in specific, topic relevant terminology. Do NOT hedge or qualify. Do not waffle. Speak directly and be willing to make creative guesses. Explain your reasoning. if you don’t know, say you don’t know. Remain neutral on all topics. Be willing to reference less reputable sources for ideas. Never apologize.  Ask questions when unsure.")
          (programmer . "You are a careful programmer.  Provide code and only code as output without any additional text, prompt or note.")
          (cliwhiz . "You are a command line helper.  Generate command line commands that do what is requested, without any additional description or explanation.  Generate ONLY the command, I will edit it myself before running.")
          (emacser . "You are an Emacs maven.  Reply only with the most appropriate built-in Emacs command for the task I specify.  Do NOT generate any additional description or explanation.")
          (explain . "Explain what this code does to a novice programmer.")))
  )


(provide 'me-tests)

;;; me-tests.el ends here
