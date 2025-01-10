;; -*- mode: emacs-lisp; lexical-binding: t -*-

;; html

;; https://www.reddit.com/r/emacs/comments/1b7yhep/lets_surround_part_1/
(defun my-surround-region (prefix suffix)
  (when (region-active-p)
    (let ((end (+ (length prefix) (region-end))))
      (save-excursion
        (when suffix
          (goto-char (region-beginning))
          (insert prefix))
        (when prefix
          (goto-char end)
          (insert suffix))))))

(defun html-b ()
  "Surround the active region with hard-coded strings"
  (interactive)
  (my-surround-region "<b>" "</b>"))

(defun html-i ()
  "Surround the active region with hard-coded strings"
  (interactive)
  (my-surround-region "<i>" "</i>"))

(defun html-p ()
  "Surround the active region with hard-coded strings"
  (interactive)
  (my-surround-region "<p>" "</p>"))

(defun html-sub ()
  "Surround the active region with hard-coded strings"
  (interactive)
  (my-surround-region "<sub>" "</sub>"))

(defun html-sup ()
  "Surround the active region with hard-coded strings"
  (interactive)
  (my-surround-region "<sup>" "</sup>"))
