;; -*- mode: emacs-lisp; lexical-binding: t -*-

;; f90

;; ;; Regexp of comment-like directive like "!HPF\\$", not to be indented (default "!hpf\\$").
;; (setq f90-directive-comment-re "#")

;; Use f90-indent-line instead on f90-indent-region on a region of code
;; because they behave differently for continuation lines
(defun f90-indent-lines ()
  "Apply f90-indent-line on all lines of a region."
  (interactive)
  (save-excursion
    (message "This might take a while.")
    (goto-char (region-beginning))
    (while (< (point) (region-end))
      (f90-indent-line)
      (forward-line 1)))
  (delete-trailing-whitespace))
;; (add-hook 'f90-mode-hook 'f90-indent-lines)

;; ;; String inserted by function M-x f90-comment-region at start of each line in region (default "!!!$").
;; (setq f90-comment-region "! ")

;; ;; Regexp determining the type of comment to be intended like code (default "!").
;; (setq f90-indented-comment-re "!")

;; Coding styles for special projects
(defun f90-default-style ()
  (interactive)
  (setq f90-do-indent 3)
  (setq f90-if-indent 3)
  (setq f90-type-indent 3)
  (setq f90-program-indent 2)
  (setq f90-associate-indent 2)
  (setq f90-critical-indent 2)
  (setq f90-continuation-indent 5))
(defun f90-eddypro-style ()
  (interactive)
  (setq f90-do-indent 4)
  (setq f90-if-indent 4)
  (setq f90-type-indent 4)
  (setq f90-program-indent 4)
  (setq f90-associate-indent 4)
  (setq f90-critical-indent 4)
  (setq f90-continuation-indent 4))
