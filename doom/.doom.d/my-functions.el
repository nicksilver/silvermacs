;;; $DOOMDIR/my-functions.el -*- lexical-binding: t; -*-

;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
(defun my/unfill-paragraph (&optional region)
"Takes a multi-line paragraph and makes it into a single line of text."
(interactive (progn (barf-if-buffer-read-only) '(t)))
(let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
(fill-paragraph nil region)))
;;; Handy key definition
(define-key global-map "\M-Q" 'my/unfill-paragraph)

;;; Automatically moves DONE task to bottom of the subtree
(defun my/org-sort-on-done ()
  "Sort the subtree whenever a task is marked as done."
    (save-excursion
      (when (string= (org-get-todo-state) "DONE")
      (org-up-heading-safe)
      (org-sort-entries nil ?o)
      (org-cycle) ; this (and the next one) cycles the heading once to remove weird formatting
      (org-cycle))))
(add-hook 'org-after-todo-state-change-hook 'my/org-sort-on-done)
