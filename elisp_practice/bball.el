;;; bball.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Nick Silverman
;;
;; Author: Nick Silverman <nick.silverman11@gmail.com>
;; Maintainer: Nick Silverman <nick.silverman11@gmail.com>
;; Created: November 07, 2022
;; Modified: November 07, 2022
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/nick/bball
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

;; (defun yahoo-fantasy-bball-rankpergame ()
;;   (interactive)
;;   (insert (shell-command-to-string (format "python3 rankpergame.py"))))


(defun insert-puppies-output ()
  "Insert my command output into the buffer."
  (interactive)
  (insert-shell-command-output "echo 'puppies'"))

insert-puppies-output

(provide 'bball)
;;; bball.el ends here
