;;; intro.el this is a practice file -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Nick Silverman
;;
;; Author: Nick Silverman <nick.silverman11@gmail.com>
;; Maintainer: Nick Silverman <nick.silverman11@gmail.com>
;; Created: November 06, 2022
;; Modified: November 06, 2022
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/nick/intro
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;; Commentary:
;;
;;
;;
;;; Code:

;; Options to evaluate elisp in Doom
;; c-x c-e if in typing mode
;; gr return if in editing mode
;; space m e e if in editing mode (last expression)
;; space m e d if in editing mode (function)

;; This is a test

;; Debugger example
(this is an unquoted list)
'(this is an quoted list)

;; Nested lists
(+ 3 (+ 2 3))

;; Use symbol in list
(+ fill-column 5)
(fill-column)

;; Concat list of strings
(concat "this is a " "test")

;; Subset a string
(substring "abcdef" 0 4)

;; Concat string and symbol
(concat "The "
        (number-to-string (+ 2 fill-column)) " red foxes.")

;; Many arguments to a function
(* 1 2 3)

;; Message function
(message "This is a message!")
(message "I %s understand elisp" "sort of")
(message (concat "The "
        (number-to-string (+ 2 fill-column)) " red %s.")
         "elephants")

;; Seting a value to a variable
(set 'flowers '(rose violet daisy buttercup))
flowers

(setq more-flowers '(sunflower iris tulip))
more-flowers

(setq trees '(oak redwood fir)
      herbivores '(chicken deer elk))
trees
herbivores

;; Counting
(setq counter 0)
(setq counter (+ counter 1))
counter











(provide 'intro)
;;; intro.el ends here
