;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

(setq user-full-name "Nick Silverman"
      user-mail-address "nick.silverman11@gmail.com")

;; Viz settings
;;; Fonts
(setq doom-font (font-spec :family "Fira Code Retina" :size 14))
(setq doom-variable-pitch-font (font-spec :family "Cantarell" :size 14 :weight 'regular))

;;; Theme
;; (setq doom-theme 'doom-one)
;; (setq doom-theme 'doom-dark+)
;; (setq doom-theme 'doom-nord)
;; (setq doom-theme 'doom-gruvbox)
;; (setq doom-theme 'tsdh-dark)
;; (setq doom-theme 'doom-challenger-deep)
(setq doom-theme 'modus-vivendi)

;;; Modeline stuff (see https://github.com/seagle0128/doom-modeline)
(setq doom-modeline-height 35)
(setq doom-modeline-bar-width 4)
(setq doom-modeline-window-width-limit fill-column)
(setq doom-modeline-icon (display-graphic-p))
(setq doom-modeline-major-mode-icon t)
(setq doom-modeline-major-mode-color-icon t)
(setq doom-modeline-buffer-state-icon t)
(setq doom-modeline-buffer-modification-icon t)
(setq doom-modeline-unicode-fallback nil)
(setq doom-modeline-minor-modes nil)
(setq doom-modeline-enable-word-count nil)
(setq doom-modeline-buffer-encoding t)
(setq doom-modeline-indent-info t)
(setq doom-modeline-checker-simple-format t)
(setq doom-modeline-number-limit 99)
(setq doom-modeline-vcs-max-length 12)
(setq doom-modeline-workspace-name t)
(setq doom-modeline-lsp t)
(setq doom-modeline-modal-icon t)

;; General preferences ==========================================
;;; Word wrap
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;;; Turn off line numbers
(setq display-line-numbers-type nil)

;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
(defun unfill-paragraph (&optional region)
"Takes a multi-line paragraph and makes it into a single line of text."
(interactive (progn (barf-if-buffer-read-only) '(t)))
(let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
(fill-paragraph nil region)))

;;; Handy key definition
(define-key global-map "\M-Q" 'unfill-paragraph)

;;; This is to use pdf-tools instead of doc-viewer
(use-package pdf-tools
  :config
  (pdf-tools-install)
  ;; This means that pdfs are fitted to width by default when you open them
  (setq-default pdf-view-display-size 'fit-width))

;; Vterm =======================================================
(use-package vterm
  :commands vterm
  :config
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")
  (setq vterm-shell "bash")
  (setq vterm-max-scrollback 10000))

;; ESS settings ================================================
(use-package ess-r-mode
  :bind
  (:map ess-r-mode-map
        ("_" . ess-insert-assign))
  (:map inferior-ess-r-mode-map
        ("_" . ess-insert-assign)))

;; Org-mode ====================================================
(setq org-directory "~/Dropbox/org/projects/")
(setq org-todo-file "~/Dropbox/org/projects/actions.org")

;;; Set TODO keywords
(setq org-todo-keywords
      '((sequence "TODO" "WAITING" "|" "DONE")))

;;; Allow ordered lists to include A. B. C. etc.
(setq org-list-allow-alphabetical t)

;;; Set archive file
(setq org-archive-location
      (concat org-directory
              "/archive/%s::datetree/"))

;;; Set org agenda files
(setq org-agenda-files (list org-directory))

;;; Include timestamp when closing a todo
(setq org-log-done 'time)

;;; Create list of common tags
(setq org-tag-alist '(("@errand" . ?E)
                      ("@tumbleleaf" . ?T)
                      ("@burlington" . ?B)
                      ("@home" . ?H)
                      ("@work" . ?W)
                      ("@ocean" . ?O)
                      ("@river" . ?R)
                      ("@mountain" . ?M)
                      ("paddling" . ?p)
                      ("hunting" . ?h)
                      ("fishing" . ?f)
                      ("surfing" . ?s)
                      ("cycling" . ?c)
                      ("climbing" . ?m)
                      ("running" . ?r)
                      ("hiking" . ?i)))

;;; Capture
(after! org
  (setq org-default-notes-file org-todo-file)
  (setq org-capture-templates
        '(("t" "TODO" entry
           (file+headline org-default-notes-file "Inbox")
           "* TODO [#B] %?\n %i\n %a"
           :kill-buffer t)

        ("n" "Note" entry
          (file+headline org-default-notes-file "Inbox")
          "* %?\n %i\n %a"
          :kill-buffer t))))

;;; Add strike through for DONE tasks
(setq org-fontify-done-headline t)
(custom-set-faces
 '(org-done ((t (:foreground "dim gray"
                 :weight normal
                 :strike-through t))))
 '(org-headline-done
            ((((class color) (min-colors 16) (background dark))
               (:foreground "dim gray" :strike-through t)))))

;;; Org-journal settings
(setq org-journal-dir "~/Dropbox/org/journal/"
      org-journal-file-type 'monthly
      org-journal-file-format "%m%Y.org"
      org-journal-date-prefix "* "
      org-journal-date-format "%A, %B %d %Y"
      org-journal-file-header "#+TITLE: %B %Y Journal\n\n")

;;; Org-roam
;;;; taken from jethro doom config:
;;;; https://github.com/jethrokuan/dots/blob/master/.doom.d/config.el
(use-package! org-roam
  :after org
  :init
  (setq org-roam-directory "~/Dropbox/org/notes/")
  :config
  (org-roam-db-autosync-mode +1)
  (set-popup-rules!
    `((,(regexp-quote org-roam-buffer) ; persistent org-roam buffer
       :side right :width .27 :height .5 :ttl nil :modeline nil :quit nil :slot 1)
      ("^\\*org-roam: " ; node dedicated org-roam buffer
       :side right :width .27 :height .5 :ttl nil :modeline nil :quit nil :slot 2)))
  (add-hook 'org-roam-mode-hook #'turn-on-visual-line-mode))

;;;; This changes the file name and template during note capture
(setq org-roam-capture-templates
      `(("d" "default" plain "%?"
         :target (file+head "${slug}.org"
                            "#+TITLE: ${title}\n")
         :unnarrowed t)
         ("r" "reference" plain "%?"
          :target (file+head "${citekey}.org"
                             ,(concat
                              "#+TITLE:  ${title}\n"
                              "#+AUTHOR: ${author-or-editor}\n"
                              "#+filetags: :Reference:\n\n"
                              "* ${title}\n"
                              "  :PROPERTIES:\n"
                              "  :Custom_ID: ${citekey}\n"
                              "  :URL: ${url}\n"
                              "  :NOTER_DOCUMENT: ${file}\n"
                              "  :NOTER_PAGE: \n"
                              "  :END:\n"))
          :unnarrowed t)
         )
      )
